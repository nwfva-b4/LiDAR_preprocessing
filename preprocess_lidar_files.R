#' Preprocess LiDAR files
#'
#' This function preprocesses LiDAR point cloud files. It first renames the input files according to a specified pattern,
#' then optionally performs ground classification using LAStools, followed by normalization, filtering, and optionally rasterization. 
#' Finally, it renames parts of the output files.
#' Detailed information regarding the preprocessing steps (excluding renaming):
#' - Optionally, point clouds are classified into ground and non-ground points using LAStools lasground_new.exe with nature setting.
#' - Point clouds are normalized by triangulating the ground points and linearly interpolating the elevation of each point within each triangle.
#' - A first filter drop points below and above desired height values.
#' - Noise points are classified using Isolated Voxel Filter (IVF). Identifies points that have only a few other points in their surrounding.
#' - A second filter drop the points classified as noise.
#' - Optionally, a canopy height model (CHM) is calculated from the normalized and filtered point clouds using point-to-raster method.
#' - If CHM is generated, pits and spikes are filled in the CHM.
#' The lasR package is used for normalization, filtering and CHM generation. LAStools is used for ground classification.
#' See https://github.com/r-lidar/lasR and https://rapidlasso.de/lastools/ for further information.
#'
#' @param input_dir character. Path to the directory containing the input LAZ files.
#' @param example_pattern character.  User-defined pattern of the input LAZ file names with placeholders. The file names must include three digits for the x-coordinate and four digits for the y-coordinate.
#' @param state character. Code representing the federal state (e.g., 'ni' for Lower Saxony).
#' @param rs_system character. The remote sensing system or platform used (e.g., 'flugzeug' for aircraft).
#' @param year character. The year of data acquisition.
#' @param client character. The client or organization for whom we have the LiDAR data (e.g., 'nlf' for Niedersaechsiche Landesforsten).
#' @param output_dir_laz character. Path to the directory where the processed LAZ files should be saved.
#' @param output_dir_tif character. Path to the directory where the rasterized LAZ files (then TIF files) should be saved. Only required if generate_chm is TRUE.
#' @param drop_z_below numeric. The lower threshold for dropping points in the normalized point cloud.
#' @param drop_z_above numeric. The upper threshold for dropping points in the normalized point cloud.
#' @param res_voxels numeric. Resolution of the voxels for classifying noise points using IVF.
#' @param n_points integer. The maximal number of 'other points' in the voxels for IVF.
#' @param classify_ground logical. Whether to perform ground classification using LAStools. Default is FALSE.
#' @param lastools_path character. Path to the LAStools bin directory containing lasground_new.exe or lasground_new64.exe. Only required if classify_ground is TRUE.
#' @param generate_chm logical. Whether to generate a canopy height model (CHM). Default is TRUE.
#' @param res_chm numeric. The target resolution of the CHM raster. Only required if generate_chm is TRUE.
#' @param n_cores integer. The number of cores to use for parallel processing in both LAStools and lasR (not use all!!!). Default uses half of the available cores.
#'
#' @examples
#' # Preprocess LiDAR files with CHM generation and ground classification
#' preprocess_lidar_files(
#'   input_dir = '/path/to/raw_lidar_data',
#'   example_pattern = 'tile_XXX_YYYY_survey_date.laz',
#'   state = 'state_code',
#'   rs_system = 'platform_type',
#'   year = '2024',
#'   client = 'client_code',
#'   output_dir_laz = '/path/to/processed_laz_output',
#'   output_dir_tif = '/path/to/chm_raster_output',
#'   drop_z_below = -1,
#'   drop_z_above = 55,
#'   res_voxels = 5,
#'   n_points = 6L,
#'   classify_ground = TRUE,
#'   lastools_path = '/path/to/LAStools/bin',
#'   generate_chm = TRUE,
#'   res_chm = 0.5,
#'   n_cores = 25
#' )
#' 
#' # Preprocess LiDAR files without CHM generation and without ground classification
#' preprocess_lidar_files(
#'   input_dir = '/path/to/raw_lidar_data',
#'   example_pattern = 'tile_XXX_YYYY_survey_date.laz',
#'   state = 'state_code',
#'   rs_system = 'platform_type',
#'   year = '2024',
#'   client = 'client_code',
#'   output_dir_laz = '/path/to/processed_laz_output',
#'   drop_z_below = -1,
#'   drop_z_above = 55,
#'   res_voxels = 5,
#'   n_points = 6L,
#'   classify_ground = FALSE,
#'   generate_chm = FALSE,
#'   n_cores = lasR::half_cores()) 
#' )

# ensure required packages are installed and loaded
required_packages <- c('stringr', 'lasR')

for (pkg in required_packages) {
  if (!require(pkg, character.only = T)) {
    if (pkg == 'lasR') {
      install.packages('lasR', repos = 'https://r-lidar.r-universe.dev')
    } else {
      install.packages(pkg)
    }
    library(pkg, character.only = T)
  }
}

preprocess_lidar_files <- function(
    input_dir,
    example_pattern,
    state,
    rs_system,
    year,
    client,
    output_dir_laz,
    output_dir_tif = NULL,
    drop_z_below,
    drop_z_above,
    res_voxels,
    n_points,
    classify_ground = FALSE,
    lastools_path = NULL,
    generate_chm = TRUE,
    res_chm = NULL,
    n_cores = lasR::half_cores()) 
{
  
  # validate parameters
  if (generate_chm && (is.null(output_dir_tif) || is.null(res_chm))) {
    stop('output_dir_tif and res_chm must be provided when generate_chm is TRUE')
  }
  
  if (classify_ground && is.null(lastools_path)) {
    stop('lastools_path must be provided when classify_ground is TRUE')
  }
  
  start.time <- Sys.time()
  
  #----------------------------
  # step 1: rename input files
  #----------------------------
  
  # get all LAZ files in the input directory
  laz_files <- list.files(path = input_dir, pattern = '*.laz$', full.names = T)
  
  # check if the files are already in the desired name format
  desired_pattern <- paste0(state, '_', rs_system, '_', year, '_laz_', client, '_\\d{6}_\\d{7}\\.laz')
  files_already_renamed <- all(grepl(desired_pattern, basename(laz_files)))
  
  if (!files_already_renamed) {
    
    # function to rename files according to the desired format
    rename_files <- function(old_file_name, pattern) {
      
      # extract file name
      old_name <- basename(old_file_name)
      
      # convert user pattern into a regex pattern by replacing XXX and YYYY
      regex_pattern <- gsub('XXX', '(\\\\d{3})', gsub('YYYY', '(\\\\d{4})', pattern))
      
      # match the pattern and extract the match
      match <- regexec(regex_pattern, old_name)
      match_result <- regmatches(old_name, match)
      
      # if no match is found, exit without printing
      if (length(match_result[[1]]) == 0) {
        return(NULL)
      }
      
      # extract the coordinates from the match result
      x_coord <- match_result[[1]][2]  # the second element is the matched XXX
      y_coord <- match_result[[1]][3]  # the third element is the matched YYYY
      
      # format coordinates
      x_coord_formatted <- sprintf('%06d', as.numeric(x_coord) * 1000)
      y_coord_formatted <- sprintf('%07d', as.numeric(y_coord) * 1000)
      
      # construct new file name using predefined components (state, rs_system, etc.)
      new_name <- paste0(state, '_', rs_system, '_', year, '_', 'laz', '_', client, '_', x_coord_formatted, '_', y_coord_formatted, '.laz')
      new_file_name <- file.path(input_dir, new_name)
      
      # rename the file and print successful renaming
      if (file.rename(file.path(input_dir, old_name), new_file_name)) {
        cat('file \'', old_name, '\' renamed to \'', new_name, '\'\n', sep = '')
      }
    }
    
    # apply the renaming function to all files
    invisible(lapply(laz_files, function(file) rename_files(file, example_pattern)))
    
  } else {
    
    cat('Files are already in the desired format. Skipping renaming step.\n')
    
  }
  
  #---------------------------------------
  # step 2: optional ground classification
  #---------------------------------------
  
  processing_dir <- input_dir  # directory to use for further processing
  
  if (classify_ground) {
    
    cat('Starting ground classification using LAStools...\n')
    
    # create temporary directory for ground-classified files
    temp_ground_dir <- file.path(dirname(input_dir), paste0(basename(input_dir), '_ground_classified_temp'))
    if (!dir.exists(temp_ground_dir)) {
      dir.create(temp_ground_dir, recursive = T)
    }
    
    # determine which lasground executable to use
    lasground_exe <- file.path(lastools_path, 'lasground_new64.exe')
    if (!file.exists(lasground_exe)) {
      lasground_exe <- file.path(lastools_path, 'lasground_new.exe')
      if (!file.exists(lasground_exe)) {
        stop('Neither lasground_new64.exe nor lasground_new.exe found in the specified lastools_path: ', lastools_path)
      }
    }
    
    cat('Using LAStools executable:', lasground_exe, '\n')
    cat('Input directory:', input_dir, '\n')
    cat('Output directory:', temp_ground_dir, '\n')
    
    # construct LAStools command
    input_pattern <- file.path(input_dir, '*.laz')
    cmd_parts <- c(
      paste0('"', lasground_exe, '"'),
      '-i', paste0('"', input_pattern, '"'),
      '-odir', paste0('"', temp_ground_dir, '"'),
      '-olaz',
      '-nature',
      '-cores', n_cores
    )
    
    # combine command parts
    lastools_cmd <- paste(cmd_parts, collapse = ' ')
    
    cat('Running LAStools command:\n', lastools_cmd, '\n')
    
    # execute LAStools command
    result <- system(lastools_cmd, wait = T)
    
    if (result != 0) {
      stop('LAStools ground classification failed with exit code: ', result)
    }
    
    cat('Ground classification completed successfully.\n')
    
    # update processing directory to use ground-classified files
    processing_dir <- temp_ground_dir
    
  }
  
  #-------------------------------------------------
  # step 3: normalization, filtering, rasterization
  #-------------------------------------------------
  
  # normalization
  normalization <- lasR::normalize()
  
  # first filter: drop points below and above a specified threshold
  first_filter <- lasR::delete_points(
    filter = lasR::drop_z_below(drop_z_below) + lasR::drop_z_above(drop_z_above)
  )
  
  # classify noise
  noise_classification <- lasR::classify_with_ivf(
    res = res_voxels, n = n_points, class = 18L
  )
  
  # second filter: drop points classified as noise
  second_filter <- lasR::delete_points(
    filter = lasR::drop_noise()
  )
  
  # write normalized and filtered point clouds to disk
  write_laz <- lasR::write_las(
    ofile = file.path(output_dir_laz, '/*.laz')
  )
  
  # initialize pipeline with core components
  pipeline <- 
    normalization +
    first_filter +
    noise_classification +
    second_filter +
    write_laz
  
  # conditionally add CHM generation to pipeline
  if (generate_chm) {
    # calculate CHM
    chm_generation <- lasR::chm(res = res_chm)
    
    # fill pits and spikes
    pitfill <- lasR::pit_fill(
      raster = chm_generation, ofile = file.path(output_dir_tif, '/*.tif')
    )
    
    # add CHM components to pipeline
    pipeline <- pipeline + chm_generation + pitfill
  }
  
  lasR::set_parallel_strategy(lasR::concurrent_files(ncores = n_cores))
  lasR::exec(pipeline, on = processing_dir, progress = T)
  
  #-----------------------------------------
  # step 4: cleanup and rename output files
  #-----------------------------------------
  
  # cleanup temporary ground classification directory if it was created
  if (classify_ground && dir.exists(temp_ground_dir)) {
    cat('Cleaning up temporary ground classification files...\n')
    unlink(temp_ground_dir, recursive = T)
  }
  
  # list all laz files
  output_laz_files <- list.files(path = output_dir_laz, pattern = '*.laz$', full.names = T)
  
  # function to rename laz files
  rename_laz_files <- function(file) {
    old_name <- basename(file)
    new_name <- stringr::str_replace(old_name, 'laz', 'laznorm')
    new_file <- file.path(output_dir_laz, new_name)
    file.rename(file, new_file)
    cat('output file \'', old_name, '\' renamed to \'', new_name, '\'\n', sep = '')
  }
  
  # apply renaming function to all laz files
  lapply(output_laz_files, rename_laz_files)
  
  # conditionally rename tif files if CHM was generated
  if (generate_chm) {
    # list all tif files
    output_tif_files <- list.files(path = output_dir_tif, pattern = '*.tif$', full.names = T)
    
    # function to rename tif files
    rename_tif_files <- function(file) {
      old_name <- basename(file)
      new_name <- stringr::str_replace(old_name, 'laz', 'lndom')
      new_file <- file.path(output_dir_tif, new_name)
      file.rename(file, new_file)
      cat('output file \'', old_name, '\' renamed to \'', new_name, '\'\n', sep = '')
    }
    
    # apply renaming function to all tif files
    lapply(output_tif_files, rename_tif_files)
  }
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  time.taken
  
  return(time.taken)
  
}
