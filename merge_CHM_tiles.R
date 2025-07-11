#---------------------------------------------------------------------------------------
# Name:         merge_CHM_tiles.R
# Description:  This script merges individual raster CHM tiles into one big raster file.
#               Pixels below 0.1 m are set to 0. NA values are filled.
#               The resulting CHM is written to disk.
#               To apply the script, one has just to set the input directory
#               to the individual CHM tiles (line 18) and the output directory
#               to which the merged and filled CHM file should be written (line 21).
#               The name of the output file can be defined in line 24
#               together with the name of the file when it's loaded into R (line 27). 
# Contact:      florian.franz@nw-fva.de
#---------------------------------------------------------------------------------------

# file path and name definitions
#--------------------------------

# input directory to CHM raster files
input_dir <- r'{...}'

# output directory to final CHM raster file
output_dir <- r'{...}'

# define file name of output CHM
output_file_name <- '.tif'

# define output layer name
output_lyr_name <- ''

#--------------------------------

# read CHM input raster files into one list
chm_files <- list.files(input_dir, pattern = '\\.tif$')
chms <- lapply(file.path(input_dir, chm_files), terra::rast)

# merge to one big raster file
chms_sprc <- terra::sprc(chms)
chm_merged <- terra::merge(chms_sprc)
chm_merged

# remove pixels less than 0.1 m (set to 0)
chm_merged[chm_merged < 0.1] <- 0
chm_merged

# fill NA values
fill.na <- function(x, i=5) {
  if (is.na(x)[i]) {
    return(mean(x, na.rm = TRUE))
  } else {
    return(x[i]) 
  }
}
w <- matrix(1,3,3)
chm_merged_filled <- terra::focal(chm_merged, w, fun = fill.na)

# write merged and filled CHM to disk
terra::writeRaster(chm_merged_filled,
                   file.path(output_dir, output_file_name),
                   names = output_lyr_name)
