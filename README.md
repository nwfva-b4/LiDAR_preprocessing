# Preprocessing Light Detection and Ranging (LiDAR) data

## Description
This repository contains an R function for preprocessing LiDAR data with a focus on Airborne Laser Scanning (ALS) (preprocess_lidar_files.R). It's developed for tasks such as renaming, ground classification, filtering, normalization, and rasterization. The function is designed to be flexible and modular, supporting to various LiDAR processing workflows. The [lasR](https://github.com/r-lidar/lasR) package as well as one [LAStools](https://rapidlasso.de/lastools/) command are used for this, enabling fast processing of several LiDAR files.  

Additionally, it contains a short script for merging individual canopy height model (CHM) raster files, possibly processed with the function, into one large CHM (merge_CHM_tiles.R).
