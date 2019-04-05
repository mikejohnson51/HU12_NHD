library(nhdplusTools)
library(sf)
library(dplyr)
library(HUCAgg)
source("R/0_functions.R")

cores <- 3

# If new run delete temp files created in 4.
unlink("cache/temp/*")

nhd_dir <- "data/nhdplus"
nhd_file <- "NHDPlusNationalData/NHDPlusV21_National_Seamless.gdb"

source("R/1_download_data.R")
source("R/2_fixes.R")

process_cache <- "cache/nhd_wbd.RData"
net_cache <- "cache/net.rds"

source("R/3_setup.R")
source("R/4_find_match.R")

points_cache <- "cache/points_out.rds"
lp_hu_points <- "out/lp_hu_points.gpkg"
linked_points_gpkg <- "out/linked_points.gpkg"
source("R/5_find_outlets.R")

wbd_viz <- "out/wbd_viz.gpkg"
source("R/6_visualize.R")