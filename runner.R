library(nhdplusTools)
library(hyRefactor)
library(sf)
library(dplyr)
library(HUCAgg)
library(drake)
library(snow)
library(xml2)
library(readr)

source("R/1_download_data.R")
source("R/2_fixes.R")
source("R/3_setup.R")
source("R/4_find_match.R")
source("R/5_find_outlets.R")
source("R/6_visualize.R")

plan <- drake_plan(
  ##### Constants
  cores = 3,
  prj = 5070,
  viz_simp = 30,
  proc_simp = 10,
  wbd_viz_gpkg = "wbd_viz.gpkg",
  temp_dir = "temp/",
  nhdplus_dir = "data/nhdp/",
  nhdplus_gdb = "NHDPlusNationalData/NHDPlusV21_National_Seamless.gdb",
  nhdplus_url = "https://s3.amazonaws.com/nhdplus/NHDPlusV21/Data/NationalData/NHDPlusV21_NationalData_CONUS_Seamless_Geodatabase_05.7z",
  ##### Load static dependencies
  nhdplus_wbd_fixes = get_fixes("nhdplusv2"),
  nhdplus_gdb_path = download_nhd_gdb(nhdplus_dir, nhdplus_gdb, nhdplus_url),
  nhdplus_wbd_exclusions = get_exclusions(nhdplus_gdb_path),
  ##### Load Data
  nhdplus_wbd = get_wbd(nhdplus_gdb_path, nhdplus_wbd_fixes, prj),
  nhdplus_net = get_net(read_sf(nhdplus_gdb_path, "NHDFlowline_Network"), prj),
  ##### Match NHDPlusV2 with stable (old) WBD
  nhdplus_oldwbd_out = "nhdplus_oldwbd_out/",
  nhdplus_oldwbd_hu_joiner = get_hu_joiner(nhdplus_net, nhdplus_wbd, proc_simp, cores, temp_dir, nhdplus_oldwbd_out),
  nhdplus_oldwbd_linked_points = get_linked_points(nhdplus_oldwbd_hu_joiner, nhdplus_net,
                                                   nhdplus_wbd, nhdplus_wbd_exclusions, cores,
                                                   file.path(nhdplus_oldwbd_out, "wbd_viz.gpkg")),
  nhdplus_oldwbd_write = write_output_gpkg(nhdplus_net, nhdplus_wbd, nhdplus_oldwbd_hu_joiner,
                    nhdplus_oldwbd_linked_points, prj, viz_simp, nhdplus_oldwbd_out),
  ##### Constants for newest WBD.
  wbd_dir = "data/wbd",
  wbd_zip_file = "WBD_National_GDB.zip",
  wbd_gdb_file = "WBD_National_GDB.gdb",
  wbd_url = "https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/WBD/National/GDB/WBD_National_GDB.zip",
  ##### Static dependencies for newest WBD
  wbd_fixes = get_fixes("latest"),
  wbd_gdb_path = download_wbd(wbd_dir, wbd_zip_file, wbd_gdb_file, wbd_url),
  wbd_exclusions = get_exclusions(wbd_gdb_path),
  wbd = get_wbd(wbd_gdb_path, wbd_fixes, prj),
  ##### Match newest WBD to NHDPlusV2.
  nhdplus_newwbd_out = "nhdplus_newwbd_out/",
  nhdplus_newwbd_hu_joiner = get_hu_joiner(nhdplus_net, wbd, proc_simp, cores, temp_dir, nhdplus_newwbd_out),
  nhdplus_newwbd_linked_points = get_linked_points(nhdplus_newwbd_hu_joiner, nhdplus_net, wbd, wbd_exclusions, cores,
                                                   file.path(nhdplus_newwbd_out, "wbd_viz.gpkg")),
  nhdplus_newwbd_write = write_output_gpkg(nhdplus_net, wbd, nhdplus_newwbd_hu_joiner,
                                           nhdplus_newwbd_linked_points, prj, viz_simp, nhdplus_newwbd_out),
  # Create plots for newest WBD matches.
  plot_data = geom_plot_data(wbd, nhdplus_net, nhdplus_newwbd_hu_joiner, "^03.*"),
  out_png = create_png(plot_data, nhdplus_newwbd_hu_joiner, "png/")
  ##### NHDPlsuHR Stuff
  # nhdhr_hu02 = c("01", "02"),
  # nhdhr_dir = "data/hr",
  # nhdhr_path = download_nhdhr(nhdhr_dir, nhdhr_hu02),
  # nhdhr = nhdhr_mod(nhdhr_path, file.path(nhdhr_dir, "nhdplushr.gpkg"), force_terminal = TRUE),
  # nhdhr_net = get_net(nhdhr, prj),
  # nhdplushr_newwbd_out = "nhdplushr_newwbd",
  # nhdplushr_newwbd_hu_joiner = get_hu_joiner(nhdhr_net, wbd, proc_simp, cores, temp_dir, nhdplushr_newwbd_out),
  # nhdplushr_newwbd_linked_points = get_linked_points(nhdplushr_newwbd_hu_joiner, nhdhr_net, wbd, wbd_exclusions, cores,
  #                                                    file.path(nhdplushr_newwbd_out, "wbd_viz.gpkg")),
  # nhdplushr_newwbd_write = write_output_gpkg(nhdhr_net, wbd, nhdplushr_newwbd_hu_joiner, 
  #                                           nhdplus_oldwbd_linked_points, prj, viz_simp, nhdplushr_newwbd_out),
)

config <- drake_config(plan = plan,
                       memory_strategy = "autoclean", 
                       garbage_collection = TRUE)
                       
make(config = config)
