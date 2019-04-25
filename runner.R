library(nhdplusTools)
library(sf)
library(dplyr)
library(HUCAgg)
library(drake)

source("R/0_functions.R")
source("R/1_download_data.R")
source("R/2_fixes.R")
source("R/3_setup.R")
source("R/4_find_match.R")
source("R/5_find_outlets.R")
source("R/6_visualize.R")

plan <- drake_plan(
  wbd_version = "nhdplusv2",
  cores = 3,
  nhd_dir = "data/nhdplus",
  nhd_file ="NHDPlusNationalData/NHDPlusV21_National_Seamless.gdb",
  nhdplus_url = "https://s3.amazonaws.com/nhdplus/NHDPlusV21/Data/NationalData/NHDPlusV21_NationalData_CONUS_Seamless_Geodatabase_05.7z",
  natdb = download_data(nhd_dir, nhd_file, nhdplus_url),
  fixes = get_fixes(wbd_version),
  net = get_net(natdb),
  net_proj = proj_net(net),
  wbd = get_wbd(natdb, fixes),
  process_data = get_process_data(natdb, net, wbd),
  hu_joiner_file = "out/map_joiner.csv",
  temp_dir = "temp/",
  hu_joiner = get_hu_joiner(hu_joiner_file, process_data, cores, temp_dir),
  exclude = get_exclusions(natdb),
  points_out = get_points_out(hu_joiner, net, wbd, exclude),
  lp_hu_points_file = "out/lp_hu_points.gpkg",
  lp_hu_points = write_lp_hu_points(points_out, wbd, lp_hu_points_file),
  linked_points_gpkg = "out/linked_points.gpkg",
  linked_points = get_linked_points(exclude, lp_hu_points, net_proj, linked_points_gpkg, cores),
  wbd_viz_gpkg = "out/wbd_viz.gpkg",
  delete_wbd_viz_gpkg = unlink(wbd_viz_gpkg),
  write_wbd_viz = write_sf(wbd, wbd_viz_gpkg, "wbd_viz"),
  net_viz = st_simplify(st_transform(net, 5070), dTolerance = 30),
  write_wbd_viz_net = write_sf(net_viz, wbd_viz_gpkg, "net"),
  wbd_matched = get_wbd_matched(hu_joiner, wbd),
  write_wbd_viz_matched = write_sf(wbd_matched, wbd_viz_gpkg, "wbd_matched"),
  wbd_grouped = get_wbd_grouped(wbd_matched),
  write_wbd_viz_grouped = write_sf(wbd_grouped, wbd_viz_gpkg, "wbd_grouped"),
  plot_data = geom_plot_data(wbd_grouped, wbd, net, hu_joiner, "^03.*"),
  out_png = create_png(plot_data, hu_joiner))

make(plan)

# After complete, run to add data to postgis.
system("bash bin/pg_setup.sh")
