library(nhdplusTools)
library(sf)
library(dplyr)
library(HUCAgg)
library(drake)
library(snow)

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
  prj = 5070,
  viz_simp = 30,
  proc_simp = 10,
  nhd_dir = "data/nhdplus",
  nhd_file ="NHDPlusNationalData/NHDPlusV21_National_Seamless.gdb",
  nhdplus_url = "https://s3.amazonaws.com/nhdplus/NHDPlusV21/Data/NationalData/NHDPlusV21_NationalData_CONUS_Seamless_Geodatabase_05.7z",
  wbd_dir = nhd_dir,
  wbd_zip_file = NA,
  wbd_gdb_file = nhd_file,
  wbd_url = NA,
  hu_joiner_file = "out_nhdp/map_joiner.csv",
  lp_hu_points_file = "out_nhdp/lp_hu_points.gpkg",
  linked_points_gpkg = "out_nhdp/linked_points.gpkg",
  wbd_viz_gpkg = "out_nhdp/wbd_viz.gpkg",
  temp_dir = "temp/",
  natdb = download_data(nhd_dir, nhd_file, nhdplus_url),
  wbd_gdb = download_wbd(wbd_dir, wbd_zip_file, wbd_gdb_file, wbd_url),
  fixes = get_fixes(wbd_version),
  net = get_net(natdb, prj),
  wbd = get_wbd(natdb, fixes, prj),
  net_prep = prep_net(net, proc_simp), 
  process_data = get_process_data(net_prep, wbd, proc_simp),
  hu_joiner = get_hu_joiner(hu_joiner_file,
                            net_atts = st_set_geometry(net, NULL)[ ,1:40], 
                            wbd_atts = st_set_geometry(wbd, NULL),
                            process_data, cores, temp_dir),
  exclude = get_exclusions(natdb),
  points_out = get_points_out(hu_joiner, net, wbd, exclude),
  lp_hu_points = write_lp_hu_points(points_out, wbd, lp_hu_points_file),
  linked_points = get_linked_points(exclude, lp_hu_points, net, linked_points_gpkg, cores),
  net_viz = st_simplify(st_transform(net, prj), dTolerance = viz_simp),
  wbd_matched = get_wbd_matched(hu_joiner, wbd),
  wbd_grouped = get_wbd_grouped(wbd_matched),
  delete_wbd_viz_gpkg = unlink(wbd_viz_gpkg),
  write_wbd_viz = write_sf(wbd, wbd_viz_gpkg, "wbd_viz"),
  write_wbd_viz_net = write_sf(net_viz, wbd_viz_gpkg, "net"),
  write_wbd_viz_matched = write_sf(wbd_matched, wbd_viz_gpkg, "wbd_matched"),
  write_wbd_viz_grouped = write_sf(wbd_grouped, wbd_viz_gpkg, "wbd_grouped"),
  plot_data = geom_plot_data(wbd_grouped, wbd, net, hu_joiner, "^03.*"),
  out_png = create_png(plot_data, hu_joiner, "out_nhdp/png/")
)

make(plan)

# After complete, run to add data to postgis.
# system("bash bin/pg_setup.sh")
