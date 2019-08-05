library(nhdplusTools)
library(sf)
library(dplyr)
library(HUCAgg)
library(drake)
library(snow)
library(xml2)

source("R/1_download_data.R")
source("R/3_setup.R")
source("R/4_find_match.R")
source("R/8_hr_proc.R")

plan <- drake_plan(
  cores = 2,
  prj = 5070,
  nhdplus_dir = "data/nhdplus",
  nhdplus_gdb = "NHDPlusNationalData/NHDPlusV21_National_Seamless.gdb",
  nhdplus_url = "https://s3.amazonaws.com/nhdplus/NHDPlusV21/Data/NationalData/NHDPlusV21_NationalData_CONUS_Seamless_Geodatabase_05.7z",
  nhdplus_gdb_path = download_nhd_gdb(nhdplus_dir, nhdplus_gdb, nhdplus_url),
  nhdplus_net = get_net(read_sf(nhdplus_gdb_path, "NHDFlowline_Network"), prj),
  nhdhr_hu02 = c("01", "02", "03", "04", "05", "06", "07", "08", "09", 
                 "10", "11", "12", "13", "14", "15", "16", "17", "18"),
  nhdhr_dir = "data/nhdplushr",
  nhdhr_path = download_nhdhr(nhdhr_dir, nhdhr_hu02),
  nhdhr = get_nhdplushr(nhdhr_path, NULL, layers = "NHDPlusFlowlineVAA")[[1]],
  nhdhr_net_01 = get_nhdplushr("data/nhdplushr/10", layers = "NHDFlowline")[[1]],
  # nhdhr_net_01 = get_nhdplushr("data/nhdplushr/", layers = "NHDFlowline", 
  #                              pattern = paste0(c("01", "02", "03", "04"), ".*GDB.gdb$", collapse = "|"))[[1]],
  # nhdhr_net_02 = get_nhdplushr("data/nhdplushr/", layers = "NHDFlowline", 
  #                              pattern = paste0(c("05", "06", "07", "08", "09", "10", "11"), ".*GDB.gdb$", collapse = "|"))[[1]],
  # nhdhr_net_03 = get_nhdplushr("data/nhdplushr/", layers = "NHDFlowline", 
  #                              pattern = paste0(c("12", "13", "14", "15", "16", "17", "18"), ".*GDB.gdb$", collapse = "|"))[[1]],
  nhdhr_vaa = select(nhdhr, NHDPlusID, LevelPathI, DnLevelPat, DnHydroSeq, HydroSeq, VPUID),
  nhdplus_hw_outlets = nhdplusTools:::mr_hw_cat_out(nhdplus_net),
  hr_pairs = get_hr_pairs(nhdhr_path, nhdplus_hw_outlets, prj, cores),
  vaa_01 = filter_vaa(nhdhr_vaa, c("01", "02", "03", "04")),
  vaa_02 = filter_vaa(nhdhr_vaa, c("05", "06", "07", "08", "09", "10", "11")),
  vaa_03 = filter_vaa(nhdhr_vaa, c("12", "13", "14", "15", "16", "17", "18")),
  matched_lp_01 = match_flowpaths(source_flowline = nhdplus_net,
                                  target_catchment = NULL, # Done in nhdplus_hw_outlets
                                  target_flowline = vaa_01, # only VAA metadata
                                  hr_pair = hr_pairs, cores = cores),
  matched_lp_02 = match_flowpaths(source_flowline = nhdplus_net,
                                  target_catchment = NULL, # Done in nhdplus_hw_outlets
                                  target_flowline = vaa_02, # only VAA metadata
                                  hr_pair = hr_pairs, cores = cores),
  matched_lp_03 = match_flowpaths(source_flowline = nhdplus_net,
                                  target_catchment = NULL, # Done in nhdplus_hw_outlets
                                  target_flowline = vaa_03, # only VAA metadata
                                  hr_pair = hr_pairs, cores = cores),
  compare_01 = post_proc(matched_lp = matched_lp_02,
                         v2_net = nhdplus_net,
                         hr_net = nhdhr_net_01),
  plot_01 = plot_bad(comp = compare_01, hr_net = nhdhr_net_01, 
                     v2_net = nhdplus_net, out_folder = "png2")
  # compare_02 = post_proc(matched_lp = matched_lp_02,
  #                        v2_net = nhdplus_net,
  #                        hr_net = nhdhr_net_02),
  # compare_03 = post_proc(matched_lp = matched_lp_03,
  #                        v2_net = nhdplus_net,
  #                        hr_net = nhdhr_net_03)
)

make(plan)
