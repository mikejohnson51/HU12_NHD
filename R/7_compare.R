network_join <- read_sf("NHDPlus_WBD_Alignment/NHDPlus_WBD_Alignment.gdb/", 
                        "network_join")

wbd <- read_sf(natdb, "HUC12")

exclude_type <- wbd$HUC_12[wbd$HU_12_TYPE %in% c("F", "I", "C")] # frontal closed or island
exclude_first_order_toHUC <- wbd$HUC_12[wbd$HU_12_DS %in% c("OCEAN", "CANADA", "GEATLAKES", "UNKNOWN") & 
                                          !wbd$HUC_12 %in% wbd$HU_12_DS] # Unless it has something flowing to it.

exclude <- unique(c(exclude_type, exclude_first_order_toHUC))

outlets <- st_set_geometry(network_join, NULL) %>%
  filter(outlet == 1 & !HUC_12 %in% exclude) %>%
  select(COMID, LevelPathI, REACHCODE, HUC_12) %>%
  distinct() %>%
  group_by(HUC_12) %>%
  mutate(gs = max(n())) %>%
  ungroup()

lp_points <- read_sf("linked_points.gpkg")

hu12_outlets <- select(st_set_geometry(wbd, NULL), HUC_12, HU_12_DS, HU_12_TYPE, HU_12_MOD) %>%
  filter(!HUC_12 %in% exclude) %>%
  left_join(select(outlets, RM_COMID = COMID, RM_LevelPathI = LevelPathI, 
                   RM_REACHCODE = REACHCODE, HUC_12), 
            by = "HUC_12") %>%
  left_join(select(st_set_geometry(lp_points, NULL), 
                   DB_COMID = COMID, DB_LevelPathI = LevelPathI, 
                   DB_REACHCODE = REACHCODE, HUC_12 = HUC12), 
            by = "HUC_12") %>%
  distinct()

db_only <- select(hu12_outlets, HUC_12, DB_COMID) %>%
  distinct()

rm_only <- select(hu12_outlets, HUC_12, RM_COMID) %>%
  distinct() %>%
  mutate(estimate = ifelse(!is.na(RM_COMID), 1, 0)) %>%
  select(-RM_COMID) %>%
  distinct()



matched <- filter(hu12_outlets, RM_COMID == DB_COMID)
cat(paste("HUC_12s where outlet catchments match:",
          length(unique(matched$HUC_12))), 
    file = "temp.txt", sep = "\n")

is_na_lp <- filter(hu12_outlets, is.na(DB_COMID))
cat(paste("HUC_12s where no match was found in levelpath method:",
      length(unique(is_na_lp$HUC_12))), 
    file = "temp.txt", append = TRUE, sep = "\n")

is_na_pp <- filter(rm_only, estimate == 0)
cat(paste("HUC_12s where no match was found in pour point method:",
    length(unique(is_na_pp$HUC_12))), 
    file = "temp.txt", append = TRUE, sep = "\n")

both_na <- filter(hu12_outlets, is.na(RM_COMID) & is.na(DB_COMID)) %>%
  select(HUC_12) %>%
  distinct()
cat(paste("HUC_12s where no match was found in either:",
      nrow(both_na)), 
    file = "temp.txt", append = TRUE, sep = "\n")

group_summary <- select(outlets, HUC_12, gs) %>%
  distinct()
cat(paste("Number of HUC_12s from the pour point method with multiple outlets.",
      sum(group_summary$gs > 1)), 
    file = "temp.txt", append = TRUE, sep = "\n")

in_lp_single <- filter(hu12_outlets, !is.na(DB_COMID)) %>%
  group_by(HUC_12) %>%
  mutate(gs = max(n())) %>%
  filter(gs == 1) %>%
  ungroup()

in_lp_single_match <- filter(in_lp_single, DB_COMID == RM_COMID)

cat(paste("Of the", nrow(in_lp_single), "HUC_12s that were found in",
          "both methods and have a single outlet in the pour point method,",
          round(100 * nrow(in_lp_single_match) / nrow(in_lp_single)), 
          "% match."), 
    file = "temp.txt", append = TRUE, sep = "\n")

in_lp_multi <- filter(hu12_outlets, !is.na(DB_COMID)) %>%
  group_by(HUC_12) %>%
  mutate(gs = max(n())) %>%
  filter(gs > 1) %>%
  ungroup()

in_lp_multi_match <- filter(in_lp_multi, DB_COMID == RM_COMID)

cat(paste("Of the", length(unique(in_lp_multi$HUC_12)), 
      "HUC_12s that were found in both methods",
      "and have more than one outlet in the pour point method",
      round(100 * length(unique(in_lp_multi_match$HUC_12)) / length(unique(in_lp_multi$HUC_12))), 
      "% of the pour-point sets contain the outlet found in the levelpath method"), 
    file = "temp.txt", append = TRUE, sep = "\n")

not_matched <- filter(hu12_outlets, !HUC_12 %in% in_lp_single_match$HUC_12 & 
                        !HUC_12 %in% in_lp_multi_match$HUC_12)

cat(paste(length(unique(not_matched$HUC_12)), 
          "were not matched to the nearest COMID"), 
    file = "temp.txt", append = TRUE, sep = "\n")

matched_lp <- filter(not_matched, RM_LevelPathI == DB_LevelPathI)
cat(paste(length(unique(matched_lp$HUC_12)), 
          "that were not matched to the nearest COMID were matched to the same LevelPathID"), 
    file = "temp.txt", append = TRUE, sep = "\n")

not_matched <- filter(not_matched, !HUC_12 %in% matched_lp$HUC_12)
cat(paste(length(unique(not_matched$HUC_12)), "were not matched"), 
    file = "temp.txt", append = TRUE, sep = "\n")

mismatch <- not_matched %>%
  filter(!is.na(DB_COMID) & !is.na(RM_COMID))

null_match_lp <- not_matched %>%
  filter(is.na(DB_COMID))

null_match_pp <- not_matched %>%
  filter(is.na(RM_COMID))

cat(paste("Of the", length(unique(not_matched$HUC_12)), "that were not matched,",
      length(unique(null_match_lp$HUC_12)), "were not attempted by the levelpath method.",
      length(unique(null_match_pp$HUC_12)), "were not attempted by the pour point method.",
      length(unique(mismatch$HUC_12)), "were mismatched between the two methods."), 
    file = "temp.txt", append = TRUE, sep = "\n")

outlet_categories <- select(st_set_geometry(wbd, NULL), HUC_12) %>%
  distinct() %>%
  mutate(matched = HUC_12 %in% matched$HUC_12,
         lp_no_match = HUC_12 %in% is_na_lp$HUC_12,
         pp_no_match = HUC_12 %in% is_na_pp$HUC_12,
         both_na = HUC_12 %in% both_na$HUC_12,
         lp_single = HUC_12 %in% in_lp_single$HUC_12,
         lp_single_match = HUC_12 %in% in_lp_single_match$HUC_12,
         lp_multi = HUC_12 %in% in_lp_multi$HUC_12,
         lp_multi_match = HUC_12 %in% in_lp_multi_match$HUC_12,
         not_matched = HUC_12 %in% not_matched$HUC_12,
         mismatch = HUC_12 %in% mismatch$HUC_12,
         null_lp = HUC_12 %in% null_match_lp$HUC_12,
         null_pp = HUC_12 %in% null_match_pp$HUC_12)
