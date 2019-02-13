library(nhdplusTools)
library(sf)

natdb <- "data/nhdplus/NHDPlusNationalData/NHDPlusV21_National_Seamless.gdb/"
wbd_gdb <- "WBD_National_GDB.gdb"
st_layers(natdb)
st_layers(wbd_gdb)

wbd <- read_sf(wbd_gdb, "WBDHU12")

wbd$TOHUC[which(wbd$HUC12 == "090203160905")] <- "090203160906"
wbd$TOHUC[which(wbd$HUC12 == "031300130600")] <- "OCEAN"
wbd$TOHUC[which(wbd$HUC12 == "100500080502")] <- "100500080504"
wbd$TOHUC[which(wbd$HUC12 == "100500010301")] <- "100500010302"
wbd$TOHUC[which(wbd$HUC12 == "101900180706")] <- "102001010305"

net <- read_sf(natdb, "NHDFlowline_Network")
net <- st_zm(net)

nldi_huc12 <- list(featureSource = "huc12pp",
                   featureID = "103002000804")
# Wisconsin 070700051802
# Mississippi 071401010603
# Missouri 103002000804
start_comid <- discover_nhdplus_id(nldi_feature = nldi_huc12)

# start_comid <- filter(net, LevelPathI == filter(net, COMID == start_comid)$LevelPathI & 
#                         TerminalFl == 1)$COMID

# Funky basin in the missouri
start_comid <- 2279159

all_comid <- get_UT(net, start_comid)

net_prep <- filter(net, COMID %in% all_comid) %>%
  prepare_nhdplus(0, 0, TRUE, TRUE) %>%
  left_join(select(net, COMID, DnLevelPat, AreaSqKM)) %>%
  st_sf() %>%
  group_by(LevelPathI) %>%
  arrange(Hydroseq) %>%
  mutate(DnLevelPat = DnLevelPat[1]) %>%
  ungroup()

net_prep["denTotalAreaSqKM"] <- 
  calculate_total_drainage_area(select(st_set_geometry(net_prep, NULL), 
                                       ID = COMID, toID = toCOMID, 
                                       area = AreaSqKM))

net_prep <- st_join(net_prep, select(wbd, HUC12, TOHUC)) %>%
  st_set_geometry(NULL)

net_prep2 <- bind_rows(net_prep, 
                       select(filter(st_set_geometry(wbd, NULL), 
                                     !HUC12 %in% net_prep$HUC12),
                              HUC12, TOHUC))

lp_hu_df <- match_levelpaths(net_prep2, start_comid, add_checks = TRUE)

length(which(lp_hu_df$trib_no_intersect))
length(which(lp_hu_df$trib_intersect))
# length(unique(lp_hu_df$HUC12))
# length(which(grepl("^0707.*", wbd$HUC12)))
# 
# expected_hus <- wbd$HUC12[which(grepl("^0707.*", wbd$HUC12))]
# found_hus <- unique(lp_hu_df$HUC12)
# 
# expected_hus[which(!expected_hus %in% found_hus)]

# "070700030902" is a goof ball but maybe could work?
# "070700031801" doesn't have any intersecting it.
# "070700051803" goes to the downstream most HU that is part of the Mississippi need to fix that with toHUC logic
# "070700021101" isn't actually in 0707...
# "070700030501" no network

net_bbox <- st_bbox(filter(net, COMID %in% net_prep$COMID)) %>%
  st_as_sfc()

st_crs(net_bbox) <- st_crs(4269)
count <- 0

for(lp in sort(unique(hu$LevelPathI))) {
  # For plotting

  file <- paste0("unstacked/file_", stringr::str_pad(count, width = 4, side = "left", pad = "0"), ".png")
  png(filename = file, bg = "transparent", height = 1024, width = 1024)
  plot_sf(net_bbox, col = NA, lwd = NA)
  plot(filter(wbd, HUC12 %in% hu$HUC12[which(hu$LevelPathI == lp)])$SHAPE, add = TRUE)
  plot(filter(net, LevelPathI == lp)$Shape, add = TRUE, col = "blue")
  dev.off()
  count <- count + 1
}
