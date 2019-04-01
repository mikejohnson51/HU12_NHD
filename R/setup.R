library(nhdplusTools)
library(sf)
library(dplyr)

natdb <- "data/nhdplus/NHDPlusNationalData/NHDPlusV21_National_Seamless.gdb/"
wbd_gdb <- "WBD_National_GDB.gdb"
st_layers(natdb)
st_layers(wbd_gdb)

wbd <- read_sf(wbd_gdb, "WBDHU12")

fixes <- readr::read_csv("hu_fixes.csv")

for(fix in 1:nrow(fixes)) {
  wbd$TOHUC[wbd$HUC12 == fixes$HUC12[fix]] <- fixes$TOHUC[fix]
}

wbd <- filter(wbd, !grepl("^20.*|^19.*|^21.*|^22.*", wbd$HUC12))
wbd <- st_transform(wbd, 5070)

net <- read_sf(natdb, "NHDFlowline_Network") %>%
  st_zm()

net_prep <- prepare_nhdplus(net, 0, 0, TRUE, TRUE) %>%
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
net_atts <- st_set_geometry(net, NULL)[ ,1:40]
rm(net)

net_prep <- st_transform(net_prep, 5070)

net_prep <- st_simplify(net_prep, 10)
wbd <- st_simplify(wbd, 10)

net_prep <- st_join(net_prep, select(wbd, HUC12, TOHUC)) %>%
  st_set_geometry(NULL)
saveRDS(net_prep, "net_prep_cache.rds")

net_prep <- readRDS("net_prep_cache.rds")

wbd_atts <- st_set_geometry(wbd, NULL)

rm(wbd)
rm(wbd_gdb)
rm(fix)
rm(fixes)
rm(natdb)

save(net_atts, net_prep, wbd_atts, file = "nhd_wbd.RData")

rm(net_atts)
rm(net_prep)
rm(wbd_atts)
