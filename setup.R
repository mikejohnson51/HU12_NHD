library(nhdplusTools)
library(sf)
library(dplyr)

  natdb <- "data/nhdplus/NHDPlusNationalData/NHDPlusV21_National_Seamless.gdb/"
wbd_gdb <- "WBD_National_GDB.gdb"
st_layers(natdb)
st_layers(wbd_gdb)

wbd <- read_sf(wbd_gdb, "WBDHU12")

wbd$TOHUC[wbd$HUC12 == "090203160905"] <- "090203160906" # circular
wbd$TOHUC[wbd$HUC12 == "031300130600"] <- "OCEAN" # circular
wbd$TOHUC[wbd$HUC12 == "100500080502"] <- "100500080504" # circular
wbd$TOHUC[wbd$HUC12 == "100500010301"] <- "100500010302" # circular
wbd$TOHUC[wbd$HUC12 == "101900180706"] <- "102001010305" # Major route error

wbd <- filter(wbd, !grepl("^20.*|^19.*|^21.*|^22.*", wbd$HUC12))
wbd <- st_transform(wbd, 5070) %>%
  st_simplify(dTolerance = 30)

write_sf(wbd, "wbd_viz.gpkg")

net <- read_sf(natdb, "NHDFlowline_Network")
net <- st_zm(net)

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

net_prep <- st_join(net_prep, select(wbd, HUC12, TOHUC)) %>%
  st_set_geometry(NULL)

net_prep <- readRDS("net_prep_cache.rds")

net_prep <- st_set_geometry(net_prep, NULL)
net_atts <- st_set_geometry(net, NULL)
wbd_atts <- st_set_geometry(wbd, NULL)

net_atts <- net_atts[ ,1:40]

rm(wbd)
rm(net)
rm(natdb)
rm(wbd_gdb)

save(net_atts, net_prep, wbd_atts, file = "nhd_wbd.RData")