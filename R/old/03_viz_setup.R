library(sf)
library(dplyr)

all <- readr::read_csv("map_joiner.csv")

no_intersect <- filter(all, trib_no_intersect) %>%
  group_by(corrected_LevelPathI) %>%
  mutate(group_size = n()) %>%
  select(corrected_LevelPathI, group_size) %>%
  distinct()

# wbd <- read_sf("WBD_National_GDB.gdb/", "WBDHU12")
wbd <- read_sf("data/nhdplus/NHDPlusNationalData/NHDPlusV21_National_Seamless.gdb", "HUC12")

wbd <- select(wbd, HUC12 = HUC_12, TOHUC = HU_12_DS)

for(fix in 1:nrow(fixes)) {
  wbd$TOHUC[wbd$HUC12 == fixes$HUC12[fix]] <- fixes$TOHUC[fix]
}

wbd <- filter(wbd, !grepl("^20.*|^19.*|^21.*|^22.*", wbd$HUC12))
wbd <- st_transform(wbd, 5070)

write_sf(wbd, "wbd_viz.gpkg", "wbd_viz")
# unlink("wbd_viz.gpkg")

# net <- read_sf("data/nhdplus/NHDPlusNationalData/NHDPlusV21_National_Seamless.gdb/", "NHDFlowline_Network") %>%
#   st_zm()
# net <- st_transform(net, 5070) %>%
#   st_simplify(dTolerance = 30)
#
# write_sf(net, "wbd_viz.gpkg", "net")

wbd <- left_join(wbd, select(all, -TOHUC), by = "HUC12")
# wbd <- wbd[,11:ncol(wbd)]
# wbd <- select(wbd, -NONCONTRIBUTINGAREAACRES, -NONCONTRIBUTINGAREASQKM, 
#               -GLOBALID, -SHAPE_Length, -SHAPE_Length)

write_sf(wbd, "wbd_viz.gpkg", "wbd_matched")

wbd_grouped <- group_by(wbd, corrected_LevelPathI) %>%
  select(corrected_LevelPathI) %>%
  summarise(do_union = FALSE)

write_sf(wbd_grouped, "wbd_viz.gpkg", "wbd_grouped")

net_grouped <- select(net, LevelPathI)

# Could do with other methods, but this uses more cores and speeds things up a bit.
par_union <-  function(id, geom) {
  sf::st_union(sf::st_geometry(geom)[which(geom$LevelPathI == id)])
}

cl <- parallel::makeCluster(rep("localhost", 4), type = "SOCK")

level_paths <- unique(net_grouped$LevelPathI)

# Could use group_by and summarize(do_union = TRUE) but this scales better.
net_grouped <- snow::parSapply(cl, level_paths, 
                         par_union, geom = net_grouped)

parallel::stopCluster(cl)

net_grouped <- st_sf(LevelPathID = level_paths, geometry = st_sfc(net_grouped), 
                     crs = st_crs(net_grouped))

write_sf(net_grouped, "wbd_viz.gpkg", "net_grouped")

system("bash pg_setup.sh")