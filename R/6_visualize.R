if(file.exists(wbd_viz)) {
  message(wbd_viz, " found, not rebuilding or loading PostGIS.")
} else {
  all <- readr::read_csv(hu_joiner)
  
  no_intersect <- filter(all, trib_no_intersect) %>%
    group_by(corrected_LevelPathI) %>%
    mutate(group_size = n()) %>%
    select(corrected_LevelPathI, group_size) %>%
    distinct()
  
  wbd <- read_sf("data/nhdplus/NHDPlusNationalData/NHDPlusV21_National_Seamless.gdb", "HUC12")
  
  wbd <- select(wbd, HUC12 = HUC_12, TOHUC = HU_12_DS)
  
  for(fix in 1:nrow(fixes)) {
    wbd$TOHUC[wbd$HUC12 == fixes$HUC12[fix]] <- fixes$TOHUC[fix]
  }
  
  wbd <- filter(wbd, !grepl("^20.*|^19.*|^21.*|^22.*", wbd$HUC12))
  wbd <- st_transform(wbd, 5070)
  
  write_sf(wbd, wbd_viz, "wbd_viz")
  
  net <- read_sf(file.path(nhd_dir, nhd_file), "NHDFlowline_Network") %>%
    st_zm()
  
  net <- st_transform(net, 5070) %>%
    st_simplify(dTolerance = 30)
  
  write_sf(net, wbd_viz, "net")
  
  wbd <- left_join(wbd, select(all, -TOHUC), by = "HUC12")
  
  wbd <- wbd[,11:ncol(wbd)]
  wbd <- select(wbd, -NONCONTRIBUTINGAREAACRES, -NONCONTRIBUTINGAREASQKM,
                -GLOBALID, -SHAPE_Length, -SHAPE_Length)
  
  write_sf(wbd, wbd_viz, "wbd_matched")
  
  wbd_grouped <- group_by(wbd, corrected_LevelPathI) %>%
    select(corrected_LevelPathI) %>%
    summarise(do_union = FALSE)
  
  write_sf(wbd_grouped, wbd_viz, "wbd_grouped")
  
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
  
  write_sf(net_grouped, wbd_viz, "net_grouped")
  
  system("bash bin/pg_setup.sh")
}

plot_data <- "cache/plot_data.RData"
if(file.exists(plot_data)) {
  message(plot_data, " cache exists, loading.")
  load(plot_data)
} else {
  hu_grouped <- read_sf(wbd_viz, "wbd_grouped")
  hu12 <- read_sf(wbd_viz, "wbd_viz")
  
  hu12 <- read_sf("WBD_National_GDB.gdb/", "WBDHU12") %>%
    st_transform(st_crs(hu_grouped))
  
  lookup <- readr::read_csv("map_joiner.csv")
  
  lookup <- filter(lookup, grepl("^03.*", lookup$HUC12)) %>%
    select(-intersected_LevelPathI, -trib_intersect, -trib_no_intersect, -headwater_error) %>%
    group_by(HUC12) %>%
    filter(corrected_LevelPathI == min(corrected_LevelPathI)) %>% # coastals with multiple get matched multiple times.
    ungroup()
  
  hu_grouped <- filter(hu_grouped, corrected_LevelPathI %in% lookup$corrected_LevelPathI)
  hu12 <- filter(hu12, HUC12 %in% lookup$HUC12)
  
  net <- read_sf(wbd_viz, "net")
  net <- filter(net, LevelPathI %in% hu_grouped$corrected_LevelPathI)
  
  save(net, hu_grouped, lookup, hu12_boundaries, plot_data)
}

# Plot order:
# 1) mainstem river and total upstream drainage boundary
# 2) mainstem HU12s
# 3) tributaries to mainstem with their drainage boundaries
# 4) mainstem HU12s of tributaries added in 3
# repeat till none left.

lp_dnlp <- select(st_set_geometry(net, NULL), LevelPathI, DnLevelPat) %>%
  distinct() %>%
  filter(LevelPathI != DnLevelPat)

# group_by(lookup, corrected_LevelPathI) %>% summarize(size = n())
levelpath <- 290002124 # entire tombigbee
# levelpath <- 290007998

plotter <- function(i) {
  png(filename = paste0("png/", i, ".png"), width = 600, height = 768, units = "px")
  par(mar=c(0, 0, 0, 0))
  plot(viz_basinboundary$geometry, lwd = 0.1, border = "white", col = NA)
  plot(viz_mainstem$geom, col = "blue", add = TRUE)
  plot(viz_hydrounits$geom, lwd = 0.5, border = "grey", col = NA, add = TRUE)
  dev.off()
}

i <- 1
viz_hydrounits <- filter(hu_grouped, corrected_LevelPathI %in% 0)
mainstem_hus <- distinct(filter(lookup, corrected_LevelPathI %in% levelpath))
viz_basinboundary <- filter(hu12_boundaries, HUC12 %in% unique(mainstem_hus$outlet_HUC12))

for(j in c(1:4)) {
  viz_mainstem <- filter(net, LevelPathI %in% levelpath)
  mainstem_hus <- distinct(filter(lookup, corrected_LevelPathI %in% levelpath))
  plotter(i)
  i <- i + 1
  viz_hydrounits <- filter(hu_grouped, corrected_LevelPathI %in% levelpath)
  plotter(i)
  next_lp <- filter(lp_dnlp, DnLevelPat %in% levelpath)$LevelPathI
  levelpath <- unique(c(levelpath, next_lp))
  i <- i + 1
}

plotter <- function(i) {
  png(filename = paste0("png/", i, ".png"), width = 450, height = 768, units = "px")
  par(mar=c(0, 0, 0, 0))
  plot(viz_basinboundary$geometry, lwd = 0.5)
  plot(viz_mainstem$geom, col = "blue", add = TRUE)
  plot(viz_hydrounits$geom, lwd = 0.5, border = "grey", col = NA, add = TRUE)
  dev.off()
}

joiner <- readr::read_csv("map_joiner.csv") %>%
  filter(grepl("^03.*", HUC12)) %>%
  select(HUC12, intersected_LevelPathI, corrected_LevelPathI)

intersected <- left_join(select(hu12, HUC12), joiner)

levelpath <- 290003311
viz_hydrounits <- filter(hu_grouped, corrected_LevelPathI %in% 0)
viz_mainstem <- filter(net, LevelPathI %in% levelpath)
mainstem_hus <- distinct(filter(lookup, corrected_LevelPathI %in% levelpath))
viz_basinboundary <- filter(hu12_boundaries, HUC12 %in% unique(mainstem_hus$outlet_HUC12))

plotter("fig1")
intersected_hydrounits <- filter(intersected, intersected_LevelPathI %in% levelpath)
corrected_hydrounits <- filter(intersected, corrected_LevelPathI %in% levelpath)

png(filename = "png/fig2.png", width = 450, height = 768, units = "px")
par(mar=c(0, 0, 0, 0))
plot(viz_basinboundary$geometry, lwd = 0.5)
plot(intersected_hydrounits$geom, lwd = 0.1, border = "grey", col = NA, add = TRUE)
plot(viz_mainstem$geom, col = "blue", add = TRUE)
plot(intersected_hydrounits$geom, lwd = 0.5, border = "grey", col = NA, add = TRUE)
dev.off()

head_hu <- unique(filter(lookup, corrected_LevelPathI == levelpath)$head_HUC12)
viz_head <- filter(hu12, HUC12 == head_hu)

png(filename = "png/fig3.png", width = 450, height = 768, units = "px")
par(mar=c(0, 0, 0, 0))
plot(viz_basinboundary$geometry, lwd = 0.5)
plot(intersected_hydrounits$geom, lwd = 0.1, border = "grey", col = NA, add = TRUE)
plot(viz_mainstem$geom, col = "blue", add = TRUE)
plot(intersected_hydrounits$geom, lwd = 0.5, border = "grey", col = NA, add = TRUE)
plot(viz_head$geom, lwd = 0.5, border = "red", col = "NA", add = TRUE)
dev.off()

png(filename = "png/fig4.png", width = 450, height = 768, units = "px")
par(mar=c(0, 0, 0, 0))
plot(viz_basinboundary$geometry, lwd = 0.5)
plot(intersected_hydrounits$geom, lwd = 0.5, border = "grey", col = NA, add = TRUE)
plot(viz_mainstem$geom, col = "blue", add = TRUE)
plot(corrected_hydrounits$geom, lwd = 0.5, border = "black", col = NA, add = TRUE)
plot(viz_head$geom, lwd = 0.5, border = "red", col = "NA", add = TRUE)
dev.off()

next_lp <- filter(lp_dnlp, DnLevelPat %in% levelpath)$LevelPathI
levelpath <- unique(c(levelpath, next_lp))
viz_mainstem <- filter(net, LevelPathI %in% levelpath)

png(filename = "png/fig5.png", width = 450, height = 768, units = "px")
par(mar=c(0, 0, 0, 0))
plot(viz_basinboundary$geometry, lwd = 0.5)
plot(intersected_hydrounits$geom, lwd = 0.5, border = "grey", col = NA, add = TRUE)
plot(corrected_hydrounits$geom, lwd = 0.5, border = "black", col = NA, add = TRUE)
plot(viz_mainstem$geom, col = "blue", add = TRUE)
dev.off()

#######


plotter <- function(i) {
  png(filename = paste0("png/", i, ".png"), width = 512, height = 768, units = "px")
  plot(viz_basinboundary$geometry, lwd = 0.1, border = "grey", col = NA)
  plot(viz_mainstem$geom, col = "blue", add = TRUE)
  plot(viz_hydrounits$geom, lwd = 0.5, border = "grey", col = NA, add = TRUE)
  dev.off()
}

levelpath <- 290001792 # ACF
viz_hydrounits <- filter(hu_grouped, corrected_LevelPathI %in% 0)
viz_mainstem <- filter(net, LevelPathI %in% levelpath)
mainstem_hus <- distinct(filter(lookup, corrected_LevelPathI %in% levelpath))
viz_basinboundary <- filter(hu12_boundaries, HUC12 %in% unique(mainstem_hus$outlet_HUC12))
plotter("example1")
next_lp <- filter(lp_dnlp, DnLevelPat %in% levelpath)$LevelPathI
levelpath <- unique(c(levelpath, next_lp))
viz_mainstem <- filter(net, LevelPathI %in% levelpath)
mainstem_hus <- distinct(filter(lookup, corrected_LevelPathI %in% levelpath))
viz_basinboundary <- filter(hu12_boundaries, HUC12 %in% unique(mainstem_hus$outlet_HUC12))
plotter("example2")
next_lp <- filter(lp_dnlp, DnLevelPat %in% levelpath)$LevelPathI
levelpath <- unique(c(levelpath, next_lp))
viz_mainstem <- filter(net, LevelPathI %in% levelpath)
mainstem_hus <- distinct(filter(lookup, corrected_LevelPathI %in% levelpath))
viz_basinboundary <- filter(hu12_boundaries, HUC12 %in% unique(mainstem_hus$outlet_HUC12))
plotter("example3")
