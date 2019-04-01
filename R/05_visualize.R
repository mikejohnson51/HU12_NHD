library(sf)
library(dplyr)
library(HUCAgg)

st_layers("wbd_viz.gpkg")
hu_grouped <- read_sf("wbd_viz.gpkg", "wbd_grouped")
hu12 <- read_sf("wbd_viz.gpkg", "wbd_viz")

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

net <- read_sf("wbd_viz.gpkg", "net")
net <- filter(net, LevelPathI %in% hu_grouped$corrected_LevelPathI)

save.image("plot_data.RData")
load("plot_data.RData")

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
