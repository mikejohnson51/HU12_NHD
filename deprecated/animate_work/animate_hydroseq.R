# https://github.com/dblodgett-usgs/nhdplusTools
library(nhdplusTools)
# https://github.com/USGS-R/HUCAgg
library(HUCAgg)
library(sf)
library(dplyr)
library(snow)

wbd_gdb <- "WBD_National_GDB.gdb"

proj <- 5070

wbd <- read_sf(wbd_gdb, "WBDHU12")

write_sf(wbd, "temp.shp")
system("mapshaper temp.shp -simplify 10% -o simp.shp")
wbd <- read_sf("simp.shp")

unlink("temp.shp", "temp.dbf", "temp.shx", "temp.prj")
unlink("simp.shp", "simp.dbf", "simp.shx", "simp.prj")

# Used to identify problem HU toHUCs
#
# fromHUC <-sapply(wbd$HUC12, fromHUC_finder,
#                  hucs=wbd$HUC12, tohucs=wbd$TOHUC)
# 
# aggrHUC <- sapply(wbd$HUC12, HUC_aggregator, fromHUC = fromHUC)

wbd$TOHUC[which(wbd$HUC12 == "090203160905")] <- "090203160906"
wbd$TOHUC[which(wbd$HUC12 == "031300130600")] <- "OCEAN"
wbd$TOHUC[which(wbd$HUC12 == "100500080502")] <- "100500080504"
wbd$TOHUC[which(wbd$HUC12 == "100500010301")] <- "100500010302"

wbd <- filter(wbd, !grepl("^19.*|^20.*|^21.*|^22.*", wbd$HUC12)) %>%
  st_transform(proj)

# Integer ID for HU12s
lookup <- data.frame(HUC12 = unique(wbd$HUC12),
                     stringsAsFactors = FALSE) %>%
  mutate(ID = seq(1:nrow(.)))

# Just the dendritic network.
wbd_net <- st_set_geometry(wbd, NULL) %>%
  left_join(lookup, by = "HUC12") %>%
  left_join(select(lookup, toID = ID, HUC12), by = c("TOHUC" = "HUC12")) %>%
  select(ID, toID, nameID = GNIS_, area = AREAS) %>%
  group_by(ID) %>%
  summarise(toID = toID[1], nameID = nameID[1], area = sum(area))

# from nhdplusTools
area <- calculate_total_drainage_area(distinct(select(wbd_net, ID, toID, area)))

wbd_net[["total_area"]] <- area
wbd_net[["nameID"]][is.na(wbd_net[["nameID"]]) | wbd_net[["nameID"]] == "0"] <- " "

# from nhdplusTools
level_paths <- calculate_levelpaths(select(wbd_net, ID, toID, nameID, weight = total_area))

# Add a headID, didn't use in the end, but worth demonstrating.
level_paths <- level_paths %>%
  group_by(levelpath) %>%
  arrange(desc(topo_sort)) %>%
  mutate(headID = first(ID)) %>%
  ungroup()

# Create a data.frame we will use for plotting later.
wbd_plotter <- select(wbd, HUC12) %>%
  group_by(HUC12) %>%
  summarise() %>% # Combines geometries from single part to multi-part
  left_join(lookup, by = "HUC12") %>%
  left_join(select(wbd_net, ID, toID), by = "ID") %>%
  left_join(distinct(level_paths), by = "ID") %>%
  mutate(toID = ifelse(is.na(toID), 0, toID)) %>% # terminals have ID = 0 now
  arrange(desc(topo_sort)) # Sort from upstream to downstream.

paths <- st_set_geometry(wbd_plotter, NULL) %>%
  group_by(levelpath) %>%
  summarise(length = n(), outletID = outletID[1], headID = headID[1]) %>%
  left_join(select(st_set_geometry(wbd_plotter, NULL), ID, outletID_toID = toID, topo_sort), 
            by = c("outletID" = "ID")) %>%
  arrange(topo_sort) # This sort gets things in downstream to upstream order.

# Moves all the terminals to the front.
paths <- bind_rows(filter(paths, outletID_toID == 0),
                   filter(paths, !outletID_toID == 0)) 

# Going to fill this in with the required plot order.
wbd_plotter$order <- rep(0, nrow(wbd_plotter))
for(i in seq_len(nrow(paths))) { 
  
  lp <- paths$levelpath[i]
  
  if(paths$outletID_toID[i] == 0) { # If it's an outlet
    # Start headwater of current path at the max path length minus current path length.
    start_step <- max(paths$length) - paths$length[i] + 1
  } else {
    # Start headwater of current path at order of the downstream 
    # trunk minus the length of the current path.
    start_step <- wbd_plotter$order[(which(wbd_plotter$ID == paths$outletID_toID[i]))] - paths$length[i]
  }
  
  # Insert the sequence for this path.
  wbd_plotter$order[which(wbd_plotter$levelpath == lp)] <- 
    seq(start_step, start_step + paths$length[i] - 1)
}

# The above results in negative values in some cases. 
# This normalizes everything to start at 1
wbd_plotter$order <- (wbd_plotter$order - min(wbd_plotter$order)) + 1

states <- read_sf("states.shp") %>%
  st_transform(proj) %>%
  rmapshaper::ms_simplify()

# Bonus fun plot.
plot(states$geometry, col = NA, lwd = 0.4)

for(i in 1:100) {
  plot(filter(wbd_plotter, levelpath == paths$levelpath[i])$SHAPE, add = TRUE, lwd = 0.3)
}

plot_hu_fun <- function(wbd_plotter, ids, png_file) {
  pd <- filter(wbd_plotter, ID %in% ids)
  png(png_file, width = 1024, height = 768)
  plot(st_geometry(states), lwd = 0.5, col = NA)
  # plot(wbd_plotter$SHAPE, col = NA, lwd = 0.1) # for testing
  plot(st_geometry(pd), 
       lwd = 0.5, col = NA, add = TRUE) 
  dev.off()
}

# Could do with other methods, but this uses more cores and speeds things up a bit.
par_union <-  function(g, wbd_plotter) {
  sf::st_union(sf::st_geometry(wbd_plotter)[which(wbd_plotter$ID == g)])
}

cl <- parallel::makeCluster(rep("localhost", 4), type = "SOCK")

id_toid <- select(st_set_geometry(wbd_plotter, NULL), ID, toID, order)

wbd_plotter <- select(wbd_plotter, ID, toID, order)

for(i in seq(1, max(wbd_plotter$order))) {
  png_file <- paste0("wbd", stringr::str_pad(i, 4, "left", "0"), ".png")
  
  ids <- filter(st_set_geometry(wbd_plotter, NULL),
                !wbd_plotter$ID %in% wbd_plotter$toID & # is a headwater
                wbd_plotter$order == i)$ID # and is on this step

  plot_hu_fun(wbd_plotter, c(0, ids), png_file)
  
  # update ID to next down and create group size.
  wbd_plotter <- mutate(wbd_plotter, ID = ifelse(ID %in% ids, toID, ID)) %>%
    group_by(ID) %>%
    mutate(g_size = n()) %>% 
    ungroup()
  
  # These need to get unioned together.
  to_combine <- unique(wbd_plotter$ID[which(wbd_plotter$g_size > 1)])
  
  # Could use group_by and summarize(do_union = TRUE) but this scales better.
  combined <- parSapply(cl, to_combine, 
                        par_union, wbd_plotter = wbd_plotter)
  
  # Update wbd_plotter removing stuff that just got combined and binding on combined stuff
  combined <- left_join(st_sf(ID = to_combine, geometry = st_sfc(combined)),
                        id_toid, by = "ID")
  wbd_plotter <- filter(wbd_plotter, g_size < 2)
  
  wbd_plotter <- bind_rows(select(wbd_plotter, -g_size),
                           combined) %>%
    st_sf()
}

parallel::stopCluster(cl)

