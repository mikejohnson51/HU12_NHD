post_proc <- function(matched_lp, v2_net, hr_net) {
  
  matched_lp <- matched_lp %>%
    rename(head_NHDPlusID = FEATUREID, 
           hr_LevelPath = member_hr_lp, 
           mr_LevelPath = LevelPathI) %>%
    distinct()
  
  v2_net <- filter(v2_net, LevelPathI %in% matched_lp$mr_LevelPath)
  hr_net <- filter(hr_net, NHDPlusID %in% matched_lp$NHDPlusID)
  
  matched_lp <- left_join(matched_lp, 
                          select(st_set_geometry(hr_net, NULL), 
                                 NHDPlusID, HydroSeq), 
                          by = "NHDPlusID")
  
  outlets <- group_by(matched_lp, mr_LevelPath) %>%
    filter(HydroSeq == min(HydroSeq)) %>%
    ungroup() %>% select(outlet_NHDPlusID = NHDPlusID, mr_LevelPath)
  
  matched_lp <- left_join(matched_lp, outlets, by = "mr_LevelPath")
  
  hr_outlets <- hr_net %>%
    filter(NHDPlusID %in% outlets$outlet_NHDPlusID) %>%
    select(NHDPlusID) %>%
    get_mid_coord()
  
  st_crs(hr_outlets) <- st_crs(hr_net)
  
  heads <- hr_net %>%
    filter(NHDPlusID %in% matched_lp$head_NHDPlusID) %>%
    select(NHDPlusID) %>%
    get_mid_coord()
  
  gc()
  
  v2_outlets <- v2_net %>%
    select(COMID, LevelPathI, Hydroseq) %>%
    filter(LevelPathI %in% matched_lp$mr_LevelPath) %>%
    group_by(LevelPathI) %>% 
    filter(Hydroseq == min(Hydroseq)) %>%
    ungroup() %>% select(LevelPathI)
  
  v2_outlets <- get_bottom_coord(v2_outlets)
  
  hr_outlets <- hr_outlets %>%
    left_join(distinct(select(matched_lp, NHDPlusID = outlet_NHDPlusID, mr_LevelPath)), 
              by = "NHDPlusID")
  
  v2_outlets <- v2_outlets %>%
    filter(LevelPathI %in% hr_outlets$mr_LevelPath)
  
  v2_outlets <- bind_cols(st_set_geometry(v2_outlets, NULL),
                          st_coordinates(v2_outlets) %>%
                            as.data.frame())
  
  hr_outlets <- st_transform(hr_outlets, st_crs(v2_net))
  
  hr_outlets <- bind_cols(st_set_geometry(hr_outlets, NULL),
                          st_coordinates(hr_outlets) %>%
                            as.data.frame())
  
  comp <- left_join(
    rename(v2_outlets, v2_X = X, v2_Y = Y),
    rename(hr_outlets, hr_X = X, hr_Y = Y),
    by = c("LevelPathI" = "mr_LevelPath")) %>% 
    mutate(distance = sqrt((hr_X - v2_X)^2 + (hr_Y - v2_Y)^2))
  
  comp <- left_join(comp,
                    left_join(
                      select(st_set_geometry(v2_net, NULL), LevelPathI, LENGTHKM) %>%
                        group_by(LevelPathI) %>%
                        summarise(mr_lengthkm = sum(LENGTHKM)), 
                      select(st_set_geometry(hr_net, NULL), NHDPlusID, LengthKM) %>%
                        left_join(select(matched_lp, NHDPlusID, mr_LevelPath), by = "NHDPlusID") %>%
                        group_by(mr_LevelPath) %>%
                        summarise(hr_lengthkm = sum(LengthKM)), 
                      by = c("LevelPathI" = "mr_LevelPath")),
                    by = "LevelPathI"
  )
  
  return(comp)
}

# for the HR flowpaths that are shorter than the MR flowpaths, 
# should find the network location where HR ends and should keep going.

# for the MR flowpaths that are shorter than the HR flowpaths, need to characterize still...

plot_bad <- function(comp, hr_net, v2_net, out_folder) {
  bad <- comp[comp$distance > 20000, ]
  mr_lp <- bad[1,]$LevelPathI
  
  hr_net <- st_transform(hr_net, st_crs(v2_net))
  
  lapply(1:nrow(bad), plot_comp, bad = bad, hr_net = hr_net, v2_net = v2_net, out_folder = out_folder)
}
  

get_mid_coord <- function(cats) {
  bind_cols(st_set_geometry(cats, NULL), 
            cats %>%
              st_coordinates() %>%
              as.data.frame() %>%
              group_by(L2) %>%
              filter(row_number() == round(n()/2)) %>%
              ungroup() %>%
              select(X, Y) %>%
              st_as_sf(coords = c("X", "Y"))) %>%
    st_sf()
}

get_bottom_coord <- function(cats) {
  bind_cols(st_set_geometry(cats, NULL), 
            cats %>%
              st_coordinates() %>%
              as.data.frame() %>%
              group_by(L2) %>%
              filter(row_number() == n()) %>%
              ungroup() %>%
              select(X, Y) %>%
              st_as_sf(coords = c("X", "Y"))) %>%
    st_sf()
}

plot_comp <- function(row, bad, hr_net, v2_net, states, out_folder) {
  mr_lp <- bad[row, ]$LevelPathI
  
  hr <- filter(hr_net, NHDPlusID %in% matched_lp$NHDPlusID[matched_lp$mr_LevelPath == mr_lp])
  mr <- filter(v2_net, LevelPathI == mr_lp)
  
  hr_length <- sum(hr$LengthKM)
  mr_length <- sum(mr$LENGTHKM)
  
  plot(st_geometry(mr))
  plot(st_geometry(st_zm(hr)), add = TRUE, col = "red", lwd = 3)
  
  mr_bb <- st_bbox(mr)
  hr_bb <- st_bbox(hr)
  bbox <- c(min(mr_bb[1], hr_bb[1]), min(mr_bb[2], hr_bb[2]), 
            max(mr_bb[3], hr_bb[3]), max(mr_bb[4], hr_bb[4]))
  
  names(bbox) <- names(mr_bb)
  class(bbox) <- "bbox"
  bbox <- st_as_sfc(bbox)
  st_crs(bbox) <- st_crs(mr)
  
  bgmap <- bbox %>%
    st_transform(4326) %>%
    st_bbox() %>%
    setNames(c("left", "bottom", "right", "top")) %>%
    ggmap::get_stamenmap(maptype = "terrain", zoom = 10)
  
  png(file.path(out_folder, paste0(round(bad[row, ]$distance/1000), "_", mr_lp, ".png")))
  plot(st_transform(bbox, 3857), 
       border = NA, 
       main = paste("Distance", bad[row, ]$distance/1000, "km"), 
       sub = paste("MR Length:", mr_length, "km, HR Length:", hr_length, "km \n"),
       bgMap = bgmap)
  plot(st_transform(st_geometry(mr), 3857), col = "blue", add = TRUE)
  plot(st_transform(st_geometry(st_zm(hr)), 3857), add = TRUE, col = "red", lwd = 3)
  usr <- par()$usr
  x_scl <- (usr[2] - usr[1])/100
  y_scl <- (usr[4] - usr[3])/100
  mr_l_y <- usr[3] + 5 * y_scl
  hr_l_y <- usr[3] + 5 * y_scl
  mr_l_x <- usr[1] + 20 * x_scl
  hr_l_x <- usr[1] + 60 * x_scl
  l_l <- 5 * x_scl
  segments(mr_l_x, mr_l_y, mr_l_x + l_l, mr_l_y, col = "blue", lwd = 2)
  segments(hr_l_x, hr_l_y, hr_l_x + l_l, hr_l_y, col = "red", lwd = 2)
  text(mr_l_x + 3 * x_scl, mr_l_y, "NHDPlusV2", pos = 4)
  text(hr_l_x + 3 * x_scl, hr_l_y, "NHDPlusHR", pos = 4)
  dev.off()
}
