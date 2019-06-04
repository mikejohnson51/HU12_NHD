get_exclusions <- function(natdb) {
  wbd <- read_sf(natdb, "HUC12")
  
  wbd_type <- select(st_set_geometry(wbd, NULL), 
                     HUC12 = HUC_12, type = HU_12_TYPE) %>%
    distinct()
  
  wbd <- group_by(wbd, HUC_12) %>%
    summarise(HU_12_DS = HU_12_DS[1]) %>%
    rename(HUC12 = HUC_12, TOHUC = HU_12_DS)
  
  # Exclusions where river-flow does not apply:
  exclude_type <- wbd_type$HUC12[wbd_type$type %in% c("F", "I", "C", "U")] # frontal closed or island
  exclude_first_order_toHUC <- wbd$HUC12[wbd$TOHUC %in% c("OCEAN", "CANADA", "GEATLAKES", "UNKNOWN") & 
                                           !wbd$HUC12 %in% wbd$TOHUC] # Unless it has something flowing to it.
  
  exclude <- unique(c(exclude_type, exclude_first_order_toHUC))
  
  return(exclude)
}

get_points_out <- function(hu_lp, net, wbd, exclude) {
  
  # Found duplicate levelpaths for some HUs that can be removed.
  hu_lp <- group_by(hu_lp, HUC12) %>%
    filter(corrected_LevelPathI == min(corrected_LevelPathI)) %>%
    ungroup()
  
  wbd <- filter(wbd, !HUC12 %in% exclude)
  hu_lp <- filter(hu_lp, !HUC12 %in% exclude)
  
  lp_ids <- unique(hu_lp$corrected_LevelPathI)
  
  if(st_crs(net) != st_crs(wbd)) net <- st_transform(net, st_crs(wbd))
  
  points <- setNames(lapply(X = lp_ids, 
                            FUN = run_lp, 
                            net = net, hu_lp = hu_lp, wbd = wbd),
                     lp_ids)
  
  return(points)
}

write_lp_hu_points <- function(points, wbd, lp_hu_points_file) {
  lp_points <- lapply(names(points), 
                      function(lp, points) {
                        hu_points <- bind_rows(lapply(points[lp], hu_points_fun))
                        
                        hu_points[["lp"]] <- lp
                        
                        return(hu_points)
                      }, points = points) %>%
    bind_rows() %>%
    st_sf()
  
  st_crs(lp_points) <- st_crs(wbd)
  write_sf(lp_points, lp_hu_points_file)
  return(lp_points)
}

get_linked_points <- function(exclude, lp_points, net, linked_points_gpkg, cores) {

  lp_points <- lp_points %>%
    filter(!hu12 %in% exclude) %>%
    mutate(lp = as.numeric(lp)) %>%
    group_by(hu12) %>%
    filter(lp == min(lp)) %>%
    ungroup()

  filter_na <- is.na(unname(st_coordinates(lp_points)[, 1]))
  na_points <- filter(lp_points, filter_na)
  lp_points <- filter(lp_points, !filter_na)

  na_points <- st_set_geometry(na_points, NULL)

  na_points <- distinct(na_points)

  both <- filter(na_points, na_points$hu12 %in% lp_points$hu12) # Only broken border HUs included.

  na_points <- filter(na_points, !hu12 %in% both)
  lp_points <- filter(lp_points, !hu12 %in% both)
  
  na_outlets <- net %>%
    filter(LevelPathI %in% na_points$lp) %>%
    group_by(LevelPathI) %>%
    filter(Hydroseq == min(Hydroseq)) %>%
    ungroup() %>%
    left_join(na_points, by = c("LevelPathI" = "lp"))

  problem_na <- filter(na_outlets, FromMeas != 0)

  na_outlets <- filter(na_outlets, FromMeas == 0)

  na_outlet_coords <- st_coordinates(na_outlets) %>%
    as.data.frame() %>%
    group_by(L2) %>%
    filter(row_number() == n()) %>%
    ungroup() %>%
    select(-L1, -L2) %>%
    bind_cols(st_set_geometry(na_outlets, NULL)) %>%
    st_as_sf(coords = c("X", "Y"), crs = st_crs(na_outlets)) %>%
    rename(geom = geometry)

  na_outlet_coords$REACH_meas <- 0
  na_outlet_coords$offset <- 0

  na_outlet_coords <- select(na_outlet_coords, 
                             COMID, REACHCODE, REACH_meas, 
                             offset, HUC12 = hu12, LevelPathI)

  lp_list <- unique(lp_points$lp)

  net <- select(net, COMID, LevelPathI, REACHCODE, ToMeas, FromMeas, Hydroseq) %>%
    filter(LevelPathI %in% lp_list)

  in_list_fun <- function(lp_search, net, lp_points) {
    list(lp_search = lp_search,
         lp_geom = filter(net, LevelPathI == lp_search),
         hu_points = filter(lp_points, lp == lp_search))
  }

  in_list <- lapply(lp_list, in_list_fun, net = net, lp_points = lp_points)

  gc()
  
  cl <- parallel::makeCluster(rep("localhost", cores), 
                              type = "SOCK", outfile = "par.log")

  linked <- parLapply(cl, in_list, par_linker)

  parallel::stopCluster(cl)

  linked <- st_sf(do.call(rbind, linked), crs = st_crs(na_outlet_coords)) %>%
    select(COMID, REACHCODE, REACH_meas, offset, HUC12 = hu12, LevelPathI = lp)
  
  names(linked)[names(linked) == attr(linked, "sf_column")] <- 
    names(na_outlet_coords)[names(na_outlet_coords) == attr(na_outlet_coords, "sf_column")]
  
  attr(linked, "sf_column") <- attr(na_outlet_coords, "sf_column")
  
  linked <- rbind(linked, na_outlet_coords) %>%
    st_sf()

  write_sf(linked, linked_points_gpkg)
  return(linked)
}
