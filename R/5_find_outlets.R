get_exclusions <- function(wbd_gdb) {
  
  if("HUC12" %in% st_layers(wbd_gdb)$name) {
    wbd <- read_sf(wbd_gdb, "HUC12")
    wbd <- rename(wbd, HUC12 = HUC_12, TOHUC = HU_12_DS)
  } else {
    wbd <- read_sf(wbd_gdb, "WBDHU12")
  }
  
  wbd_type <- st_set_geometry(wbd, NULL)
  
  if("HUTYPE" %in% names(wbd_type)) {
    wbd_type <- distinct(select(wbd_type, HUC12, HUTYPE))
  } else {
    wbd_type <- distinct(select(wbd_type, HUC12, HUTYPE = HU_12_TYPE))
  }
  
  wbd <- group_by(wbd, HUC12) %>%
    summarise(TOHUC = TOHUC[1])
  
  # Exclusions where river-flow does not apply:
  exclude_type <- wbd_type$HUC12[wbd_type$HUTYPE %in% c("F", "I", "C", "U")] # frontal closed or island
  exclude_first_order_toHUC <- wbd$HUC12[wbd$TOHUC %in% c("OCEAN", "CANADA", "GEATLAKES", "UNKNOWN") & 
                                           !wbd$HUC12 %in% wbd$TOHUC] # Unless it has something flowing to it.
  
  exclude <- unique(c(exclude_type, exclude_first_order_toHUC))
  
  return(exclude)
}

hu_points_fun <- function(hp) {
  if(length(unlist(hp)) > 0) {
    out <- do.call(rbind, 
                   lapply(1:length(hp), 
                          function(x) {
                            if(length(unlist(hp[[x]])) > 0) {
                              o <- lapply(1:length(hp[[x]]), 
                                          function(y) st_cast(hp[[x]][y], "POINT"))
                              o <- data.frame(geometry = do.call(c, o))
                              o[["hu12"]] <- names(hp[x])
                            } else {
                              o <- data.frame(hu12 = names(hp[x]), stringsAsFactors = FALSE)
                              o[["geometry"]] <- list(st_point())
                            }
                            return(o)
                          }))
  } else {
    out <- data.frame(hu12 = names(hp), stringsAsFactors = FALSE)
    out[["geometry"]] <- list(st_point())
  }
  
  return(out)
}

run_lp <- function(lp_id, net, hu_lp, wbd) {
  out <- NULL
  tryCatch({
    lp <- net[net$LevelPathI == lp_id, ] %>%
      st_geometry()
    
    hu_ids <- hu_lp[hu_lp$corrected_LevelPathI == lp_id, ]
    hus <- wbd[wbd$HUC12 %in% hu_ids$HUC12, ]
    st_geometry(hus) <- st_cast(st_geometry(hus), "MULTILINESTRING")
    
    out <- setNames(lapply(1:nrow(hus), 
                           function(i, lp, hus) {
                             suppressMessages(
                               st_intersection(lp, st_geometry(hus)[i])
                             )
                           }, 
                           lp = lp, hus = hus), 
                    hus$HUC12)
  }, 
  error = function(e) {
    warning(paste(lp_id, e))
  }, 
  warning = function(w) {
    warning(paste(lp_id, w))
  })
  return(out)
}

par_linker <- function(lp_list) {
  library(nhdplusTools)
  library(dplyr)
  library(sf)
  linked <- NULL
  tryCatch({
    linked <- get_flowline_index(lp_list$lp_geom, lp_list$hu_points, search_radius = 1000) %>%
      bind_cols(lp_list$hu_points) %>%
      left_join(select(st_set_geometry(lp_list$lp_geom, NULL), COMID, Hydroseq), by = "COMID") %>%
      group_by(hu12) %>%
      filter(Hydroseq == min(Hydroseq))
    
    if(any(group_size(linked) > 1)) {
      linked <- linked %>%
        group_by(hu12, REACHCODE) %>%
        filter(REACH_meas == min(REACH_meas))
    }
    
    linked <- ungroup(linked)
  },
  error = function(e) warning(paste(lp_list$lp_search, e)),
  warning = function(w) warning(paste(lp_list$lp_search, w)))
  return(linked)
}

get_points_out <- function(hu_lp, net, wbd, exclude) {
  
  hu_lp <- group_by(hu_lp, HUC12) %>%
    filter(corrected_LevelPathI == min(corrected_LevelPathI)) %>%
    ungroup()
  
  wbd <- filter(wbd, !HUC12 %in% exclude)
  hu_lp <- filter(hu_lp, !HUC12 %in% exclude)
  
  lp_ids <- unique(hu_lp$corrected_LevelPathI)
  
  if(st_crs(net) != st_crs(wbd)) {
    net <- st_transform(net, st_crs(wbd))
  }
  
  net <- net[net$LevelPathI %in% lp_ids, ]
  
  points <- setNames(lapply(X = lp_ids, 
                            FUN = run_lp, 
                            net = net, hu_lp = hu_lp, wbd = wbd),
                     lp_ids)
  
  return(points)
}

get_lp_hu_points <- function(points, prj) {
  lp_points <- lapply(names(points), 
                      function(lp, points) {
                        hu_points <- bind_rows(lapply(points[lp], hu_points_fun))
                        
                        hu_points[["lp"]] <- lp
                        
                        return(hu_points)
                      }, points = points) %>%
    bind_rows() %>%
    st_sf()
  
  st_crs(lp_points) <- prj
  
  return(lp_points)
}

get_lp_points <- function(hu_lp, net, wbd, exclude) {
  lp_points <- get_points_out(hu_lp, net, wbd, exclude) %>%
    get_lp_hu_points(st_crs(wbd)) %>%
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
  
  return(list(lp = lp_points, na = na_points))
}

get_na_outlets_coords <- function(lp_points, net) {
  na_points <- lp_points$na
  
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
  
  select(na_outlet_coords, 
         COMID, REACHCODE, REACH_meas, 
         offset, HUC12 = hu12, LevelPathI)
}

get_in_list <- function(lp_points, net) {
  lp_points <- lp_points$lp
  
  lp_list <- unique(lp_points$lp)
  
  net <- select(net, COMID, LevelPathI, REACHCODE, 
                ToMeas, FromMeas, Hydroseq) %>%
    filter(LevelPathI %in% lp_list)
  
  in_list_fun <- function(lp_search, net, lp_points) {
    list(lp_search = lp_search,
         lp_geom = filter(net, LevelPathI == lp_search),
         hu_points = filter(lp_points, lp == lp_search))
  }
  
  lapply(lp_list, in_list_fun, net = net, lp_points = lp_points)
}

get_linked_points <- function(in_list, na_outlet_coords, cores, check_file) {
  
  if(file.exists(check_file)) {
    linked <- read_sf(check_file, "linked_points")
  } else {
    
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
  }
  return(linked)
}
