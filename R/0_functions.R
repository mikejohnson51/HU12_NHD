par_fun <- function(start_comid, net_atts, net_prep, wbd_atts, temp_dir) {
  library(nhdplusTools)
  library(sf)
  library(dplyr)
  
  out_file <- paste0(temp_dir, start_comid, ".rds")
  
  if(!file.exists(out_file)) {
    
    message(paste(Sys.time(), out_file, "\n"))
    
    all_comid <- get_UT(net_atts, start_comid)
    
    sub_net <- filter(net_prep, COMID %in% all_comid)
    
    out <- list(NULL)
    
    if(nrow(sub_net) > 0) {
      sub_net <- bind_rows(sub_net,
                           select(filter(wbd_atts,
                                         !HUC12 %in% sub_net$HUC12),
                                  HUC12, TOHUC))
      
      out <- list(match_levelpaths(sub_net, start_comid, add_checks = TRUE))
    }
    
    saveRDS(out, out_file)
  }
  return(out_file)
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
    lp <- filter(net, LevelPathI == lp_id) %>%
      st_geometry()
    
    hu_ids <- filter(hu_lp, corrected_LevelPathI == lp_id)
    hus <- filter(wbd, HUC12 %in% hu_ids$HUC12)
    hus$Shape <- st_cast(hus$Shape, "MULTILINESTRING")
    
    out <- setNames(lapply(1:nrow(hus), 
                           function(i, lp, hus) {
                             suppressMessages(
                               st_intersection(lp, hus$Shape[i])
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
    linked <- get_flowline_index(lp_list$lp_geom, lp_list$hu_points, search_radius = 0.001) %>%
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

load_nhd <- function(natdb, net_cache) {
  message("loading NHD")
  
  if(file.exists(net_cache)) {
    net <- readRDS(net_cache)
  } else {
    net <- read_sf(natdb, "NHDFlowline_Network") %>%
      st_zm()
    saveRDS(net, net_cache)
  }
  return(net)
}
