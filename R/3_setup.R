rename_hr_fl = function(hr_vpus) {
  new_names <- gsub("NHDPLUS_H_", "", gsub("_HU4_GDB", "", hr_vpus))
  
  if(!all(new_names == hr_vpus)) {
    for(i in seq_along(hr_vpus)) {
      file.rename(hr_vpus[i], new_names[i])
    }
  }
  
  return(new_names)
}

prep_nhdplushr <- function(hr_fline) {
  hr_fline <- left_join(st_set_geometry(hr_fline, NULL), 
                        select(st_set_geometry(hr_fline, NULL),
                               toCOMID = .data$NHDPlusID,
                               .data$FromNode),
                        by = c("ToNode" = "FromNode"))
  
  hr_fline$TerminalFl[which(is.na(hr_fline$toCOMID))] <- 1
  
  hr_fline <- prepare_nhdplus(hr_fline, 
                                min_network_size = 0, 
                                min_path_length = 0, 
                                min_path_size = 0, 
                                purge_non_dendritic = TRUE) %>%
    select(ID = COMID, toID = toCOMID, length = LENGTHKM) %>%
    left_join(select(hr_fline,
                     ID = NHDPlusID, area = AreaSqKm, nameID = GNIS_ID),
              by = "ID") %>%
    distinct()
  
  hr_fline["weight"] <- nhdplusTools::calculate_arbolate_sum(select(hr_fline, ID, toID, length))
  
  hr_fline <- left_join(hr_fline, 
                          nhdplusTools::calculate_levelpaths(hr_fline), 
                          by = "ID")
  
  hr_fline <- left_join(hr_fline, 
                          select(hr_fline, ID, 
                                 down_topo_sort = topo_sort,
                                 down_levelpath = levelpath), 
                          by = c("toID" = "ID"))
  
  
  select(hr_fline, NHDPlusID = ID, 
         LevelPathI = levelpath, 
         DnLevelPat = down_levelpath, 
         DnHydroSeq = down_topo_sort, 
         HydroSeq = topo_sort)
}


get_net <- function(net, prj) {
  
  if("NHDPlusID" %in% names(net)) {
    net <- rename(net, COMID = NHDPlusID, LENGTHKM = LengthKM, FTYPE = FType, 
                  TotDASqKM = TotDASqKm, Hydroseq = HydroSeq, Pathlength = PathLength,
                  AreaSqKM = AreaSqKm, DnHydroseq = DnHydroSeq)
    net$TerminalFl[which(!net$ToNode %in% net$FromNode)] <- 1
  }
  return(net %>%
           st_zm() %>%
           st_transform(prj)
  )
}

get_wbd <- function(wbd_gdb, fixes, prj) {
  wbd <- tryCatch(read_sf(wbd_gdb, "HUC12"), 
                  error = function(e) read_sf(wbd_gdb, "WBDHU12"))
  
  if("HUC_12" %in% names(wbd)) {
    wbd <- select(wbd, HUC12 = HUC_12, TOHUC = HU_12_DS)
  } else {
    wbd <- select(wbd, HUC12, TOHUC)
  }
  
  wbd <- filter(wbd, !grepl("^20.*|^19.*|^21.*|^22.*", wbd$HUC12))
  
  if(!is.null(fixes)) {
    # # Check if what we have is a DAG.
    for(fix in 1:nrow(fixes)) {
      fix_huc <- fixes$HUC12[fix]
      fix_tohuc <- fixes$TOHUC[fix]
      wbd_tohuc <- wbd$TOHUC[wbd$HUC12 == fix_huc]
      if(fix_tohuc != wbd_tohuc) {
        wbd$TOHUC[wbd$HUC12 == fix_huc] <- fix_tohuc
      } else {
        print(paste("unneeded fix", fix_huc))
      }
    }
  }
  
  if(any(wbd$HUC12 == wbd$TOHUC)) {
    error(paste("Some HUCs go to themselves.",  paste(wbd$HUC12[wbd$HUC12 == wbd$TOHUC], collapse = ", ")))
  }
  
  if(!igraph::is.dag(igraph::graph_from_data_frame(st_set_geometry(wbd, NULL)))) {
    g <- igraph::graph_from_data_frame(st_set_geometry(wbd, NULL))
    warning(paste("need to make HUCs into a DAG. Running accumulation to find them."))
    
    library(HUCAgg)
    fromHUC <-sapply(wbd$HUC12, fromHUC_finder,
                     hucs=wbd$HUC12, tohucs=wbd$TOHUC)
    
    aggrHUC <- sapply(wbd$HUC12, HUC_aggregator, fromHUC = fromHUC)
    message("investigate those and try again.")
  }
  
  wbd <- st_transform(wbd, prj)
  
  return(wbd)
}

prep_net <- function(net, simp) {
  
  net_prep <- prepare_nhdplus(net, 
                              min_network_size = 20, # sqkm
                              min_path_length = 0, # sqkm
                              min_path_size = 10, # sqkm
                              purge_non_dendritic = TRUE,
                              warn =  TRUE) %>%
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
  
  net_prep <- st_simplify(net_prep, dTolerance = simp)
  
  return(net_prep)
}

get_process_data <- function(net, wbd, simp) {
  
  net_prep <- prep_net(net, simp)
  
  wbd <- select(st_simplify(wbd, dTolerance = simp), HUC12, TOHUC)
  
  net_prep <- st_join(net_prep, wbd) %>%
    st_set_geometry(NULL)
  
  return(net_prep)
}

# Not used?
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

clean_rf1 <- function(rf1) {
  from_to <- st_set_geometry(rf1, NULL) %>%
    filter(!TYPE %in% c("C", "G", "I", "L", "W", "X", "Z", "N")) %>%
    select(ID = ERF1_2., fnode_temp = FNODE_, tnode= TNODE_, name = PNAME, div_fraction = FRAC) %>%
    group_by(fnode_temp) %>%
    mutate(fnode = ifelse(div_fraction == max(div_fraction), fnode_temp, NA)) %>%
    ungroup() %>%
    select(-fnode_temp, -div_fraction)
  
  left_join(from_to, select(from_to, toID = ID, fnode), by = c("tnode" = "fnode")) %>%
    select(-fnode, -tnode) %>%
    left_join(select(rf1, ID = ERF1_2.), by = "ID") %>%
    st_sf() 
}

