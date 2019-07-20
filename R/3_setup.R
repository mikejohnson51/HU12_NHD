nhdhr_mod <- function(nhdhr_path, out_gpkg, force_terminal = FALSE) {
  
  if(!file.exists(out_gpkg)){
    
    hr_gpkg <- get_nhdplushr(nhdhr_path, 
                             file.path(tempdir(), "temp.gpkg"), 
                             layers = "NHDFlowline")
    
    layer <- read_sf(hr_gpkg, "NHDFlowline")
    
    terminals <- filter(layer, TerminalFl == 1)
    
    terminal_test <- layer$TerminalPa %in% terminals$TerminalPa
    
    warning(paste("Removing", sum(!terminal_test), 
                  "flowlines that are missing terminal paths."))
    
    layer <- filter(layer, terminal_test)
    
    layer <- nhdplusTools:::rename_nhdplus(layer)
    
    layer <- filter(layer, FTYPE != 566 & TerminalFl != 1)
    
    if(force_terminal) {
      t_atts <- select(st_set_geometry(layer, NULL), COMID, ToNode, FromNode, TerminalFl)
      
      t_atts <- left_join(t_atts, select(t_atts,
                                         toCOMID = COMID,
                                         FromNode),
                          by = c("ToNode" = "FromNode"))
      
      na_t_atts <- filter(t_atts, is.na(t_atts$toCOMID) & TerminalFl == 0) 
      
      warning(paste("Found", nrow(na_t_atts), "broken outlets where no toNode and not terminal. Fixing."))
      
      layer$TerminalFl[which(layer$COMID %in% na_t_atts$COMID)] <- 1
      
      # out <- filter(layer, COMID %in% na_t_atts$COMID)
      # 
      # write_sf(out, "./bad.gpkg")
    }
    
    write_sf(layer, layer = "NHDFlowline", dsn = out_gpkg)
  } else {
    layer <- read_sf(layer, layer = "NHDPlowline")
  }
  return(layer)
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

filter_vaa <- function(nhdhr_vaa, filter_spec) {
  huid <- substr(nhdhr_vaa$VPUID, 1, 2)
  filter(nhdhr_vaa, huid %in% filter_spec)
}