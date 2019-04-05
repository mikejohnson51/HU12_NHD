
if(file.exists(process_cache)) {
  
  message(paste("Found", process_cache, "nothing to do."))
  
} else {
  
  message("Loading WBD and NHD building cache.")
  wbd <- read_sf(natdb, "HUC12")
  wbd <- select(wbd, HUC12 = HUC_12, TOHUC = HU_12_DS)
  wbd <- filter(wbd, !grepl("^20.*|^19.*|^21.*|^22.*", wbd$HUC12))
  
  # # Check if what we have is a DAG.
  for(fix in 1:nrow(fixes)) {
    wbd$TOHUC[wbd$HUC12 == fixes$HUC12[fix]] <- fixes$TOHUC[fix]
    if(!igraph::is.dag(igraph::graph_from_data_frame(st_set_geometry(wbd, NULL)))) message(fix)
  }
  
  wbd <- st_transform(wbd, 5070)
  write_sf(wbd, "wbd_viz.gpkg", "wbd_viz")
  
  if(file.exists(net_cache)) {
    net <- readRDS(net_cache)
  } else {
    net <- read_sf(natdb, "NHDFlowline_Network") %>%
      st_zm()
    saveRDS(net, net_cache)
  }
  
  net_prep <- prepare_nhdplus(net, 0, 0, TRUE, TRUE) %>%
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
  
  net_atts <- st_set_geometry(net, NULL)[ ,1:40]
  rm(net)
  
  net_prep <- st_transform(net_prep, 5070)
  
  net_prep <- st_simplify(net_prep, 10)
  wbd <- st_simplify(wbd, 10)
  
  net_prep <- st_join(net_prep, select(wbd, HUC12, TOHUC)) %>%
    st_set_geometry(NULL)
  
  wbd_atts <- st_set_geometry(wbd, NULL)
  
  rm(wbd)
  rm(fix)
  rm(fixes)
  rm(natdb)
  
  save(net_atts, net_prep, wbd_atts, file = process_cache)
  
  rm(net_atts)
  rm(net_prep)
  rm(wbd_atts)
}
