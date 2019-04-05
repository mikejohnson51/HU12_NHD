hu_joiner <- "out/map_joiner.csv"
if(file.exists(hu_joiner)) {
  message(hu_joiner, " exists, nothing to do.")
} else {
  load(process_cache)
  
  igraph::is.dag(igraph::graph_from_data_frame(wbd_atts))
  
  GNIS_terminals <- net_atts %>%
    select(GNIS_ID, TerminalPa) %>%
    filter(GNIS_ID != " ") %>%
    select(-GNIS_ID) %>%
    distinct() %>%
    left_join(select(net_atts, COMID, LevelPathI, Hydroseq), 
              by = c("TerminalPa" = "LevelPathI")) %>%
    filter(COMID %in% net_prep$COMID) %>%
    group_by(TerminalPa) %>%
    filter(Hydroseq == min(Hydroseq))
  
  library(snow)
  
  cl <- parallel::makeCluster(rep("localhost", cores), type = "SOCK", outfile = "job.log")
  
  to_run <- GNIS_terminals$COMID
  
  already_run <- list.files("cache/temp/", pattern = "*.rds")
  already_run <- as.integer(gsub(".rds", "", already_run))
  
  to_run <- to_run[!to_run %in% already_run]
  
  all_GNIS_outlets <- parLapply(cl, to_run, par_fun,
                                net_atts = net_atts,
                                net_prep = net_prep,
                                wbd_atts = wbd_atts)
  
  stopCluster(cl)
  
  out_files <- list.files("cache/temp/", full.names = TRUE)
  
  all <- sapply(out_files, readRDS, USE.NAMES = TRUE)
  
  names(all) <- gsub("cache/temp/", "", names(all))
  names(all) <- gsub(".rds", "", names(all))
  
  all <- brind_rows(all)
  
  readr::write_csv(all, hu_joiner)
  
  rm(all)
  rm(GNIS_terminals)
  rm(net_atts)
  rm(net_prep)
  rm(wbd_atts)
}
