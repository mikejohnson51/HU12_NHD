get_hu_joiner <- function(hu_joiner_file, net_prep, cores, temp_dir = "temp/") {
  
  wbd_atts <- net_prep$wbd_atts
  net_atts <- net_prep$net_atts
  net_prep <- net_prep$net_prep
  
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
  
  cl <- parallel::makeCluster(rep("localhost", cores), 
                              type = "SOCK", 
                              outfile = "job.log")
  
  to_run <- GNIS_terminals$COMID
  
  dir.create(temp_dir, showWarnings = FALSE)
  already_run <- list.files(temp_dir, pattern = "*.rds")
  already_run <- as.integer(gsub(".rds", "", already_run))
  
  to_run <- to_run[!to_run %in% already_run]
  
  all_GNIS_outlets <- parLapply(cl, to_run, par_fun,
                                net_atts = net_atts,
                                net_prep = net_prep,
                                wbd_atts = wbd_atts,
                                temp_dir = temp_dir)
  
  stopCluster(cl)
  
  out_files <- list.files(temp_dir, full.names = TRUE)
  
  all <- sapply(out_files, readRDS, USE.NAMES = TRUE)
  
  names(all) <- gsub(temp_dir, "", names(all))
  names(all) <- gsub(".rds", "", names(all))
  
  all <- bind_rows(all)
  
  readr::write_csv(all, hu_joiner_file)
  
  return(all)
}
