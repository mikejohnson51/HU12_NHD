get_hu_joiner <- function(hu_joiner_file, net_atts, wbd_atts, 
                          net_int, cores, temp_dir = "temp/") {
  
  terminals <- net_atts %>%
    select(TerminalPa) %>%
    distinct() %>%
    left_join(select(net_atts, COMID, LevelPathI, Hydroseq), 
              by = c("TerminalPa" = "LevelPathI")) %>%
    filter(COMID %in% net_int$COMID) %>%
    group_by(TerminalPa) %>%
    filter(Hydroseq == min(Hydroseq))
  
  cl <- parallel::makeCluster(rep("localhost", cores), 
                              type = "SOCK", 
                              outfile = "hu_joiner.log")
  
  to_run <- terminals$COMID
  
  dir.create(temp_dir, showWarnings = FALSE)
  already_run <- list.files(temp_dir, pattern = "*.rds")
  already_run <- as.numeric(gsub(".rds", "", already_run))
  
  to_run <- to_run[!to_run %in% already_run]
  
  all_outlets <- parLapply(cl, to_run, par_fun,
                           net_atts = net_atts,
                           net_prep = net_int,
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
