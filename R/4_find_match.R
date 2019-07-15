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

get_hu_joiner <- function(net, wbd, simp, cores, temp_dir = "temp/", out_dir = "") {
  
  out_file <- file.path(out_dir, "map_joiner.csv")
  
  if(file.exists(out_file)) {
    all <- readr::read_csv(out_file)
  } else {
  
  unlink(temp_dir, recursive = TRUE)
  
  net_int <- get_process_data(net, wbd, simp)
  
  net <- st_set_geometry(net, NULL)[ ,1:40]
  wbd <- st_set_geometry(wbd, NULL)
  
  terminals <- net %>%
    select(TerminalPa) %>%
    distinct() %>%
    left_join(select(net, COMID, LevelPathI, Hydroseq), 
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
                           net_atts = net,
                           net_prep = net_int,
                           wbd_atts = wbd,
                           temp_dir = temp_dir)
  
  stopCluster(cl)
  
  out_files <- list.files(temp_dir, full.names = TRUE)
  
  all <- sapply(out_files, readRDS, USE.NAMES = TRUE)
  
  names(all) <- gsub(temp_dir, "", names(all))
  names(all) <- gsub(".rds", "", names(all))
  
  all <- bind_rows(all)
  
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  readr::write_csv(all, out_file)
  
  }
  
  return(all)
}

par_hr_pairs <- function(x, prj, nhdplus_hw_outlets) {
  cats <- sf::read_sf(x, "NHDPlusCatchment")
  cats <- sf::st_transform(cats, prj)
  cats <- nhdplusTools:::rename_nhdplus(cats)
  
  dplyr::filter(nhdplusTools:::get_hr_pair(nhdplus_hw_outlets, cats), 
                !is.na(FEATUREID))
}

get_hr_pairs <- function(nhdhr_path, nhdplus_hw_outlets, prj, cores) {
  gdb_files <- unlist(lapply(nhdhr_path, function(x) {
    list.files(x, pattern = ".*GDB.gdb$",
               full.names = TRUE, recursive = TRUE, include.dirs = TRUE)
  }))
  
  cl <- parallel::makeCluster(rep("localhost", cores), type = "SOCK")
  hr_pairs <- parallel::parLapply(cl, gdb_files, par_hr_pairs, prj = prj,
                                  nhdplus_hw_outlets = nhdplus_hw_outlets)
  dplyr::bind_rows(hr_pairs)
}
