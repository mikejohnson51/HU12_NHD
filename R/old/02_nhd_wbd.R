library(nhdplusTools)
library(sf)
library(dplyr)

load("nhd_wbd.RData")

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

par_fun <- function(start_comid, net_atts, net_prep, wbd_atts, log_file) {
  library(nhdplusTools)
  library(sf)
  library(dplyr)
  
  # con <- file(log_file, open = "wt")
  # sink(con, type = "output", append = TRUE)
  # sink(con, type = "message", append = TRUE)
  
  out_file <- paste0("out/", start_comid, ".rds")
  
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
    # sink(type = "output")
    # sink(type = "message")
  }
  return(out_file)
}

library(snow)

cl <- parallel::makeCluster(rep("localhost", 4), type = "SOCK")

to_run <- GNIS_terminals$COMID
already_run <- list.files("out/", pattern = "*.rds")
already_run <- as.integer(gsub(".rds", "", already_run))

to_run <- to_run[!to_run %in% already_run]

# all_GNIS_outlets <- lapply(to_run, par_fun,
#                            net_atts = net_atts,
#                            net_prep = net_prep,
#                            wbd_atts = wbd_atts)

all_GNIS_outlets <- parLapply(cl, to_run, par_fun,
                              net_atts = net_atts,
                              net_prep = net_prep,
                              wbd_atts = wbd_atts,
                              log_file = "par2.log")

stopCluster(cl)

out_files <- list.files("out", full.names = TRUE)

all <- sapply(out_files, readRDS, USE.NAMES = TRUE)

names(all) <- gsub("out/", "", names(all))
names(all) <- gsub(".rds", "", names(all))

all <- do.call(rbind, all)

readr::write_csv(all, "map_joiner.csv")
