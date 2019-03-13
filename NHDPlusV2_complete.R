library(nhdplusTools)
library(sf)
library(dplyr)
library(HUCAgg)

natdb <- "data/nhdplus/NHDPlusNationalData/NHDPlusV21_National_Seamless.gdb/"
st_layers(natdb)

wbd <- read_sf(natdb, "HUC12")

wbd <- select(wbd, HUC12 = HUC_12, TOHUC = HU_12_DS)

fixes <- readr::read_csv("hu_fixes.csv") %>%
  filter(HUC12 != "160300070308") %>% # Causes a loop in the old HUs.
  # more fixes for NHDPlusV2 version
  bind_rows(data.frame(HUC12 = c("101800100703", "060101051402", "160300020305",
                                 "170401050304", "101800110901", "101800120602",
                                 "100200070304", "060101051302", "170402030303",
                                 "100302030902", "180102040902"),
                       TOHUC = c("101800100702", "060101051404", "160300020306",
                                 "170401050306", "101800110902", "101800120603",
                                 "100200070307", "060101051303", "170402030305",
                                 "100302030901", "180102041201"), stringsAsFactors = F))

for(fix in 1:nrow(fixes)) {
  wbd$TOHUC[wbd$HUC12 == fixes$HUC12[fix]] <- fixes$TOHUC[fix]
}

wbd <- filter(wbd, !grepl("^20.*|^19.*|^21.*|^22.*", wbd$HUC12))
wbd <- st_transform(wbd, 5070)

# Used to identify problem HU toHUCs
#
fromHUC <-sapply(wbd$HUC12, fromHUC_finder,
                 hucs=wbd$HUC12, tohucs=wbd$TOHUC)

aggrHUC <- sapply(wbd$HUC12, HUC_aggregator, fromHUC = fromHUC)

net <- read_sf(natdb, "NHDFlowline_Network") %>%
  st_zm()

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
saveRDS(net_prep, "net_prep_cache_nhdp.rds")

net_prep <- readRDS("net_prep_cache_nhdp.rds")

wbd_atts <- st_set_geometry(wbd, NULL)

rm(wbd)
rm(fix)
rm(fixes)
rm(natdb)

save(net_atts, net_prep, wbd_atts, file = "nhd_wbd.RData")

rm(net_atts)
rm(net_prep)
rm(wbd_atts)

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

par_fun <- function(start_comid, net_atts, net_prep, wbd_atts) {
  library(nhdplusTools)
  library(sf)
  library(dplyr)
  
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
  }
  return(out_file)
}

library(snow)

cl <- parallel::makeCluster(rep("localhost", 4), type = "SOCK", outfile = "job.log")

to_run <- GNIS_terminals$COMID
already_run <- list.files("out/", pattern = "*.rds")
already_run <- as.integer(gsub(".rds", "", already_run))

to_run <- to_run[!to_run %in% already_run]

all_GNIS_outlets <- parLapply(cl, to_run, par_fun,
                              net_atts = net_atts,
                              net_prep = net_prep,
                              wbd_atts = wbd_atts)

all_GNIS_outlets <- lapply(to_run, par_fun,
                           net_atts = net_atts,
                           net_prep = net_prep,
                           wbd_atts = wbd_atts)
stopCluster(cl)

out_files <- list.files("out", full.names = TRUE)

all <- sapply(out_files, readRDS, USE.NAMES = TRUE)

names(all) <- gsub("out/", "", names(all))
names(all) <- gsub(".rds", "", names(all))

all <- do.call(rbind, all)

readr::write_csv(all, "map_joiner.csv")