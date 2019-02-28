library(sf)
library(dplyr)

huc12 <- readr::read_csv("map_joiner.csv") %>%
  rename(levelpath = corrected_LevelPathI, head_huc12 = head_HUC12, outlet_huc12 = outlet_HUC12) %>%
  select(-HUC12, -TOHUC, -intersected_LevelPathI)

bad_lps <- select(huc12, levelpath, trib_no_intersect, headwater_error) %>%
  filter(trib_no_intersect | headwater_error)

huc12 <- select(huc12, -trib_intersect) %>%
  filter(!levelpath %in% bad_lps$levelpath & !is.na(outlet_huc12)) %>%
  distinct()

doops <- filter(huc12, levelpath %in% huc12$levelpath[duplicated(huc12$levelpath)])

LPs <- unique(huc12$levelpath)

load("nhd_wbd.RData")
rm(net_prep)

NHDPlus_heads <- net_atts %>%
  select(GNIS_ID, Hydroseq, levelpath = LevelPathI, reachcode = REACHCODE) %>%
  group_by(levelpath)
  
NHDPlus_outlets <- NHDPlus_heads %>%
  filter(Hydroseq == min(Hydroseq)) %>%
  ungroup() %>% 
  select(-Hydroseq, head_gnis = GNIS_ID, head_reachcode = reachcode)

NHDPlus_heads <- NHDPlus_heads %>%
  filter(Hydroseq == max(Hydroseq)) %>%
  ungroup() %>% 
  select(-Hydroseq, outlet_gnis = GNIS_ID, outlet_reachcode = reachcode)

LP <- data.frame(levelpath = LPs) %>%
  left_join(huc12, by = "levelpath") %>%
  left_join(NHDPlus_heads, by = "levelpath") %>%
  left_join(NHDPlus_outlets, by = "levelpath") %>%
  distinct()

readr::write_csv(LP, "main_stems.csv")

  


