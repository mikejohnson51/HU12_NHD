# todos: check for toHUCs that don't exist, 
# figure out why some HUs are getting attached to multiple levelpaths.
# Check out NULL matches


library(dplyr)
fix <- tibble(HUC12 = character(0), 
              TOHUC = character(0), 
              comment = character(0))
# Set found with HU12 aggregation code
fix <- bind_rows(fix, list(HUC12 = "090203160905", 
                           TOHUC = "090203160906", 
                           comment = "circular"))
fix <- bind_rows(fix, list(HUC12 = "031300130600", 
                           TOHUC = "OCEAN", 
                           comment = "circular"))
fix <- bind_rows(fix, list(HUC12 = "100500080502", 
                           TOHUC = "100500080504", 
                           comment = "circular"))
fix <- bind_rows(fix, list(HUC12 = "100500010301", 
                           TOHUC = "100500010302", 
                           comment = "circular"))

# Found durring development of levelpath matching routines
fix <- bind_rows(fix, list(HUC12 = "101900180706", 
                           TOHUC = "102001010305", 
                           comment = "Major route error"))

all <- readr::read_csv("map_joiner.csv")
no_intersect <- filter(all, trib_no_intersect) %>%
  group_by(LevelPathI) %>%
  mutate(group_size = n()) %>%
  select(LevelPathI, group_size) %>%
  distinct()
to_check <- filter(no_intersect, group_size > 1)
to_check$comment <- ""
to_check$fixed <- FALSE

ud_check <- function(lp, comment, fixed) {
  to_check$comment[to_check$LevelPathI == lp] <- comment
  to_check$fixed[to_check$LevelPathI == lp] <- fixed
  to_check
}

# Major routing errors found using levelpath matching algorithm
comment <- "current routing follows man-made waterbody?"
to_check <- ud_check(250019472, comment, TRUE)
fix <- bind_rows(fix, list(HUC12 = "030201040502", 
                           TOHUC = "030102050803", 
                           comment = comment))

comment <- "Strange delineation of 160300070309"
to_check <- ud_check(800045055, comment, FALSE)
fix <- bind_rows(fix, list(HUC12 = "160300070308", 
                           TOHUC = "160300070307", 
                           comment = comment))

comment <- "same as lp 800094099 Strange delineation of 160300070309"
to_check <- ud_check(800094099, comment, FALSE)

comment <- "WBD delineation is problematic around dominant artificial path lp# 350020229."
to_check <- ud_check(350020229, comment, FALSE)

to_check <- ud_check(350033560, comment, FALSE)

to_check <- ud_check(350002528, "080801010407 intersects mainstem and shouldn't", FALSE)

to_check <- ud_check(350006379, "strange delineations along rivers, see 080500030503", FALSE)

fix <- bind_rows(fix, list(HUC12 = "080500030503", 
                           TOHUC = "080500030504", 
                           comment = "better follow river path, could also redelineate to fix"))

to_check <- ud_check(590016507, "CLOSED BASIN error at 101701030209", FALSE)
fix <- bind_rows(fix, list(HUC12 = "101701030209", 
                           TOHUC = "101701030210", 
                           comment = "Shouldn't be CLOSED BASIN"))

readr::write_csv(fix, "hu_fixes.csv")
