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
  distinct() %>%
  group_by(corrected_LevelPathI) %>%
  mutate(group_size = n()) %>%
  select(LevelPathI = corrected_LevelPathI, group_size) %>%
  distinct()
to_check <- no_intersect
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

# "180901030407" is labeled closed basin but the LA aquaduct flows through and connects to something downstream. Error in the NHD because the levelpath is continuous from a river to an aquaduct.

to_check <- ud_check(50008693, "toHUC or delineation of 170402060301 is problematic", FALSE)

to_check <- ud_check(350010387, "LevelPath 350010387 is a boundary between HUs (artificial path)", FALSE)

to_check <- ud_check(350006029, "broken at HU 080500030402", TRUE)
fix <- bind_rows(fix, list(HUC12 = "080500030402", 
                           TOHUC = "080500030403", 
                           comment = "misdirected"))

to_check <- ud_check(350005173, "one break at 080202030604", TRUE)
fix <- bind_rows(fix, list(HUC12 = "080202030604",
                           TOHUC = "080202030608",
                           comment = "misdirected"))

to_check <- ud_check(350005173, "misdirect at 080202031010", TRUE)
fix <- bind_rows(fix, list(HUC12 = "080202031010",
                           TOHUC = "080202031013",
                           comment = "misdirected"))

to_check <- ud_check(350005173, "misdirected at 080202030902", TRUE)
fix <- bind_rows(fix, list(HUC12 = "080202030902",
                           TOHUC = "080202031302",
                           comment = "misdirected"))

to_check <- ud_check(350034264, "misdirected at 080202031205", TRUE)
fix <- bind_rows(fix, list(HUC12 = "080202031205",
                           TOHUC = "080202031202", 
                           comment = "misdirected"))

to_check <- ud_check(150002450, "misdirected at 010300010510", TRUE)
fix <- bind_rows(fix, list(HUC12 = "010300010510",
                           TOHUC = "010300010603", 
                           comment = "misdirected"))

to_check <- ud_check(10004787, "error in nhd WBD OK", FALSE)

to_check <- ud_check(50007511, "Head HUC shouldn't be CLOSED BASIN?", TRUE)
fix <- bind_rows(fix, list(HUC12 = "170900040201",
                           TOHUC = "170900040202", 
                           comment = "misdirected"))

to_check <- ud_check(630008101, "Misdirected at 120500011102", TRUE)
fix <- bind_rows(fix, list(HUC12 = "120500011102",
                           TOHUC = "120500011301", 
                           comment = "misdirected"))
fix <- bind_rows(fix, list(HUC12 = "120500010805",
                           TOHUC = "120500010804", 
                           comment = "misdirected"))
fix <- bind_rows(fix, list(HUC12 = "120500010804",
                           TOHUC = "120500010906", 
                           comment = "misdirected"))
fix <- bind_rows(fix, list(HUC12 = "120500011005",
                           TOHUC = "120500011101", 
                           comment = "misdirected"))

to_check <- ud_check(350008531, "Misdirected at 080302071803 but also too many crazy things going on.", FALSE)
fix <- bind_rows(fix, list(HUC12 = "080302071803",
                           TOHUC = "080302090605", 
                           comment = "misdirected"))

to_check <- ud_check(50013727, "Problematic delineation of route-through / partially closed basin: 171200080305, partial fix", TRUE)
fix <- bind_rows(fix, list(HUC12 = "171200080301",
                           TOHUC = "171200080401", 
                           comment = "misdirected"))

to_check <- ud_check(10023632, "Problematic delineation of 180102041403 and others?", TRUE)
fix <- bind_rows(fix, list(HUC12 = "180102041405",
                           TOHUC = "180102041403", 
                           comment = "problem closed basin"))

fix <- bind_rows(fix, list(HUC12 = "180102041403",
                           TOHUC = "180102041003", 
                           comment = "problem delineation -- ideal fix would redelineate"))

fix <- bind_rows(fix, list(HUC12 = "180102041003",
                           TOHUC = "180102040904", 
                           comment = "problem closed basin"))

fix <- bind_rows(fix, list(HUC12 = "180102040904",
                           TOHUC = "180102040903", 
                           comment = "Misdirected?"))

fix <- bind_rows(fix, list(HUC12 = "180102040904",
                           TOHUC = "180102040902", 
                           comment = "Misdirected?"))

to_check <- ud_check(350025363, "WBD boundary along artificial path, no good solution other than redilineate.", FALSE)
to_check <- ud_check(350149594, "WBD boundary along artificial path, no good solution other than redelineate.", FALSE)

to_check <- ud_check(10014015, "Odd NHD levelpath, suggested HU12 reroute but tenuous.", TRUE)

fix <- bind_rows(fix, list(HUC12 = "180300090403",
                           TOHUC = "180300090503", 
                           comment = "Misdirected?"))

to_check <- ud_check(680013674, "NHD LevelPath is a problem artificial path", FALSE)

to_check <- ud_check(720025435, "Not enough NHD density to match", FALSE)

to_check <- ud_check(510021725, "Odd headwater overlap confounding issue in 070300050404, looks OK", FALSE)

to_check <- ud_check(510026951, "Odd headwater overlap confounding issue in 070200090602, looks OK", FALSE)

to_check <- ud_check(350006023, "Need to reroute 080202031010", TRUE)

to_check <- ud_check(350021104, "Misdirected upstream HUC 080302090101", TRUE)
fix <- bind_rows(fix, list(HUC12 = "080302090101",
                           TOHUC = "080302090201", 
                           comment = "Misdirected"))

to_check <- ud_check(590051154, "Looks fine, maybe redirect 100600030702?", FALSE)
fix <- bind_rows(fix, list(HUC12 = "100600030702",
                           TOHUC = "100600030704", 
                           comment = "Misdirected"))


to_check <- ud_check(350012700, "Messy, maybe redelineate?", FALSE)
fix <- bind_rows(fix, list(HUC12 = "080302070702",
                           TOHUC = "080302070703", 
                           comment = "Misdirected"))

to_check <- ud_check(50023103, "170200150602 not a closed basin in NHD", TRUE)
fix <- bind_rows(fix, list(HUC12 = "170200150602",
                           TOHUC = "170200150605", 
                           comment = "Not a closed basin"))


to_check <- ud_check(50047267, "170900080501 is misdirected to this path", TRUE)
fix <- bind_rows(fix, list(HUC12 = "170900080501",
                           TOHUC = "170900080504", 
                           comment = "Misdirected"))

to_check <- ud_check(150010299, "Lots of headwater overlaps -- one causing an issue between 010600030705 and 010600030704", FALSE)



readr::write_csv(fix, "hu_fixes.csv")
readr::write_csv(to_check, "levelpath_notes.csv")
