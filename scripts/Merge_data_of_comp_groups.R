## ------------------------------------------------------------------------------------####
## ---------------------------- Merging data ------------------------------------------####
## ------------------------------------------------------------------------------------####


library(tidyverse)


## ----------------- Comprehensively assessed groups - merge --------------------------####

folders <- list.files("data/rl_download_02_03_2020/") ## make list of files in folder
summaries <- data.frame()
threats <- data.frame()
actions <- data.frame()


for(i in 1:length(folders)) {
  # Make df called summaries of all RL aummary files:
  summ <- read.csv(paste("data/rl_download_02_03_2020/", folders[i], "/simple_summary.csv", 
                         sep = ""), na.string = c("", "NA"))
  summaries <- bind_rows(summaries, summ)
  
  # Make df called threats of all RL threat files:
  thr <- read.csv(paste("data/rl_download_02_03_2020/", folders[i], "/threats.csv", 
                        sep = ""), na.string = c("", "NA"))
  threats <- bind_rows(threats, thr)
  
  # Make df called actions of all RL action files:
  act <- read.csv(paste("data/rl_download_02_03_2020/", folders[i], "/conservation_needed.csv", 
                        sep = ""), na.string = c("", "NA"))
  actions <- bind_rows(actions, act)
}


## Remove some bony fish genera ------------------------####

summaries <- filter(summaries, !genusName %in% c("Hydrophis", "Aipysurus", "Emydocephalus", 
                                                 "Hydrelaps", "Laticauda"))


## Retain releveant RL categories and save files -----####
summaries <- summaries %>% 
  filter(redlistCategory %in% c("Extinct in the Wild", "Critically Endangered", "Endangered", 
                                "Vulnerable")) %>% 
  unique()
write_csv(summaries, "data/simple_summaries.csv")

threats <- threats %>% 
  filter(scientificName %in% summaries$scientificName) %>% 
  unique()
write_csv(threats, "data/threats.csv")


actions <- actions %>% 
  filter(scientificName %in% summaries$scientificName) %>% 
  unique()
write_csv(actions, "data/actions_needed.csv")



##---------------------------------- Threats and stresses data --------------------------####

threats <- read.csv("data/threats.csv")

## Sort out stresses data ----------------------------------####

stresses <- threats %>% 
  filter(timing %in% c("Future", "Ongoing")) %>% 
  select(scientificName, code, name, stressName) %>% 
  
  # Turn col stressName into 8 cols, so each one is in a separate col:
  separate(stressName, into = c("s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8"), sep = "[|]", 
           fill = "right") %>% 
  
  # Make 2 cols so each stress is listed in col stress:
  gather(4:11, key = s, value = stress, na.rm = T) %>% 
  select(-s) %>% 
  unique() %>% 
  filter(stress != "")


## Convert level 3 stresses to level 2:
stresses$stress[stresses$stress %in% c("Hybridisation", "Competition", "Loss of mutualism", 
                                       "Loss of pollinator", "Inbreeding", "Skewed sex ratios", 
                                       "Reduced reproductive success", "Other")] <- 
  c("Indirect species effects")


stresses$stress[stresses$stress %in% c("Ecosystem conversion", "Ecosystem degradation", 
                                       "Indirect ecosystem effects")] <- c("Ecosystem stresses")


## Remove 1 spp which has only level 1 stress listed:
stresses <- filter(stresses, stress != "Species Stresses")



## Sort out threats data ----------------------------------####

stresses <- stresses %>% 
  # Remove level 3 threats and turn into level 2:
  separate(col = code, into = c("T1", "T2", "T3"), sep = "[.]") %>% 
  select(-T3, -name) %>% 
  unite(thr_lev2, T1:T2, sep = ".")


## Load in key for threat levels and names:
thr_lev <- read.csv("data/threat_levels.csv")
thr_lev$thr_lev2 <- as.character(thr_lev$thr_lev2)

stresses <- left_join(stresses, thr_lev, by = "thr_lev2")

## Remove duplicate spp (due to removing lev 3 threats and stresses):
stresses <- unique(stresses)


## Save file with all spp, their threats and stresses:
write_csv(stresses, "data/spp_thr_str.csv")


## Count threats/stress, merge with matching, and save file ---------------------####
stresses %>% 
  group_by(thr_lev2, thr2_name, stress) %>% 
  count() ->
  stresscount

match <- read.csv("data/thr_str_tar_matched.csv")
match$thr_lev2 <- as.character(match$thr_lev2)

stresscount <- full_join(stresscount, match, by = c("thr_lev2", "stress", "thr2_name"))
write_csv(stresscount, "data/stresses.csv")


spp <- read.csv("data/spp_thr_str.csv")
match <- read.csv("data/thr_str_tar_matched.csv")

spp %>% 
  left_join(match, by = c("thr_lev2", "stress", "thr2_name")) %>% 
  select(scientificName, target) %>% 
  unique() %>% 
  count(target) %>% 
  mutate(n = n/9750*100)

spp %>% 
  left_join(match, by = c("thr_lev2", "stress", "thr2_name")) %>% 
  filter(!is.na(target)) %>% 
  select(scientificName) %>% 
  unique() ->
  spp2

spp %>% 
  left_join(match, by = c("thr_lev2", "stress", "thr2_name")) %>% 
  filter(!scientificName %in% spp2$scientificName) %>% 
  select(scientificName) %>% 
  unique() %>% 
  nrow()
