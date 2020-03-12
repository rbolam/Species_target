## ------------------------------------------------------------------------------------------------------------####
## ---------------------------------------------------- Merging data ------------------------------------------####
## ------------------------------------------------------------------------------------------------------------####


library(tidyverse)


## ------------------------------ Comprehensively assessed groups - merge -------------------------------------####

folders <- list.files("data/rl_download_02_03_2020/") ## make list of files in folder
summaries <- data.frame()
threats <- data.frame()
actions <- data.frame()


for(i in 1:length(folders)) {
  summ <- read.csv(paste("data/rl_download_02_03_2020/", folders[i], "/simple_summary.csv", sep = ""), na.string = c("", "NA"))
  summaries <- bind_rows(summaries, summ)
  thr <- read.csv(paste("data/rl_download_02_03_2020/", folders[i], "/threats.csv", sep = ""), na.string = c("", "NA"))
  threats <- bind_rows(threats, thr)
  act <- read.csv(paste("data/rl_download_02_03_2020/", folders[i], "/conservation_needed.csv", sep = ""), na.string = c("", "NA"))
  actions <- bind_rows(actions, act)
}


summaries <- summaries %>% 
  filter(redlistCategory %in% c("Critically Endangered", "Endangered", "Vulnerable", "Near Threatened")) %>% 
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


## Sort out stresses data ----------------------------------####

stresses <- threats %>% 
  filter(timing %in% c("Future", "Ongoing")) %>% 
  select(scientificName, code, name, stressName) %>% 
  separate(stressName, into = c("s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8"), sep = "[|]", fill = "right") %>% 
  gather(4:11, key = s, value = stress, na.rm = T) %>% 
  select(-s) %>% 
  unique() %>% 
  filter(stress != "")

## Convert level 3 stresses to level 2:
stresses$stress[stresses$stress %in% c("Hybridisation", "Competition", "Loss of mutualism", "Loss of pollinator", 
                                       "Inbreeding", "Skewed sex ratios", "Reduced reproductive success", "Other")] <- 
  c("Indirect species effects")

stresses$stress[stresses$stress %in% c("Ecosystem conversion", "Ecosystem degradation", "Indirect ecosystem effects")] <- 
  c("Ecosystem stresses")

## Remove 1 spp which has level 1 stress listed:
stresses <- filter(stresses, stress != "Species Stresses")



## Sort out threats data ----------------------------------####

stresses <- stresses %>% 
  separate(col = code, into = c("T1", "T2", "T3"), sep = "[.]") %>% 
  select(-T3, -name) %>% 
  unite(thr_lev2, T1:T2, sep = ".")

## Load in key for threat 2 levels and names:
thr_lev2 <- read.csv("data/threats_level2.csv")
thr_lev2$thr_lev2 <- as.character(thr_lev2$thr_lev2)

stresses <- left_join(stresses, thr_lev2, by = "thr_lev2")

## Remove duplicate spp (due to removing lev 3 threats and stresses):
stresses <- unique(stresses)


## Save file with all spp, their threats and stresses:
write_csv(stresses, "spp_thr_str.csv")


## Count threats/stress and save file ---------------------####
stresses %>% 
  group_by(thr_lev2, thr2_name, stress) %>% 
  count() ->
  stresses

write_csv(stresses, "data/stresses.csv")


