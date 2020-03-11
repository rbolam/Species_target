## --------------------------- Merging all comp groups --------------------------------####

library(tidyverse)


## ------------------------------ Read in files, remove duplicate spp, and save new files ####

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



########## Stresses data

stresses <- threats %>% 
  filter(timing %in% c("Future", "Ongoing")) %>% 
  select(scientificName, code, name, stressName) %>% 
  separate(stressName, into = c("s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8"), sep = "[|]", fill = "right") %>% 
  gather(4:11, key = s, value = stress, na.rm = T) %>% 
  select(-s) %>% 
  unique() %>% 
  filter(stress != "")

## Convert level 3 stresses to level 2:
stresses$stress[stresses$stress %in% c("Hybridisation", "Competition", "Loss of mutualism", "Loss of pollinator", "Inbreeding", 
                                     "Skewed sex ratios", "Reduced reproductive success", "Other")] <- 
  c("Indirect species effects")

## Remove 2 spp which have level 1 stress listed:
stresses <- filter(stresses, !stress %in% c("Ecosystem stresses", "Species Stresses"))
stresses$stress <- as.factor(stresses$stress)
stresses$stress <- factor(stresses$stress, levels(stresses$stress)[c(1, 2, 3, 6, 5, 4)])

stresses %>% 
  group_by(code, name, stress) %>% 
  count() ->
  stresses
write_csv(stresses, "data/stresses.csv")







