## ------------------------------------------------------------------------------------####
## ---------------------------- Merging data ------------------------------------------####
## ------------------------------------------------------------------------------------####


library(tidyverse)


## ----------------- Comprehensively assessed groups - merge --------------------------####

folders <- list.files("data/rl_download_12_05_2020/") ## make list of files in folder
folders <- folders[2:4] # remove all other spp folder
summaries <- data.frame()
threats <- data.frame()
actions <- data.frame()
allother <- data.frame()

## ------------------- Load birds, corals and fw shrimp:-----------------------####
for(i in 1:length(folders)) {
  # Make df called summaries of all RL aummary files:
  summ <- read.csv(paste("data/rl_download_12_05_2020/", folders[i], "/simple_summary.csv", 
                         sep = ""), na.string = c("", "NA"))
  summaries <- bind_rows(summaries, summ)
  
  # Make df called threats of all RL threat files:
  thr <- read.csv(paste("data/rl_download_12_05_2020/", folders[i], "/threats.csv", 
                        sep = ""), na.string = c("", "NA"))
  threats <- bind_rows(threats, thr)
  
  # Make df called actions of all RL action files:
  act <- read.csv(paste("data/rl_download_12_05_2020/", folders[i], "/conservation_needed.csv", 
                        sep = ""), na.string = c("", "NA"))
  actions <- bind_rows(actions, act)
  
  # Make df called allother of all all_other_fields files:
  allo <- read.csv(paste("data/rl_download_12_05_2020/", folders[i], "/all_other_fields.csv", 
                        sep = ""), na.string = c("", "NA"))
  allo$GenerationLength.range <- as.character(allo$GenerationLength.range)
  allo$LocationsNumber.range <- as.character(allo$LocationsNumber.range)
  allother <- bind_rows(allother, allo)
}


## --------------- Load all other spp: --------------------------------------####
comp <- read.csv("data/comprehensive_groups.csv")

## Sep files for sep tax hierarchies:
ord <- filter(comp, tax1 == "Order")
cla <- filter(comp, tax1 == "Class")
fam <- filter(comp, tax1 %in% c("Family", "Families"))
gen <- filter(comp, tax1 == "Genus")

## Deal sep with tax groups where we need 2 levels:
comp2 <- filter(comp, !is.na(tax2))

## load in summary data:
summ <- read.csv("data/rl_download_12_05_2020/all_other_spp/simple_summary.csv", 
                 na.string = c("", "NA"))


## Retain those spp in first tax level:
spp <- filter(summ, orderName %in% ord$group1 | className %in% cla$group1 | 
                familyName %in% fam$group1 | genusName %in% gen$group1)
spp1 <- filter(spp, ! familyName %in% comp2$group1)

## Retain those spp in second tax level:
spp2 <- filter(spp, familyName %in% comp2$group1 & genusName %in% comp2$group2)

## Merge:
spp <- bind_rows(spp1, spp2)



## Count classes for checking of taxonomies
a <- count(summaries, className)
#write_csv(a, "data/count_tax.csv") ##pretty consistent!!


## Merge summaries/ threats and actions for all: -------------------------####

summaries <- bind_rows(summaries, spp)

## Threat data:
thr <- read.csv("data/rl_download_12_05_2020/all_other_spp/threats.csv", 
                      na.string = c("", "NA"))
thr <- filter(thr, scientificName %in% spp$scientificName)
threats <- bind_rows(threats, thr)


## Action data:
act <- read.csv("data/rl_download_12_05_2020/all_other_spp/conservation_needed.csv", 
                     na.string = c("", "NA"))
act <- filter(act, scientificName %in% spp$scientificName)
actions <- bind_rows(actions, act)


# all_other_fields data:
allo <- read.csv("data/rl_download_12_05_2020/all_other_spp/all_other_fields.csv", 
                 na.string = c("", "NA"))
allother <- bind_rows(allother, allo)



## Retain relevant RL categories and save files ---------------------------####
summariesf <- summaries %>% 
  filter(redlistCategory %in% c("Extinct in the Wild", "Critically Endangered", "Endangered", 
                                "Vulnerable")) %>% 
  unique()
write_csv(summariesf, "data/simple_summaries.csv")

threatsf <- threats %>% 
  filter(scientificName %in% summariesf$scientificName) %>% 
  unique()
write_csv(threatsf, "data/threats.csv")


actionsf <- actions %>% 
  filter(scientificName %in% summariesf$scientificName) %>% 
  unique()
write_csv(actionsf, "data/actions_needed.csv")


allotherf <- allother %>% 
  filter(scientificName %in% summariesf$scientificName) %>% 
  select(scientificName, PopulationSize.range) %>% 
  unique()
write_csv(allotherf, "data/all_other_fields.csv")



##---------------------------------- Threats and stresses data --------------------------####

threats <- read.csv("data/threats.csv")

## Sort out stresses data ----------------------------------####

stresses <- threats %>% 
  filter(timing %in% c("Future", "Ongoing")) %>% 
  select(scientificName, code, name, stressName) %>%
  separate_rows(stressName, sep = "[|]") %>% 
  filter(!is.na(stressName))


## Convert level 3 stresses to level 2:
stresses$stressName[stresses$stressName %in% c("Hybridisation", "Competition", 
                                               "Loss of mutualism", "Loss of pollinator", 
                                               "Inbreeding", "Skewed sex ratios", 
                                               "Reduced reproductive success", "Other")] <- 
  c("Indirect species effects")


stresses$stressName[stresses$stressName %in% c("Ecosystem conversion", "Ecosystem degradation", 
                                       "Indirect ecosystem effects")] <- c("Ecosystem stresses")



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


## Merge spp with target matching, and save file
match <- read.csv("data/thr_str_tar_matched.csv")
match$thr_lev2 <- as.character(match$thr_lev2)

stresses <- left_join(stresses, match, by = c("thr_lev2", "stressName", "thr_lev2name"))
write_csv(stresses, "data/spp_tar.csv")

