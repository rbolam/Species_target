## --------------------------------------------------------------------------------------------##
## ----------------------- Counting different species groups --------------------------------####
## --------------------------------------------------------------------------------------------##

library(tidyverse)
library(gridExtra)

summaries <- read.csv("data/simple_summaries.csv")
nspp <- summaries %>% select(scientificName) %>% unique() %>% nrow()
nspp / 36602 *100  ## calculate % of all spp that are threatened + EW

summaries %>% count(className) ## count no in each class


## ----------------------- Counts threats, targets, actions --------------------------------####


## Percent of spp with at least one threat listed:
thr <- read.csv("data/threats.csv")
thr %>% 
  select(scientificName) %>% 
  unique() %>% 
  nrow() / nrow(summaries) * 100


## Percent of threats with stresses listed:
thr %>% 
  select(scientificName, code, stressCode) %>% 
  unique() %>% 
  #nrow() 
  filter(is.na(stressCode)) %>% 
  nrow()
(33984 - 1003) / 33984 * 100


## Count spp threatened by each threat (level 1):
thr_str <- read.csv("data/spp_tar.csv")
thr_str %>% 
  select(scientificName, thr_lev1name) %>% 
  filter(thr_lev1name != "Geological events") %>% ## remove Geol events as can't be mitigated
  unique() %>% 
  count(thr_lev1name) %>% 
  mutate(perc1 = n / nspp * 100) %>% ## calculate % of threatened/EW spp
  mutate(perc2 = n / 36602 * 100) %>% ## calculate % of all spp
  arrange(-n)


## Percent/Count spp benefiting from each target:
thr_str %>% 
  filter(thr_lev1name != "Geological events") %>% ## remove Geol events as can't be mitigated
  select(scientificName, target) %>% 
  unique() %>% 
  count(target) %>% 
  mutate(perc1 = n / nspp * 100) %>% ## calculate % of threatened/EW spp
  mutate(perc2 = n / 36602 * 100) %>% ## calculate % of all spp
  arrange(-n)


## Filter spp benefiting from target 3:
target3 <- thr_str %>% 
  filter(target == "Target 3") %>% 
  select(scientificName) %>% 
  unique()

## Percent of spp benefitting from different actions:
act <- read.csv("data/actions_needed_tidy.csv")
act %>% 
  select(scientificName, name) %>% 
  unique() %>% 
  count(name) %>% 
  mutate(perc1 = n / nspp * 100) %>% ## calculate % of threatened/EW spp
  mutate(perc2 = n / 36602 * 100) %>% ## calculate % of all spp
  arrange(-n)




## -------------- Identifying spp which need captivity until solutions found -----------------####


## Count corals threatened by temperature extremes:

thr_str <- read.csv("data/spp_tar.csv")
thr_str %>%  
  left_join(summaries, by = "scientificName") %>% 
  filter(phylumName == "CNIDARIA" & thr_lev2 == "11.3") %>% 
  select(scientificName) %>% 
  unique() %>% 
  nrow()


## Count amphibians threatened by chytrid disease:
threats <- read.csv("data/threats.csv")
threats %>% 
  left_join(summaries, by = "scientificName") %>% 
  filter(className == "AMPHIBIA" & ias == "Batrachochytrium dendrobatidis") %>% 
  select(scientificName) %>% 
  unique() %>% 
  nrow()




## ------------------- Identifying spp which need emergency actions ------------------------####


## Species with <= 1000 individuals:

mature <- read.csv("data/all_other_fields.csv", na.string = c("", "U", "NA"))
mature <- filter(mature, !is.na(PopulationSize.range))

## Remove median values:
mature <- separate(mature, PopulationSize.range, sep = ",", into = "PopulationSize.range")

## Make col of low and high pop estimate:
mature <- separate(mature, PopulationSize.range, sep = "-", into = c("low", "high"))
mature$low <- as.numeric(mature$low)

summaries <- read.csv("data/simple_summaries.csv")
summaries <- full_join(summaries, mature, by = "scientificName")

mature <- summaries %>% 
  filter(low <= 1000) %>% 
  select(scientificName, redlistCategory, low) %>% 
  rename(min_population = low) %>% 
  unique()


## Species which fit certain criteria:

summaries <- read.csv("data/simple_summaries.csv")
summaries <- summaries %>% 
  select(scientificName, kingdomName, phylumName, className, redlistCategory, 
         redlistCriteria) %>% 
  separate_rows(redlistCriteria, sep = ";") %>% 
  filter(!is.na(redlistCriteria))

## Remove empty spaces:

summaries$redlistCriteria <- str_squish(summaries$redlistCriteria)

## Add columns that extract criteria:

summaries$C <- str_detect(summaries$redlistCriteria, "C") ## any that contain C
summaries$C2ai <- str_detect(summaries$redlistCriteria, "C2a\\(i[^ii]") ##any that contain C2a(i,
## but not C2a(ii
summaries$D <- str_detect(summaries$redlistCriteria, "^D$") ##any exact matches to D
summaries$D1 <- str_detect(summaries$redlistCriteria, "^D1$") ##any exact matches to D1
summaries$Bac <- str_detect(summaries$redlistCriteria, "^B.a.*?c") ##any that start with B, 
## followed by any character, followed by a. *?c means anything in between, then c

suma <- summaries

suma <- filter(suma, Bac == TRUE | C == TRUE & redlistCategory == "Critically Endangered" |
                 C2ai == TRUE | D1 == TRUE & redlistCategory == "Vulnerable" |
                 D == TRUE & redlistCategory %in% c("Critically Endangered", "Endangered"))

## Spp listed under Bac for checking:

suma %>% 
  filter(Bac == TRUE) %>% 
  select(scientificName, redlistCategory, redlistCriteria)


## Manual check of which spp to retain:

sppretain <- c("Turnix olivii", "Nothofagus alessandrii", "Pteropus rodricensis", 
               "Aproteles bulmerae", "Otomys barbouri", "Lepus flavigularis", "Capensibufo rosei",
               "Arthroleptella rugosa", "Pseudomys australis", "Urocitellus endemicus")
sppremove <- c("Acrocephalus familiaris", "Anas laysanensis", "Telespiza ultima", "Alauda razae",
               "Mimus trifasciatus", "Megadyptes antipodes", "Spheniscus mendiculus", 
               "Montipora dilatata", "Pediocactus paradinei", "Peromyscus dickeyi", 
               "Mannophryne cordilleriana", "Peromyscus stephani", "Mammillaria schwarzii",
               "Pediocactus knowltonii", "Sylvilagus robustus")

suma <- suma %>% 
  filter(!scientificName %in% sppremove) %>% 
  select(scientificName, redlistCategory) %>% 
  unique()


## -------------------------- Merge all relevant spp for target 3 ------------------####

## Spp with criteria + spp with mature individuals <= 1000:
suma <- full_join(suma, mature, by = c("scientificName", "redlistCategory")) 
# 998 obs + 1036 obs - overlap of 687 = 1347


## EW species:
suma %>% filter(redlistCategory == "Extinct in the Wild") %>% nrow()

summaries <- read.csv("data/simple_summaries.csv")
EW <- summaries %>% 
  filter(redlistCategory == "Extinct in the Wild") %>% 
  select(scientificName, redlistCategory) # 15 EW spp

suma <- suma %>% 
  full_join(EW, by = c("scientificName", "redlistCategory")) %>% 
  select(scientificName, redlistCategory, min_population) %>% 
  unique()


## Spp which need actions not covered by other targets:
suma <- full_join(suma, target3, by = "scientificName")
# 1357 + 1971 needing target 3 - 455 overlap = 2873

## Percent of spp that need target 3, of all threatened + EW spp:
suma %>% nrow() / nspp * 100

## Percent of spp that need emergency actions, of all spp:
suma %>% nrow() / 36602 * 100


