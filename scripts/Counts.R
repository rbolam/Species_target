## --------------------------------------------------------------------------------------------##
## ----------------------- Counting different species groups --------------------------------####
## --------------------------------------------------------------------------------------------##

library(tidyverse)
library(gridExtra)
library(viridis)

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



## Count spp threatened by each threat (level 1):
thr_str <- read.csv("data/spp_tar.csv")
thr_str %>% 
  select(scientificName, thr_lev1name) %>% 
  filter(thr_lev1name != "Geological events") %>% #remove 141 spp with Geol events (no mitigation)
  unique() %>% 
  count(thr_lev1name) %>% 
  mutate(perc1 = n / nspp * 100) %>% ## calculate % of threatened/EW spp
  mutate(perc2 = n / 36602 * 100) %>% ## calculate % of all spp
  arrange(-n)

thr_str$thr_lev1name <- as.factor(thr_str$thr_lev1name)
thr_str$thr_lev1name <- factor(thr_str$thr_lev1name, levels(thr_str$thr_lev1name)
                               [c(2, 1, 11, 7, 8, 10, 3, 12, 6, 4, 5, 9)])
thr_str %>% 
  select(scientificName, thr_lev1name) %>% 
  unique() %>% 
  ggplot(aes(x = fct_rev(fct_infreq(thr_lev1name)), fill = thr_lev1name)) +
  geom_bar() +
  coord_flip() +
  scale_fill_viridis_d(option = "B") +
  scale_y_continuous(expand = c(0.004, 0)) + 
  theme_classic() +
  labs(x = "Threats", y = "Number of species") +
  theme(legend.position = "none",
        text = element_text(size = 6.5))
ggsave("figures/supp_threat_figure.png", height = 8, width = 10, dpi = 300, unit = "cm")

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


## --------------------------- Species that need actions addressed by target 3 -------------####

actions <- read.csv("data/actions_needed_tidy.csv")
actions_tar3 <- actions %>% 
  filter(name %in% c("Ex-situ conservation", "Species re-introduction", "Species recovery")) %>% 
  select(scientificName) %>% 
  unique()

actions_tar3$actions <- "yes"

ex_situ <- actions %>% 
  filter(name %in% c("Ex-situ conservation")) %>% 
  select(scientificName) %>% 
  unique()
write_csv(ex_situ, "data/spp_needing_exsitu.csv")


## ---------------------- Species that face threats not addressed by other targets ----------####


thr_match <- read.csv("data/spp_tar.csv")
thr_match <- thr_match %>% 
  filter(target == "Target 3") %>% 
  select(scientificName) %>% 
  unique()

thr_match$other_threats <- "yes"

## --------------------------------- Species with <= 1000 individuals ----------------------####

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
  select(scientificName) %>% 
  unique()
suma$smallpop <- "yes"



## -------------------------- Merge all relevant spp for target 3 ------------------####


## Spp which need specific actions:
nrow(actions_tar3)


## Spp which have threats not addressed by other targets:
nrow(thr_match)


## Spp with mature individuals <= 1000:
nrow(suma)


summaries <- read.csv("data/simple_summaries.csv")


summaries <- summaries %>% 
  select(scientificName, redlistCategory) %>% 
  left_join(actions_tar3, by = "scientificName") %>% 
  left_join(thr_match, by = "scientificName") %>% 
  left_join(suma, by = "scientificName")

count(summaries, actions, other_threats)
## spp that need additional actions: 1521
## additional spp that have threats not tackled: 1971 (additional 1182 to above)
## total spp needing target 3: 2703
2703 / 7313 * 100


## additional spp with small populations: 489
count(summaries, actions, other_threats, smallpop)

write_csv(summaries, "data/target3_eligible.csv")


## Identify which spp needing ex-situ have ex-situ populations --------------------####

exsitu <- read.csv("data/spp_needing_exsitu.csv")


folders <- list.files("data/rl_download_12_05_2020/") ## make list of files in folder

allfields <- data.frame()


## ------------------- Load birds, corals and fw shrimp:-----------------------####
for(i in 1:length(folders)) {
  # Make df called summaries of all RL aummary files:
  summ <- read_csv(paste("data/rl_download_12_05_2020/", folders[i], "/all_other_fields.csv", 
                         sep = ""), na = c("", "NA"))
  summ <- select(summ, scientificName, InPlaceSpeciesManagementExSitu.value)
  allfields <- bind_rows(allfields, summ)
}

allfields <- allfields %>% 
  filter(scientificName %in% exsitu$scientificName) %>% 
  unique()
allfields %>% count(InPlaceSpeciesManagementExSitu.value)
