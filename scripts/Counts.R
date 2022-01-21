## --------------------------------------------------------------------------------------------##
## ----------------------- Counting different species groups --------------------------------####
## --------------------------------------------------------------------------------------------##

library(tidyverse)
library(gridExtra)
library(viridis)
library(ggvenn)

summaries <- read.csv("data/all_summaries.csv")
nspp <- summaries %>% select(scientificName, className) %>% unique()  ##no of all included spp
tspp <- summaries %>% 
  filter(redlistCategory %in% c("Extinct in the Wild", "Critically Endangered",
                                "Endangered", "Vulnerable")) %>% 
  select(scientificName, phylumName, className) %>% 
  unique() ##no of all included threatened/ EW spp
nrow(tspp) / nrow(nspp) *100  ## calculate % of all spp that are threatened + EW

nspp %>% count(className) ## count no in each class
tspp %>% count(phylumName, className) %>% arrange(-n)




## ----------------------- Counts threats, targets, actions --------------------------------####


## Percent of threatened spp with at least one threat listed:
thr <- read.csv("data/threats.csv")
thr %>% 
  filter(scientificName %in% tspp$scientificName) %>% 
  select(scientificName) %>% 
  unique() %>% 
  nrow() / nrow(tspp) * 100



## Count spp threatened by each threat (level 1):
thr_str <- read.csv("data/spp_tar.csv")
thr_str %>% 
  select(scientificName, thr_lev1name) %>% 
  unique() %>%
  filter(thr_lev1name != "Geological events") %>% #remove 108 spp with Geol events (no mitigation)
  count(thr_lev1name) %>% 
  mutate(perc1 = n / nrow(tspp) * 100) %>% ## calculate % of threatened/EW spp
  mutate(perc2 = n / nrow(nspp) * 100) %>% ## calculate % of all spp
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
#ggsave("figures/supp_threat_figure.png", height = 6, width = 9, dpi = 300, unit = "cm")



## Percent/Count spp benefiting from each target:
thr_str %>% 
  filter(thr_lev1name != "Geological events") %>% ## remove Geol events as can't be mitigated
  select(scientificName, target) %>% 
  unique() %>% 
  count(target) %>% 
  mutate(perc1 = n / nrow(tspp) * 100) %>% ## calculate % of threatened/EW spp
  mutate(perc2 = n / nrow(nspp) * 100) %>% ## calculate % of all spp
  arrange(-n)


## Percent/Count spp benefiting from target 3:
thr_str %>% 
  filter(thr_lev1name != "Geological events") %>% ## remove Geol events as can't be mitigated
  filter(target == "Target 3") %>% 
  select(scientificName) %>% 
  unique() ->
  tar3

thr_str %>% 
  filter(thr_lev1name != "Geological events") %>% ## remove Geol events as can't be mitigated
  filter(scientificName %in% tar3$scientificName) %>% 
  select(scientificName, target) %>% 
  unique() %>% 
  count(scientificName) %>% 
  filter(n > 1) %>% 
  #filter(n == 1) %>% 
  nrow()
  


thr_str %>% 
  filter(thr_lev1name != "Geological events") %>% ## remove Geol events as can't be mitigated
  select(scientificName, target) %>% 
  filter(!is.na(target)) %>% 
  unique() %>% 
  ggplot(aes(x = fct_rev(target), fill = target)) +
  geom_bar() +
  coord_flip() +
  scale_fill_viridis_d(option = "D") +
  scale_y_continuous(expand = c(0.005, 0)) + 
  theme_classic() +
  labs(x = "Threats", y = "Number of species") +
  theme(legend.position = "none",
        text = element_text(size = 6.5))
#ggsave("figures/supp_target_figure.png", height = 4, width = 5, dpi = 300, unit = "cm")


## Counts of spp requiring Target 3

## Filter all spp benefiting from target 3 (those for which threats not addressed by other targets):
target3 <- thr_str %>% 
  filter(target == "Target 3") %>% 
  select(scientificName) %>% 
  unique()

## Filter spp ONLY benefiting from target 3:

othertar <- thr_str %>% 
  filter(target != "Target 3") %>% 
  select(scientificName) %>% 
  unique()

target3 %>% filter(!scientificName %in% othertar$scientificName) %>% nrow()


## Percent of spp benefiting from different actions:
act <- read.csv("data/actions_needed_tidy.csv")
act %>% 
  select(scientificName, name) %>% 
  unique() %>% 
  count(name) %>% 
  mutate(perc1 = n / nrow(tspp) * 100) %>% ## calculate % of threatened/EW spp
  mutate(perc2 = n / nrow(nspp) * 100) %>% ## calculate % of all spp
  arrange(-n)

## Spp with actions listed

tspp %>% filter(scientificName %in% act$scientificName) %>% nrow()
6692 / nrow(tspp) * 100

## Spp benefiting from site/area protection and management:
act %>% 
  filter(name %in% c("Site/area protection", "Site/area management")) %>% 
  select(scientificName) %>% 
  unique() %>% 
  nrow()

act %>% 
  filter(name %in% c("Ex-situ conservation", "Species re-introduction", 
                     "Species recovery")) %>% 
  select(scientificName) %>% 
  unique() ->
  tar3actions



tar3actions <- tar3actions %>% 
  full_join(target3) %>% 
  replace_na(list(`Species that require\nrecovery actions` = FALSE, 
                  `Species that require\nthreat mitigation` = FALSE))





## -------------- Identifying spp which need captivity until solutions found -----------------####


## Count corals threatened by temperature extremes:

thr_str <- read.csv("data/spp_tar.csv") ## file contains threats to second level
thr_str %>%  
  left_join(summaries, by = "scientificName") %>% 
  filter(phylumName == "CNIDARIA" & thr_lev2 == "11.3") %>% 
  select(scientificName) %>% 
  unique() %>% 
  nrow()


## Count amphibians threatened by chytrid disease:
threats <- read.csv("data/threats.csv") ## file contains named invasive species
threats %>% 
  left_join(summaries, by = "scientificName") %>% 
  filter(className == "AMPHIBIA" & ias == "Batrachochytrium dendrobatidis") %>% 
  select(scientificName) %>% 
  unique() %>% 
  nrow()




## ------------------- Identifying spp which need emergency actions ------------------------####


## --------------------------- Species that need actions addressed by target 3 -------------####

actions <- read.csv("data/actions_needed_tidy.csv")


## Count how many spp need emergency actions listed:
actions %>% 
  filter(name %in% c("Ex-situ conservation", "Species re-introduction", "Species recovery")) %>% 
  select(scientificName, name) %>% 
  unique() %>% 
  count(name)


actions_tar3 <- actions %>% 
  filter(name %in% c("Ex-situ conservation", "Species re-introduction", "Species recovery")) %>% 
  select(scientificName) %>% 
  unique()

actions_tar3$actions <- "yes"



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

summaries <- read.csv("data/all_summaries.csv")
summaries <- full_join(summaries, mature, by = "scientificName")

mature <- summaries %>% 
  filter(low <= 1000) %>% 
  select(scientificName, redlistCategory, low) %>% 
  rename(min_population = low) %>% 
  unique()


## Species which fit certain criteria:

summaries <- read.csv("data/all_summaries.csv")
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

suma <- suma %>% 
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
## spp that need additional actions:
1252 + 611 #1863
## spp that have threats not tackled:
1252 + 2172 #3424
## total spp needing target 3, and %:
1252 + 611 + 2172 #4035
4035 / 7784 * 100 #52%


## additional spp with small populations: 393
count(summaries, actions, other_threats, smallpop)

write_csv(summaries, "data/target3_eligible.csv")



## Venn diagram


summaries <- read.csv("data/target3_eligible.csv")

summaries$actions <- recode(summaries$actions, yes = TRUE)
summaries$other_threats <- recode(summaries$other_threats, yes = TRUE)
summaries$smallpop <- recode(summaries$smallpop, yes = TRUE)

summaries <- replace_na(summaries, list(actions = FALSE, other_threats = FALSE, 
                                        smallpop = FALSE))

summaries <-rename(summaries, 
                   `a) Species requiring\nrecovery actions` = actions,
                   `b) Species affected by threats\nnot addressed by other targets` = other_threats,
                   `c) Species with very small\npopulation sizes or ranges` = smallpop)


ggplot(summaries, aes(A = `a) Species requiring\nrecovery actions`, 
                      B = `b) Species affected by threats\nnot addressed by other targets`,
                      C = `c) Species with very small\npopulation sizes or ranges`)) +
  geom_venn(fill_color = c("#8c96c6", "#810f7c", "#edf8fb"), 
            fill_alpha = 0.8,
            stroke_color = c("#8c96c6", "#810f7c", "#edf8fb"), 
            show_percentage = FALSE,
            set_name_size = 2.2) + 
  theme_void() + 
  coord_fixed()
ggsave("figures/venn_diagram.png", width = 6, height = 3.1, dpi = 300)

library(VennDiagram)

vennl <- list(A = 1:1863, B = 612:3135, C = c(485:908, 2846:3528))
names(vennl) <- c("Species requiring\nrecovery actions",
                  "Species affected by\nother threats",
                  "Species with small\npopulation sizes")


venn.diagram(vennl, 
             #fill = c("#B4B8AB", "#153243"), 
             alpha = c(0.6, 0.6, 0.6), 
             #cat.just=list(c(0.8, -8.5) , c(0.3, -10.5)),
             "figures/venn_diagram2.png")

overrideTriple=T
draw.triple.venn(9, 20, 30, 2, 10, 3, 2, category =
                   rep("", 3), rotation = 1, reverse = FALSE, euler.d = F, scaled = F)



## New supplementary figures
threats <- read.csv("data/spp_tar.csv")
target3 <- read.csv("data/target3_eligible.csv")
target3 <- target3 %>% 
  filter(actions == "yes" | smallpop == "yes") %>% 
  select(scientificName)
target3$target <- "Target 3"
target3$thr_lev1name <- "Additional actions required"

threats <- bind_rows(threats, target3)


threats %>% 
  select(scientificName, thr_lev1, thr_lev1name) %>% 
  unique() %>% 
  count(scientificName) %>% 
  ggplot(aes(x = n)) +
  geom_histogram() +
  labs(x = "Number of threats",
       y = "Number of species")
ggsave("figures/WebFigure3.png", width = 6, height = 3.1, dpi = 300)


threats %>% 
  select(scientificName, thr_lev1, thr_lev1name) %>% 
  unique() %>% 
  count(scientificName) %>% 
  filter(n == 1) ->
  spp_one_threat


threats %>% 
  filter(scientificName %in% spp_one_threat$scientificName) %>% 
  count(thr_lev1, thr_lev1name) %>% 
  filter(!is.na(thr_lev1)) %>% 
  ggplot(aes(x = n, y = fct_reorder(thr_lev1name, n))) +
  geom_col() +
  labs(x = "Number of species",
       y = "Threat")
ggsave("figures/WebFigure4.png", width = 6, height = 3.1, dpi = 300)


threats %>% 
  select(scientificName, target) %>% 
  unique() %>% 
  count(scientificName) %>% 
  ggplot(aes(x = n)) +
  geom_histogram() +
  labs(x = "Number of targets required",
       y = "Number of species")
ggsave("figures/WebFigure5.png", width = 6, height = 3.1, dpi = 300)
