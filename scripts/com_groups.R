## --------------------------- Merging all comp groups --------------------------------####

library(rredlist)
library(tidyverse)
library(viridis)

## -------------------- Looking at comprehensively assessed groups ####


IUCN_REDLIST_KEY <- "f08cfc8b6eb01c715601bb49551d131a4bb762625c61d8d42c970f8d17c61629"
rl_comp_groups(key = IUCN_REDLIST_KEY)

comp <- rl_comp_groups(key = IUCN_REDLIST_KEY)
arrange(comp[[2]], group_name)

nrow(rl_comp_groups('hagfishes', key = IUCN_REDLIST_KEY)[[2]])



## ------------------------------ Read in files and remove duplicate spp ####

folders <- list.files("rl_download_02_03_2020/") ## make list of files in folder
summaries <- data.frame()
threats <- data.frame()
actions <- data.frame()


for(i in 1:length(folders)) {
  summ <- read.csv(paste("rl_download_02_03_2020/", folders[i], "/simple_summary.csv", sep = ""))
  summaries <- bind_rows(summaries, summ)
  thr <- read.csv(paste("rl_download_02_03_2020/", folders[i], "/threats.csv", sep = ""))
  threats <- bind_rows(threats, thr)
  act <- read.csv(paste("rl_download_02_03_2020/", folders[i], "/conservation_needed.csv", sep = ""))
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


## ---------------------- Figure out if there are patterns in mising data ####

(threats %>% group_by(scientificName) %>% count() %>% nrow())/nrow(summaries)*100 ## % of spp with threats listed

no_threats <- filter(summaries, !scientificName %in% threats$scientificName)
no_threats %>% count(className) %>% rename(no_thr = n) -> a
yes_threats <- filter(summaries, scientificName %in% threats$scientificName)
yes_threats %>% count(className) %>% rename(yes_thr = n) -> b

a %>% 
  left_join(b, by = "className") %>% 
  mutate(suma = yes_thr + no_thr) %>% 
  mutate(perc = yes_thr/suma*100) %>% 
  ggplot(aes(x = fct_reorder(className, perc), y = perc)) +
  geom_col() +
  coord_flip() +
  labs(x = "Class", y = "Percentage of species which have threats listed")
ggsave("figures/percent_threats_listed.png", dpi = 300)
## all above 75%



(actions %>% group_by(scientificName) %>% count() %>% nrow())/nrow(summaries)*100 ## % of spp with actions listed

no_actions <- filter(summaries, !scientificName %in% actions$scientificName)
no_actions %>% count(className) %>% rename(no_act = n) -> a
yes_actions <- filter(summaries, scientificName %in% actions$scientificName)
yes_actions %>% count(className) %>% rename(yes_act = n) -> b

a %>% 
  left_join(b, by = "className") %>% 
  mutate(suma = yes_act + no_act) %>% 
  mutate(perc = yes_act/suma*100) %>% 
  ggplot(aes(x = fct_reorder(className, perc), y = perc)) +
  geom_col() +
  coord_flip() +
  labs(x = "Class", y = "Percentage of species which have 'Actions Needed' listed")
ggsave("figures/percent_actions_listed.png", dpi = 300)
## all above 50% except for Malacostraca and Gastropoda



## --------------------------- Make figure of actions ####

actions$name[actions$code %in% c("3.1.1", "3.1.2", "3.1.3")] <- c("Species management")
actions$name[actions$code %in% c("3.3.1", "3.3.2")] <- c("Species re-introduction")
actions$name[actions$code %in% c("3.4.1", "3.4.2")] <- c("Ex-situ conservation")
actions$name[actions$code %in% c("5.1.1", "5.1.2", "5.1.3", "5.1.4")] <- c("Legislation")
actions$name[actions$code %in% c("5.4.1", "5.4.2", "5.4.3", "5.4.4")] <- c("Compliance and enforcement")

actions %>% 
  select(scientificName, name) %>% 
  unique() %>% 
  ggplot(aes(x = fct_rev(fct_infreq(name)))) +
  geom_bar() +
  coord_flip() +
  labs(x = "Actions needed", y = "Number of species needing actions")
ggsave("figures/actions_needed.png", dpi = 300)
