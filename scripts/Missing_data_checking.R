## --------------------------- Investigate missing data --------------------------------####

library(tidyverse)


## Threats


### % of spp with no threats listed:
(summaries %>% 
    filter(scientificName %in% threats$scientificName) %>% 
    count(scientificName) %>% 
    nrow()) / nrow(summaries) * 100


### Count spp where no threats listed by class:
no_thr <- summaries %>% 
  filter(!scientificName %in% threats$scientificName) %>% 
  count(className) %>% 
  rename(no_thr = n)


### Plot of percentages of threat listed by class:
summaries %>% 
  filter(scientificName %in% threats$scientificName) %>% 
  count(className) %>% 
  rename(yes_thr = n) %>% 
  left_join(no_thr, by = "className") %>% 
  replace_na(list(no_thr = 0)) %>% 
  mutate(perc = yes_thr / (yes_thr + no_thr) * 100) %>% 
  ggplot(aes(x = fct_reorder(className, perc), y = perc)) +
  geom_col() +
  coord_flip() +
  labs(x = "", y = "Percentage")


## Stresses

## % of threats with no stresses listed:

number_thr <- nrow(threats)
(threats %>% 
    filter(!is.na(stressCode)) %>% 
    nrow()) / number_thr * 100


no_str <- filter(threats, is.na(stressCode))
yes_str <- filter(threats, !is.na(stressCode))
filter(no_str, scientificName %in% yes_str$scientificName)
threats %>%  filter(scientificName == "Ferocactus flavovirens")


thr_str <- summaries %>% 
  select(scientificName, className) %>% 
  left_join(threats, by = "scientificName")

thr_uni <- thr_str %>% 
  select(scientificName, className, code) %>% 
  unique()

no_thr <- thr_uni %>% 
  filter(is.na(code)) %>% 
  count(className) %>% 
  rename(no_thr = n)

class_thr <- thr_uni %>% 
  filter(!is.na(code)) %>% 
  count(className) %>% 
  rename(yes_thr = n) %>% 
  left_join(no_thr, by = "className") %>% 
  replace_na(list(no_thr = 0))  %>% 
  mutate(perc_thr = yes_thr / (yes_thr + no_thr) * 100)

str_uni <- thr_str %>% 
  select(scientificName, className, code, stressCode) %>% 
  unique()


no_str <- str_uni %>% 
  filter(is.na(stressCode)) %>% 
  count(className) %>% 
  rename(no_str = n)

class_str <- str_uni %>% 
  filter(!is.na(stressCode)) %>% 
  count(className) %>% 
  rename(yes_str = n) %>% 
  left_join(no_str, by = "className") %>% 
  replace_na(list(no_str = 0))  %>% 
  mutate(perc_str = yes_str / (yes_str + no_str) * 100)


class_thr %>% 
  left_join(class_str, by = "className") %>% 
  mutate(perc_diff = perc_thr - perc_str) %>% 
  select(className, perc_str, perc_diff) %>% 
  gather("type", "perc", 2:3) %>% 
  ggplot(aes(x = fct_reorder(className, perc), y = perc, fill = type)) +
  geom_col() +
  geom_hline(yintercept = 100, size = 1) +
  coord_flip() +
  labs(x = "", y = "Percentage") +
  scale_fill_manual(values = c("#bdd7e7", "#3182bd"), labels = c("Threats only", "Threats and stresses"), name = "")

yes_stress <- filter(threats, !is.na(stressName))

(summaries %>% 
    filter(scientificName %in% yes_stress$scientificName) %>% 
    count(scientificName) %>% 
    nrow()) / nrow(summaries) * 100
## % of spp with stresses listed

no_stress <- filter(threats, is.na(stressName))
summaries %>% 
  filter(scientificName %in% no_stress$scientificName) %>% 
  group_by(scientificName, className) %>% 
  count() %>% 
  rename(no_str = n) -> 
  c
summaries %>% filter(scientificName %in% yes_stress$scientificName) %>% group_by(scientificName, className) %>% count()  %>% rename(yes_str = n) -> d

thr_str <- a %>% 
  left_join(b, by = "className") %>% 
  left_join(c, by = "className") %>% 
  left_join(d, by = "className") %>% 
  mutate(suma = yes_thr + no_thr) %>% 
  mutate(perc_thr = yes_thr/suma*100) %>% 
  mutate(sumb = yes_str + no_str) %>% 
  mutate(perc_str = yes_str/sumb*100) %>% 
  mutate(perc = perc_thr - perc_str)
ggplot(aes(x = fct_reorder(className, perc), y = perc)) +
  geom_col() +
  coord_flip() +
  labs(x = "Class", y = "Percentage of species which have stresses listed")
ggsave("figures/percent_stresses_listed.png", dpi = 300)



## Actions needed


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




