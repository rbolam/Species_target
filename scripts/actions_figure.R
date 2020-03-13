## --------------------------- Make figure of actions ####

library(tidyverse)


actions <- read_csv("data/actions_needed.csv")
summaries <- read_csv("data/simple_summaries.csv")
summaries <- select(summaries, scientificName, className, redlistCategory)

actions <- left_join(actions, summaries, by = "scientificName")

actions$name[actions$code %in% c("3.1.1", "3.1.2", "3.1.3")] <- c("Species management")
actions$name[actions$code %in% c("3.3.1", "3.3.2")] <- c("Species re-introduction")
actions$name[actions$code %in% c("3.4.1", "3.4.2")] <- c("Ex-situ conservation")
actions$name[actions$code %in% c("5.1.1", "5.1.2", "5.1.3", "5.1.4")] <- c("Legislation")
actions$name[actions$code %in% c("5.4.1", "5.4.2", "5.4.3", "5.4.4")] <- c("Compliance and enforcement")

actions$redlistCategory <- factor(actions$redlistCategory)
actions$redlistCategory <- factor(actions$redlistCategory, levels(actions$redlistCategory)[c(3, 4, 2, 1)])

actions %>% 
  select(scientificName, name, className, redlistCategory) %>% 
  unique() %>% 
  filter(className != "GASTROPODA") %>% 
  ggplot(aes(x = fct_infreq(name), fill = redlistCategory)) +
  geom_bar() +
  scale_fill_brewer(palette = "YlOrRd", name = "IUCN Red List\nCategory") +
  labs(x = "Actions needed", y = "Number of species needing actions") +
  facet_wrap(~className, ncol = 1, scales = "free_y", strip.position = "right") +
  theme(legend.position = "right", 
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        strip.text.y = element_text(angle = 0))
ggsave("figures/actions_option1.png", height = 10, width = 8, dpi = 300)

actions %>% 
  select(scientificName, name, className) %>% 
  unique() %>% 
  ggplot(aes(x = fct_rev(fct_infreq(name)), fill = className)) +
  geom_bar() +
  coord_flip() +
  scale_fill_viridis_d(option = "D", name = "Class") +
  labs(x = "Actions needed", y = "Number of species needing actions")
ggsave("figures/actions_option2.png", dpi = 300)

actions %>% 
  select(scientificName, name, redlistCategory) %>% 
  unique() %>% 
  ggplot(aes(x = fct_rev(fct_infreq(name)), fill = redlistCategory)) +
  geom_bar() +
  coord_flip() +
  scale_fill_brewer(palette = "YlOrRd", name = "IUCN Red List\nCategory") +
  labs(x = "Actions needed", y = "Number of species needing actions")
ggsave("figures/actions_option3.png", dpi = 300)



## ---------------------------- Add number of spp requiring each action and save -----------------####

actions %>% 
  select(scientificName, code, name) %>% 
  separate(col = code, into = c("A1", "A2"), sep = "[.]") %>% 
  unite(code, A1:A2, sep = ".") ->
  no_actions

no_actions$name[no_actions$code == 3.1] <- c("Species management")
no_actions$name[no_actions$code == 3.3] <- c("Species re-introduction")
no_actions$name[no_actions$code == 3.4] <- c("Ex-situ conservation")
no_actions$name[no_actions$code == 5.1] <- c("Legislation")
no_actions$name[no_actions$code == 5.4] <- c("Compliance and enforcement")

no_actions %>% 
  unique() %>% 
  group_by(code, name) %>% 
  count() ->
  no_actions

write_csv(no_actions, "data/number_of_actions.csv")
