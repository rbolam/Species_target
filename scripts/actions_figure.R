## --------------------------- Make figure of actions ####

library(tidyverse)


actions <- read_csv("data/actions_needed.csv")

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
