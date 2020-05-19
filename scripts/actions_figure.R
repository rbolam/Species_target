## --------------------------- Make figure of actions ####

library(tidyverse)
library(lemon)

actions <- read_csv("data/actions_needed.csv")
summaries <- read_csv("data/simple_summaries.csv")
summaries <- select(summaries, scientificName, redlistCategory)

actions <- left_join(actions, summaries, by = "scientificName")

actions$name[actions$code %in% c("3.1.1", "3.1.2", "3.1.3")] <- c("Species management")
actions$name[actions$code %in% c("3.3.1", "3.3.2")] <- c("Species re-introduction")
actions$name[actions$code %in% c("3.4.1", "3.4.2")] <- c("Ex-situ conservation")
actions$name[actions$code %in% c("5.1.1", "5.1.2", "5.1.3", "5.1.4")] <- c("Legislation")
actions$name[actions$code %in% c("5.4.1", "5.4.2", "5.4.3", "5.4.4")] <- c("Compliance and enforcement")

actions$redlistCategory <- factor(actions$redlistCategory)
actions$redlistCategory <- factor(actions$redlistCategory, levels(actions$redlistCategory)[c(4, 2, 1, 3)])

write_csv(actions, "data/actions_needed_tidy.csv")

actions %>% 
  select(scientificName, name, redlistCategory) %>% 
  filter(redlistCategory != "Extinct in the Wild") %>% ##exclude 15 EW spp as can't see them
  unique() %>% 
  ggplot(aes(x = fct_rev(fct_infreq(name)), fill = redlistCategory)) +
  geom_bar() +
  coord_flip(ylim = c(0, 4000)) +
  scale_fill_manual(values = c("#fecc5c", "#fd8d3c", "#e31a1c"), 
                    name = "IUCN Red List\nCategory") +
  scale_y_continuous(expand = c(0.001, 0)) + 
  theme_classic() +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "Recovery actions needed", y = "Number of species") +
  theme(legend.position = c(0.8, 0.3),
        text = element_text(size = 6.5))
ggsave("figures/actions.png", height = 8, width = 12, dpi = 300, unit = "cm")

