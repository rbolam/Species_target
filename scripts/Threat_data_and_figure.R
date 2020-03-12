## --------------------------- Make figure of threats ####

library(tidyverse)

threats <- read_csv("data/stresses.csv", na = c("", "NA")) ## this contains threats to species in comprehensively assessed groups



## -------------------------- Make figure of threats ####

### Separate threat codes for matching
threats$code2 <- threats$code
threats <- separate(threats, code2, into = c("threat_level1"), extra = "drop") 

### Add in threat level 1 names
threat_level1 <- rep(1:12)
tnames <- c("Residential & commercial development", "Agriculture & aquaculture", "Energy production & mining", 
            "Transportation & service corridors", "Biological resource use", "Human intrusions & disturbance",
            "Natural system modifications", "Invasive & other problematic species",
            "Pollution", "Geological events", "Climate change & severe weather", "Other")
tnames <- data.frame(cbind(threat_level1, tnames))
threats <- left_join(threats, tnames, by = "threat_level1")

threats %>% 
  select(scientificName, tnames, stress) %>% 
  unique() %>% 
  ggplot(aes(x = fct_rev(fct_infreq(tnames)), fill = stress)) +
  geom_bar() +
  coord_flip() +
  labs(x = "Threat level 1", y = "Number of species affected") +
  #scale_fill_viridis_d(option = "B", direction = 1, name = "Stress") +
  scale_fill_brewer(palette = "PuBuGn", direction = -1, name = "Stress") +
  theme(legend.position = "bottom")
ggsave("figures/threats_stresses.png", dpi = 300)


