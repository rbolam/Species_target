library(tidyverse)
library(viridis)



(threats %>% filter(stressName != "") %>% nrow())/nrow(threats)*100

threats <- threats %>% 
  filter(timing %in% c("Future", "Ongoing")) %>% 
  select(scientificName, code, name, stressName) %>% 
  separate(stressName, into = c("s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8"), sep = "[|]", fill = "right") %>% 
  gather(4:11, key = s, value = stress, na.rm = T) %>% 
  select(-s) %>% 
  unique() %>% 
  filter(stress != "")

threats$stress <- as.character(threats$stress)


threats$stress[threats$stress %in% c("Hybridisation", "Competition", "Loss of mutualism", "Loss of pollinator", "Inbreeding", 
                                     "Skewed sex ratios", "Reduced reproductive success", "Other")] <- 
  c("Indirect species effects")
threats <- filter(threats, stress != "Ecosystem stresses")
threats$stress <- as.factor(threats$stress)
threats$stress <- factor(threats$stress, levels(threats$stress)[c(1, 2, 3, 6, 5, 4)])

threats %>% 
  group_by(code, name, stress) %>% 
  count() ->
  stress
write_csv(stress, "stresses.csv")


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
  scale_fill_brewer(palette = "Dark2", direction = -1, name = "Stress") +
  theme(legend.position = "bottom")
ggsave("figures/threats_stresses.png", dpi = 300)


