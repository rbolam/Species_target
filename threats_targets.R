# --------------------------- Making plot linking threats to targets ---------------------------####

### Load packages
library(tidyverse)
library(ggalluvial)
library(viridis)

## --------------------------- Sort data --------------------------####
threats <- read.csv("threats_targets.csv")

### Separate threat codes for matching
threats$code2 <- threats$code
threats <- separate(threats, code2, into = c("threat_level1"), extra = "drop") 


### Add threats 4 and 6 (Transportation & service corridors, Human intrusions & disturbance) to "other" 
### as there were only 3% and 2% of observations in each one respectively
threats$threat_level1[threats$threat_level1 %in% c(4, 6)] <- c("12")

### Add in threat level 1 names
threat_level1 <- rep(1:12)
tnames <- c("Residential & commercial\ndevelopment", "Agriculture & aquaculture", "Energy production & mining", 
            "Transportation & service corridors", "Biological resource use", "Human intrusions & disturbance",
            "Natural system modifications", "Invasive & other\nproblematic species",
            "Pollution", "Geological events", "Climate change &\nsevere weather", "Other")
tnames <- data.frame(cbind(threat_level1, tnames))
threats <- left_join(threats, tnames, by = "threat_level1")

### Count how many threats in each target
threats %>% 
  group_by(Aichi.Target, Zero.draft.Action.Target, tnames) %>% 
  summarise(n_new = sum(n)) ->
  threats_summ

### Remove threat Geological events as they cannot be addressed by conservation action/policy
threats_summ <- filter(threats_summ, tnames != "Geological events")

### Reorder factors depending on size of threat, or numerical order of Target
threats_summ %>% group_by(tnames) %>% summarise(sum = sum(n_new)) %>% arrange(sum)
threats_summ$tnames <- factor(threats_summ$tnames, levels(threats_summ$tnames)[c(1, 2, 11, 7, 8, 3, 4, 10, 12, 6, 5, 9)])
threats_summ$Aichi.Target <- factor(threats_summ$Aichi.Target, levels(threats_summ$Aichi.Target)[c(2, 3, 4, 5, 6, 1)])



## --------------------------- Code for figure --------------------------####

ggplot(threats_summ, aes(axis1 = Aichi.Target, axis2 = tnames, axis3 = Zero.draft.Action.Target, y = n_new)) +
  geom_alluvium(aes(fill = tnames), alpha = 0.9, aes.bind = TRUE, width = c(1/4, 1/2, 1/4)) +
  geom_stratum(size = 0.5, colour = "white", width = 
                 c(1/4, 1/4, 1/4, 1/4, 1/4, 1/4, 
                   1/2, 1/2, 1/2, 1/2, 1/2, 1/2, 1/2, 1/2, 1/2, 
                   1/4, 1/4, 1/4, 1/4, 1/4), 
               fill = 
                 c("grey90", "grey90", "grey90", "grey90", "grey90", "grey90", 
                   NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                   "grey90", "grey90", "grey90", "grey90", "grey90")) +
  geom_text(stat = "stratum", infer.label = TRUE, size = 2, min.y = 1000, colour = 
              c("black", "black", "black",
                "black", "black", "black", "black", "white", "white", "white", "white", "white",
                "black", "black", "black", "black")) +
  scale_x_discrete(limits = c("Aichi Target", "Threat", "Post-2020 Target"), name = "", expand = c(.0005, 0)) + 
  scale_y_continuous(name = "Number of species affected per threat", expand = c(0.0005, 0)) + 
  scale_fill_viridis_d(option = "E", direction = 1) +
  theme_classic() +
  theme(legend.position = "none",
        text = element_text(size = 10),
        axis.ticks.x = element_blank()) +
  annotate("text", x = 0.97, y = 8782, label = "Aichi Target 6", size = 2) +
  annotate("segment", x = 0.89, xend = 0.89, y = 8282, yend = 4921, size = 0.3) +
  
  annotate("text", x = 1, y = 7482, label = "Aichi Target 7", size = 2) +
  annotate("segment", x = 0.92, xend = 0.92, y = 6982, yend = 4482, size = 0.3) +
  
  annotate("text", x = 1.03, y = 6182, label = "Aichi Target 8", size = 2) +
  annotate("segment", x = 0.95, xend = 0.95, y = 5682, yend = 4016, size = 0.3) +
  
  annotate("text", x = 2.97, y = 8471, label = "New Target 4", size = 2) +
  annotate("segment", x = 2.89, xend = 2.89, y = 4471, yend = 7971, size = 0.3) +
  
  annotate("rect", xmin = 0.875, xmax = 1.125, ymin = 40279, ymax = 46665, fill = "grey30", colour = "white") +
  annotate("text", x = 1, y = 43472, label = "Not addressed\nby targets", size = 2, colour = "white") +
  
  annotate("rect", xmin = 2.875, xmax = 3.125, ymin = 42367, ymax = 46665, fill = "grey30", colour = "white") +
  annotate("text", x = 3, y = 44516, label = "Not addressed\nby targets", size = 2, colour = "white")

ggsave("flow.png", width = 7, height = 5, dpi = 600)

