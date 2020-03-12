# --------------------------- Making plot linking threats to targets ---------------------------####

### Load packages
library(tidyverse)
library(ggalluvial)
library(viridis)



## --------------------------- Sort data --------------------------####
threats <- read.csv("data/thr_str_tar_matched.csv")

### Separate threat codes for matching
threats <- separate(threats, thr_lev2, into = c("threat_level1"), extra = "drop") 


### Add threats 3, 4 and 6 (Energy Production & Mining, Transportation & service corridors, Human intrusions & disturbance) 
### to "other" as there were less than 5% of observations in each one respectively
threats$threat_level1[threats$threat_level1 %in% c(3, 4, 6)] <- c("12")

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
  group_by(target, tnames) %>% 
  summarise(n_new = sum(n)) ->
  threats_summ

### Remove threat Geological events as they cannot be addressed by conservation action/policy
#threats_summ <- filter(threats_summ, tnames != "Geological events")

### Reorder factors depending on size of threat, or numerical order of Target
threats_summ %>% group_by(tnames) %>% summarise(sum = sum(n_new)) %>% arrange(-sum)
threats_summ$tnames <- factor(threats_summ$tnames, levels(threats_summ$tnames)[c(1, 2, 11, 7, 10, 3, 8, 9, 4, 5, 6, 12)])



## --------------------------- Code for figure --------------------------####

ggplot(threats_summ, aes(axis1 = tnames, axis2 = target, y = n_new)) +
  geom_alluvium(aes(fill = tnames), alpha = 0.9, aes.bind = TRUE, width = 1/4) +
  geom_stratum(size = 0.5, colour = "white", width = 1/4,
               fill = 
                 c(NA, NA, NA, NA, NA, NA, NA, NA,
                   "grey90", "grey90", "grey90", "grey90", "grey90")) +
  geom_text(stat = "stratum", infer.label = TRUE, size = 2.4, min.y = 1000, colour = 
              c("black", "black", "black", "white", "white", "white", "white", "white", 
                "black", "black", "black", "black", "black")) +
  scale_x_discrete(limits = c("Threat", "Post-2020\nFramework"), name = "", expand = c(.1, 0)) + 
  scale_y_continuous(name = "Number of species affected per threat", expand = c(0.0005, 0)) + 
  scale_fill_viridis_d(option = "E", direction = 1) +
  theme_classic() +
  theme(legend.position = "none",
        text = element_text(size = 10),
        axis.ticks.x = element_blank()) +
  annotate("rect", xmin = 1.875, xmax = 2.125, ymin = 62888, ymax = 69213, fill = "grey30", colour = "white") +
  annotate("text", x = 2, y = 66051, label = "Not addressed\nby targets", size = 2.4, colour = "white")

ggsave("figures/flow.png", width = 7, height = 5, dpi = 600)

