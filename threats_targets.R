## Making plot linking threats to targets


library(tidyverse)
library(ggalluvial)
library(viridis)
library(cowplot)

threats <- read.csv("threats_targets.csv")

threats$code2 <- threats$code
threats <- separate(threats, code2, into = c("threat_level1"), extra = "drop") 

threats$threat_level1[threats$threat_level1 %in% c(4, 6)] <- c("12")

## Add in threat level 1 names

threat_level1 <- rep(1:12)
tnames <- c("Residential & commercial\ndevelopment", "Agriculture & aquaculture", "Energy production & mining", 
            "Transportation & service corridors", "Biological resource use", "Human intrusions & disturbance",
            "Natural system\nmodifications", "Invasive & other\nproblematic species",
            "Pollution", "Geological events", "Climate change &\nsevere weather", "Other")
tnames <- data.frame(cbind(threat_level1, tnames))

threats <- left_join(threats, tnames, by = "threat_level1")

threats %>% 
  group_by(Aichi.Target, Zero.draft.Action.Target, tnames) %>% 
  summarise(n_new = sum(n)) ->
  threats_summ


threats_summ <- filter(threats_summ, tnames != "Geological events")

threats_summ %>% group_by(tnames) %>% summarise(sum = sum(n_new)) %>% arrange(sum)
threats_summ$tnames <- factor(threats_summ$tnames, levels(threats_summ$tnames)[c(1, 2, 11, 7, 8, 3, 4, 10, 12, 6, 5, 9)])
threats_summ$Aichi.Target <- factor(threats_summ$Aichi.Target, levels(threats_summ$Aichi.Target)[c(2, 3, 4, 5, 6, 1)])



## Figure
ggplot(threats_summ, aes(axis1 = Aichi.Target, axis2 = tnames, axis3 = Zero.draft.Action.Target, y = n_new)) +
  geom_alluvium(aes(fill = tnames), width = 1/2, alpha = 0.9, aes.bind = TRUE) +
  geom_stratum(width = 1/2, size = 0.5, colour = "white", fill = 
                 c("grey90", "grey90", "grey90", "grey90", "grey90", "grey90", 
                   NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                   "grey90", "grey90", "grey90", "grey90", "grey90")) +
  #geom_text(stat = "stratum", infer.label = TRUE, size = 2, min.y = 1000, colour = "black") +
  geom_label(stat = "stratum", infer.label = TRUE, size = 2, min.y = 1000, fill = "grey90",
             label.padding = unit(0.08, "lines"), label.r = unit(0, "lines"), label.size = 0, alpha = 0.7) +
  scale_x_discrete(limits = c("Aichi Target", "Threat", "Post-2020 Target"), name = "", expand = c(.05, 0)) + 
  scale_y_continuous(name = "Number of species affected per threat", expand = c(0.0005, 0)) + 
  scale_fill_viridis_d(option = "E", direction = 1) +
  theme_classic() +
  theme(legend.position = "none",
        text = element_text(size = 10),
        #axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  annotate("text", x = 0.6, y = 5782, label = "Aichi Target 6", size = 2) +
  annotate("segment", x = 0.72, xend = 0.75, y = 5782, yend = 4921, size = 0.3) +
  
  annotate("text", x = 0.6, y = 4482, label = "Aichi Target 7", size = 2) +
  annotate("segment", x = 0.72, xend = 0.75, y = 4482, yend = 4482, size = 0.3) +
  
  annotate("text", x = 0.6, y = 3182, label = "Aichi Target 8", size = 2) +
  annotate("segment", x = 0.72, xend = 0.75, y = 3182, yend = 4016, size = 0.3) +
  
  annotate("text", x = 3.4, y = 4471, label = "New Target 4", size = 2) +
  annotate("segment", x = 3.25, xend = 3.28, y = 4471, yend = 4471, size = 0.3) +
  
  annotate("rect", xmin = 0.75, xmax = 1.25, ymin = 40279, ymax = 46665, fill = "grey90", colour = "white") +
  annotate("text", x = 1, y = 43472, label = "Not covered\nby targets", size = 2) +
  
  annotate("rect", xmin = 2.75, xmax = 3.25, ymin = 42367, ymax = 46665, fill = "grey90", colour = "white") +
  annotate("text", x = 3, y = 44516, label = "Not covered\nby targets", size = 2)

ggsave("flow.png", width = 7, height = 5, dpi = 600)

