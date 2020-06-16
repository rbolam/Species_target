# --------------------------- Making plot linking threats to targets ------------------------####

### Load packages
library(tidyverse)
library(ggalluvial)
library(viridis)
library(rworldmap)
library(ggalt)
library(cowplot)

## --------------------------- Sort data --------------------------####
threats <- read.csv("data/spp_tar.csv")

### Remove threat Geological events as they cannot be addressed by conservation action/policy
threats <- threats %>% 
  filter(!thr_lev2 %in% c(10.1, 10.2, 10.3)) %>% 
  select(scientificName, thr_lev1name, target) %>% 
  unique()


### Add threats Transportation & service corridors, Energy Production & Mining, 
### Human intrusions & disturbance, to "other" as less than 5% of observations in each:
threats %>% count(thr_lev1name) %>% mutate(perc = n/sum(n)*100) %>% arrange(-perc)

threats$thr_lev1name[threats$thr_lev1name %in% c("Transportation & service corridors", 
                                                 "Energy production & mining", 
                                                 "Human intrusions & disturbance")] <- 
  c("Other")


### Count how many threats in each target
threats %>% 
  group_by(target, thr_lev1name) %>% 
  count() ->
  threats_summ


### Reorder factors depending on size of threat, or numerical order of Target
threats_summ %>% group_by(thr_lev1name) %>% summarise(sum = sum(n)) %>% arrange(-sum)
threats_summ$thr_lev1name <- factor(threats_summ$thr_lev1name)
levels(threats_summ$thr_lev1name)
threats_summ$thr_lev1name <- factor(threats_summ$thr_lev1name, levels(threats_summ$thr_lev1name)[c(2, 1, 8, 4, 5, 7, 3, 6)])


## ---------------------------------------- Figure 1a ----------------------------------####



## Set coordinates for polygons:
x <- c(1, 1, 1.124, 1.124)


## Number of spp-threat combinations in each target:
threats_summ %>% 
  group_by(target) %>% 
  summarise(all = sum(n)) ->
  tar_all


## Number of spp benefitting from each target:
threats %>% 
  select(scientificName, target) %>% 
  unique() %>% 
  count(target) ->
  tar_n


## Calculate difference:
tar <- full_join(tar_all, tar_n, by = "target")
tar <- mutate(tar, double = all - n)


# Calculate other values
## Spp not addressed by targets:
sum(tar$all) - tar[6,2] + (tar[6,4]/2) #top of rectangle
sum(tar$all) - (tar[6,4]/2) #bottom of rectangle
sum(tar$all) #top of polygon
sum(tar$all) - tar[6,2] #bottom of polygon

## Spp addressed by targets 1 and 2:
sum(tar$all) - tar[6,2] - tar[1,2] + (tar[1,4]/2) #top of rectangle
sum(tar$all) - tar[6,2] - (tar[1,4]/2) #bottom of rectangle
sum(tar$all) - tar[6,2] #top of polygon
sum(tar$all) - tar[6,2] - tar[1,2] #bottom of polygon


## Calculate no of spp in each threat (by target - some double counted):
threats_summ %>% 
  group_by(thr_lev1name) %>% 
  summarise(all = sum(n)) ->
  thr_all


## Calculate no of spp in each threat uniquely:
threats %>% 
  select(scientificName, thr_lev1name) %>% 
  unique() %>% 
  count(thr_lev1name) ->
  thr_n


##Merge and calculate difference:
thr <- thr_all %>% 
  full_join(thr_n, by = "thr_lev1name") %>% 
  mutate(double = all - n)

thr$y1 <- NA
thr$y2 <- NA


for (i in 1:8) {
  if (i == 1) {
    thr$y1[i] <- sum(thr$all) - thr$all[i]
    thr$y2[i] <- sum(thr$all)
  } else {
    thr$y1[i] <- thr$y1[i - 1] - thr$all[i]
    thr$y2[i] <- thr$y1[i-1]
  }
}

thr <- thr %>% 
  mutate(ymin = y1 + double/2) %>% 
  mutate(ymax = y2 - double/2)

thr <- filter(thr, double > 0)



ggplot(threats_summ, aes(axis1 = thr_lev1name, axis2 = target, y = n)) +
  geom_alluvium(aes(fill = thr_lev1name), alpha = 0.9, aes.bind = TRUE, width = 1/4) +
  geom_stratum(size = 0.5, colour = "white", width = 1/4, fill = 
                 c("white", NA, NA, rep("white", 5), rep("grey90", 4), "white", "white")) +
  ## Polygons for threats:
  annotate("polygon", x = x, y = c(thr$ymin[1], thr$ymax[1], thr$y2[1], thr$y1[1]), 
           fill = viridis(8, option = "E")[1], alpha = 0.9, colour = NA) + 
  annotate("polygon", x = x, y = c(thr$ymin[2], thr$ymax[2], thr$y2[2], thr$y1[2]), 
           fill = viridis(8, option = "E")[2], alpha = 0.9, colour = NA) +  
  annotate("polygon", x = x, y = c(thr$ymin[3], thr$ymax[3], thr$y2[3], thr$y1[3]), 
           fill = viridis(8, option = "E")[3], alpha = 0.9, colour = NA) +  
  annotate("polygon", x = x, y = c(thr$ymin[4], thr$ymax[4], thr$y2[4], thr$y1[4]), 
           fill = viridis(8, option = "E")[4], alpha = 0.9, colour = NA) +  
  annotate("polygon", x = x, y = c(thr$ymin[5], thr$ymax[5], thr$y2[5], thr$y1[5]), 
           fill = viridis(8, option = "E")[5], alpha = 0.9, colour = NA) +  
  annotate("polygon", x = x, y = c(thr$ymin[6], thr$ymax[6], thr$y2[6], thr$y1[6]), 
           fill = viridis(8, option = "E")[8], alpha = 0.9, colour = NA) +  
  
  # Rectangles for threats:  
  annotate("rect", xmin = 0.875, xmax = 1, ymin = thr$ymin, ymax = thr$ymax, 
           fill = viridis(8, option = "E")[c(1:5,8)], alpha = 0.9, colour = NA) +

  #Polygon/rectangle for no target:
  annotate("polygon", x = c(1.875, 1.875, 2, 2), y = c(22620, 25528, 25063, 23086), 
           fill = "grey10", alpha = 0.9, colour = NA) +
  annotate("rect", xmin = 2, xmax = 2.125, ymin = 23068, ymax = 25063, 
           fill = "grey10", alpha = 0.9, colour = NA) +
  
  #Polygon/rectangle for targets 1 & 2:
  annotate("polygon", x = c(1.875, 1.875, 2, 2), y = c(9102, 22620, 18890, 12832), 
           fill = "grey90", alpha = 0.9, colour = NA) +  
  annotate("rect", xmin = 2, xmax = 2.125, ymin = 12832, ymax = 18890, 
           fill = "grey90", alpha = 0.9, colour = NA) +
  
  ## Add labels:
  geom_text(stat = "stratum", infer.label = FALSE, size = 1.6, fontface = "bold",  
            colour = c(rep("grey10", 4), rep("grey90", 4), rep("grey10", 5), "grey90"),
            label = c("Other (2,055)", 
                      "Climate change & severe\nweather (1,339)",
                      "Pollution (1,472)", 
                      "Natural system\nmodifications (1,517)", 
                      "Invasive & other pro-\nblematic species (1,926)", 
                      "Residential & commercial\ndevelopment (2,321)", 
                      "Agriculture & aquaculture\n(4,447)", 
                      "Biological resource use\n(4,596)", 
                      "Target 6 - Climate\nchange (1,339)", 
                      "Target 5 - Harvesting &\ntrade (4,596)", 
                      "Target 4 - Pollution\n(1,472)", 
                      "Target 3 - Invasive\nspecies (1,695)", 
                      "Target 1 & 2 -\nEcosystems & protected\nareas (6,058)",
                      "Not addressed by targets\n(1,977)")) +
  scale_x_discrete(limits = c("Threat", "Post-2020 Framework"), name = "", expand = c(.001, 0)) + 
  scale_y_continuous(name = "Number of species", expand = c(0.001, 0)) + 
  scale_fill_viridis_d(option = "E", direction = 1) +
  theme_classic() +
  theme(legend.position = "none",
        text = element_text(size = 9),
        axis.ticks.x = element_blank(),
        plot.margin = unit(c(0, 0.2, -0.3, 0.3), "cm")) ->
  a



## -------------------------------------- Map -----------------------------------####

folders <- list.files("data/rl_download_12_05_2020/") ## make list of files in folder
countries <- data.frame()


for(i in 1:length(folders)) {
  countr <- read.csv(paste("data/rl_download_12_05_2020/", folders[i], "/countries.csv", 
                           sep = ""), na.string = c("", "NA"))
  countries <- bind_rows(countries, countr)
}



## Load spp files:

## spp matched to targets:

spptar <- read.csv("data/spp_tar.csv") 

spptar <- spptar %>% 
  filter(is.na(target)) %>% 
  select(scientificName) %>% 
  unique()


## Add col to indicate these spp have threats not matched by targets:
spptar$spptar <- "yes"



## Spp that need threat abatement and actions:

sppthract <- read.csv("data/spp_needing_thr_aba_act.csv") 
sppthract$sppthract <- "yes"



## Summary of all spp that need an additional spp target:

summaries <- read.csv("data/simple_summaries.csv")

summaries <- summaries %>% 
  left_join(spptar, by = "scientificName") %>% 
  left_join(sppthract, by = "scientificName") %>% 
  filter(spptar == "yes" | sppthract == "yes")



## Calculate no of spp needing additional target:

summaries %>% select(scientificName) %>% unique() %>%  nrow()
2921 / 7313 * 100
2921 / 36602 * 100

## Sort country level data


countriesmatch <- read.csv("data/countrymatching.csv")

## Retain relevant categories only:

countries <- countries %>% 
  filter(origin %in% c("Native", "Reintroduced")) %>% 
  filter(presence %in% c("Extant", "Possibly Extant", "Presence Uncertain", 
                         "Possibly Extinct")) %>% 
  select(scientificName, name) %>% 
  unique() %>% 
  full_join(countriesmatch, by = "name") %>% 
  select(-name) %>% 
  filter(!is.na(region)) %>% 
  unique()


## Merge w relevant spp and count:
summaries <- left_join(summaries, countries, by = "scientificName")
spp_cou <- summaries %>% 
  select(scientificName, region) %>% 
  unique() %>% 
  count(region)

# Check distribution:
spp_cou %>% ggplot(aes(x = n)) + geom_histogram()
median(spp_cou$n)


## Map


map.all <- map_data(map = "world")
map.all <- full_join(map.all, spp_cou, by = "region") 

map.all %>% select(region, n) %>%  unique() %>% arrange(-n)


ggplot() + 
  geom_map(data = map.all, map = map.all, 
           aes(map_id = region, x = long, y = lat, fill = n), colour = "black", size = 0.3) + 
  scale_fill_distiller(palette = "YlOrRd", direction = 1, name = "N") +
  coord_proj("+proj=cea +lat_ts=37.5") +
  labs(tag = "", x = "", y = "") + 
  guides(colour = "none") +
  theme_void() +
  theme(text = element_text(size = 7),
        legend.key.size = unit(0.4, "cm"),
        plot.margin = unit(c(0, 0, 0, 0.5), "cm")) ->
  b


plot_grid(a, b, ncol = 1, labels = "AUTO", rel_heights = c(1.5, 1))

ggsave("figures/figure1.png", width = 12, height = 15, unit = "cm", dpi = 300)



## Which countries have most of those spp:
map.all %>% 
  select(region, n) %>% 
  unique() %>% 
  filter(n > 250 & !is.na(region))

