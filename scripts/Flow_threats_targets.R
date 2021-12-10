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
target3 <- read.csv("data/target3_eligible.csv")
target3 <- target3 %>% 
  filter(actions == "yes") %>% 
  select(scientificName)
target3$target <- "Target 3"
target3$thr_lev1name <- "Additional actions required"

threats <- bind_rows(threats, target3)

### Remove threat Geological events as they cannot be addressed by conservation action/policy
threats <- threats %>% 
  filter(!thr_lev2 %in% c(10.1, 10.2, 10.3)) %>% 
  select(scientificName, thr_lev1name, target) %>% 
  unique()

  

### Add threats Transportation & service corridors, Energy Production & Mining, 
### Human intrusions & disturbance, to "other" as less than 5% of observations in each:
threats %>% select(scientificName, thr_lev1name) %>% unique() %>% count(thr_lev1name) %>% 
  mutate(perc = n/sum(n)*100) %>% arrange(-perc)

threats$thr_lev1name[threats$thr_lev1name %in% c("Transportation & service corridors", 
                                                 "Energy production & mining", 
                                                 "Human intrusions & disturbance")] <- 
  c("Other")


### Count how many spp are affected by threats tackled in each target
threats %>% 
  group_by(target, thr_lev1name) %>% 
  count() ->
  threats_summ


### Reorder factors depending on size of threat, or numerical order of Target
threats %>% select(scientificName, thr_lev1name) %>% unique() %>% 
  count(thr_lev1name) %>% arrange(-n)
threats_summ$thr_lev1name <- factor(threats_summ$thr_lev1name)
levels(threats_summ$thr_lev1name)
threats_summ$thr_lev1name <- 
  factor(threats_summ$thr_lev1name, 
         levels(threats_summ$thr_lev1name)[c(3, 2, 9, 5, 6, 4, 8, 7, 1)])


## ---------------------------------------- Figure 1 ----------------------------------####



## Set coordinates for polygons:
x <- c(1, 1, 1.122, 1.122)


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
## Spp addressed by tar 1 + 2:
sum(tar$all) - (tar[1,4]/2) #top of rectangle
sum(tar$all) - tar[1,2] + (tar[1,4]/2) #bottom of rectangle
sum(tar$all) #top of polygon
sum(tar$all) - tar[1,2] #bottom of polygon

## Spp addressed by targets 3:
sum(tar$all) - tar[1,2] - (tar[2,4]/2) #top of rectangle
sum(tar$all) - tar[1,2] - tar[2,2] + (tar[2,4]/2) #bottom of rectangle
sum(tar$all) - tar[1,2] #top of polygon
sum(tar$all) - tar[1,2] - tar[2,2] #bottom of polygon


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


for (i in 1:9) {
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
  geom_alluvium(aes(fill = thr_lev1name), alpha = 0.7, aes.bind = TRUE, width = 1/4) +
  geom_stratum(size = 0.5, colour = "white", width = 1/4, 
               fill = c(NA, "white", "white", NA, rep("white", 5), rep("grey90", 4), "white", "white")
               ) +
  ## Polygons for threats:
  annotate("polygon", x = x, y = c(thr$ymin[1], thr$ymax[1], thr$y2[1], thr$y1[1]), 
           fill = viridis(9, option = "E")[1], alpha = 0.7, colour = NA) + 
  annotate("polygon", x = x, y = c(thr$ymin[2], thr$ymax[2], thr$y2[2], thr$y1[2]), 
           fill = viridis(9, option = "E")[2], alpha = 0.7, colour = NA) +  
  annotate("polygon", x = x, y = c(thr$ymin[3], thr$ymax[3], thr$y2[3], thr$y1[3]), 
           fill = viridis(9, option = "E")[3], alpha = 0.7, colour = NA) +  
  annotate("polygon", x = x, y = c(thr$ymin[4], thr$ymax[4], thr$y2[4], thr$y1[4]), 
           fill = viridis(9, option = "E")[4], alpha = 0.7, colour = NA) +  
  annotate("polygon", x = x, y = c(thr$ymin[5], thr$ymax[5], thr$y2[5], thr$y1[5]), 
           fill = viridis(9, option = "E")[5], alpha = 0.7, colour = NA) +  
  annotate("polygon", x = x, y = c(thr$ymin[6], thr$ymax[6], thr$y2[6], thr$y1[6]), 
           fill = viridis(9, option = "E")[7], alpha = 0.7, colour = NA) +  
  annotate("polygon", x = x, y = c(thr$ymin[7], thr$ymax[7], thr$y2[7], thr$y1[7]), 
           fill = viridis(9, option = "E")[8], alpha = 0.7, colour = NA) + 
  
  # Rectangles for threats:  
  annotate("rect", xmin = 0.875, xmax = 1, ymin = thr$ymin, ymax = thr$ymax, 
           fill = viridis(9, option = "E")[c(1:5, 7, 8)], alpha = 0.7, colour = NA) +
  
  #Polygon/rectangle for targets 1 & 2:
  annotate("polygon", x = c(1.878, 1.878, 2, 2), y = c(16878, 37459, 30851, 23486), 
           fill = "grey90", alpha = 0.9, colour = NA) +  
  annotate("rect", xmin = 2, xmax = 2.125, ymin = 23486, ymax = 30851, 
           fill = "grey90", alpha = 0.9, colour = NA) +
  
  #Polygon/rectangle for target 3:
  annotate("polygon", x = c(1.878, 1.878, 2, 2), y = c(10259, 16878, 15586, 11551), 
           fill = "grey90", alpha = 0.9, colour = NA) +
  annotate("rect", xmin = 2, xmax = 2.125, ymin = 11551, ymax = 15586, 
           fill = "grey90", alpha = 0.9, colour = NA) +
  
  ## Add labels:
  geom_text(stat = "stratum", infer.label = FALSE, size = 3, lineheight = 0.8, 
            colour = "black" ,
            label = c("Additional actions\nrequired (1,863)",
                      "Other (3,028)", 
                      "Pollution (1,621)", 
                      "Climate change &\nsevere weather\n(1,658)",
                      "Natural system\nmodification (1,701)", 
                      "Invasive & other\nproblematic\nspecies (2,258)", 
                      "Residential &\ncommercial\ndevelopment\n(2,499)", 
                      "Agriculture &\naquaculture\n(4,730)",
                      "Biological resource\nuse (4,981)", 
                      "Target 7 - Climate\nchange (1,658)",
                      "Target 6 - Pollution\n(1,621)",
                      "Target 5 - Invasive\nspecies (1,999)", 
                      "Target 4 -\nHarvesting \ntrade (4,981)", 
                      "Target 3 - Manage\nspecies for\nrecovery (3,424)",
                      "Target 1 & 2 -\nEcosystems &\nprotected areas\n(7,365)")) +
  scale_x_discrete(limits = c("Threat", "Proposed targets"), name = "", expand = c(.001, 0)) + 
  scale_y_continuous(name = "Number of species", expand = c(0.001, 0), 
  label = scales::comma) + 
  scale_fill_viridis_d(option = "E", direction = 1) +
  theme_classic() +
  theme(legend.position = "none",
        text = element_text(size = 11),
        axis.ticks.x = element_blank(),
        axis.text = element_text(colour = "black"))
ggsave("figures/fig1.tiff", width = 6, height = 6.5, dpi = 300)


## -------------------------------------- Map -----------------------------------####

## Get  spp needing target 3:

tar3 <- threats %>% 
  filter(target == "Target 3") %>% 
  select(scientificName) %>% 
  unique()


## Sort country level data

folders <- list.files("data/rl_download_29_03_2021/") ## make list of files in folder
countries <- data.frame()

for(i in 1:length(folders)) {
  countr <- read.csv(paste("data/rl_download_29_03_2021/", folders[i], "/countries.csv", 
                           sep = ""), na.string = c("", "NA"))
  countries <- bind_rows(countries, countr)
}


countriesmatch <- read.csv("data/countrymatching.csv")

## Retain relevant categories only:

countries <- countries %>% 
  filter(origin %in% c("Native", "Reintroduced")) %>% 
  filter(presence %in% c("Extant", "Possibly Extant", "Presence Uncertain", 
                         "Possibly Extinct")) %>% 
  select(scientificName, name) %>% 
  unique()


## Match w full country list for map:
countries <- full_join(countries, countriesmatch, by = "name") 


## Count spp per country + calc median:

countries %>% select(scientificName, region) %>% 
  filter(scientificName %in% tar3$scientificName) %>% unique() %>% count(region) %>% 
  summarise(median(n))


countries <- countries %>% 
  select(-name) %>% 
  filter(!is.na(region)) %>% 
  filter(scientificName %in% tar3$scientificName) %>% 
  unique() %>% 
  count(region)

countries %>% filter(n > 300) %>% arrange(-n)

## Map


map.all <- map_data(map = "world")
map.all <- full_join(map.all, countries, by = "region") 

map.all %>% select(region, n) %>%  unique() %>% filter(is.na(n))


ggplot() + 
  geom_map(data = map.all, map = map.all, 
           aes(map_id = region, x = long, y = lat, fill = n), colour = "black", size = 0.3) + 
  scale_fill_distiller(palette = "YlOrRd", direction = 1, name = "Species\nrequiring\nTarget 4") +
  coord_proj("+proj=cea +lat_ts=37.5") +
  labs(tag = "", x = "", y = "") + 
  guides(colour = "none") +
  theme_void() +
  theme(text = element_text(size = 9))

ggsave("figures/fig2.tiff", width = 6, height = 3.1, dpi = 300)




############## Supplementary map #######################################


all <- read.csv("data/all_summaries.csv")

## Sort country level data

folders <- list.files("data/rl_download_29_03_2021/") ## make list of files in folder
countries <- data.frame()

for(i in 1:length(folders)) {
  countr <- read.csv(paste("data/rl_download_29_03_2021/", folders[i], "/countries.csv", 
                           sep = ""), na.string = c("", "NA"))
  countries <- bind_rows(countries, countr)
}


countriesmatch <- read.csv("data/countrymatching.csv")

## Retain relevant categories only:

countries <- countries %>% 
  filter(origin %in% c("Native", "Reintroduced")) %>% 
  filter(presence %in% c("Extant", "Possibly Extant", "Presence Uncertain", 
                         "Possibly Extinct")) %>% 
  select(scientificName, name) %>% 
  unique()


## Match w full country list for map:
countries <- full_join(countries, countriesmatch, by = "name") 


## Count spp per country + calc median:

countries %>% 
  select(scientificName, region) %>% 
  filter(scientificName %in% tar3$scientificName) %>% 
  unique() %>% 
  count(region) ->
  percent

countries %>% 
  select(scientificName, region) %>% 
  filter(scientificName %in% all$scientificName) %>% 
  unique() %>% 
  count(region) %>% 
  rename(allspp = n) ->
  percent2

percent <- percent %>% 
  full_join(percent2) %>% 
  mutate(perc = n / allspp * 100) %>% 
  filter(!is.na(region))


## Map


map.all <- map_data(map = "world")
map.all <- full_join(map.all, percent, by = "region") 

map.all %>% select(region, n) %>%  unique() %>% filter(is.na(n))


ggplot() + 
  geom_map(data = map.all, map = map.all, 
           aes(map_id = region, x = long, y = lat, fill = perc), colour = "black", size = 0.3) + 
  scale_fill_distiller(palette = "BuPu", 
                       direction = 1, 
                       name = "Percent of\nall species\nrequiring\nTarget 4",
                       values = c(0, 0.3, 1)) +
  coord_proj("+proj=cea +lat_ts=37.5") +
  labs(tag = "", x = "", y = "") + 
  theme_void() +
  theme(text = element_text(size = 9))

ggsave("figures/suppmap.tiff", width = 6, height = 3.1, dpi = 300)




