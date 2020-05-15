# --------------------------- Making plot linking threats to targets ------------------------####

### Load packages
library(tidyverse)
library(ggalluvial)
library(viridis)
library(rworldmap)
library(ggalt)
library(gridExtra)

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


## --------------------------- Code for figure 1 --------------------------####
## ------------------------------- Option 1 -------------------------------####

ggplot(threats_summ, aes(axis1 = thr_lev1name, axis2 = target, y = n)) +
  geom_alluvium(aes(fill = thr_lev1name), alpha = 0.9, aes.bind = TRUE, width = 1/4) +
  geom_stratum(size = 0.5, colour = "white", width = 1/4, fill = 
                 c(rep(NA, 8), rep("grey90", 5), "grey10")) +
  geom_text(stat = "stratum", infer.label = FALSE, size = 2, fontface = "bold", min.y = 1, 
            colour = c(rep("grey10", 4), rep("grey90", 4), rep("grey10", 5), "grey90"),
            label = c("Other (2,055)", 
                      "Climate change & severe\nweather (1,339)",
                      "Pollution (1,472)", 
                      "Natural system modifications\n(1,517)", 
                      "Invasive & other problematic\nspecies (1,926)", 
                      "Residential & commercial\ndevelopment (2,321)", 
                      "Agriculture & aquaculture\n(4,447)", 
                      "Biological resource use (4,596)", 
                      "Target 6 - Climate change\n(1,339)", 
                      "Target 5 - Harvesting & trade\n(4,596)", 
                      "Target 4 - Pollution (1,472)", 
                      "Target 3 - Invasive species\n(1,695)", 
                      "Target 1 & 2 - Ecosystems &\nprotected areas (6,058)",
                      "Not addressed by targets\n(1,977)")) +
  scale_x_discrete(limits = c("Threat", "Post-2020 Framework"), name = "", expand = c(.001, 0)) + 
  scale_y_continuous(name = "Number of species", expand = c(0.001, 0)) + 
  scale_fill_viridis_d(option = "E", direction = 1) +
  theme_classic() +
  theme(legend.position = "none",
        text = element_text(size = 9),
        axis.ticks.x = element_blank())

ggsave("figures/flow_option1.png", width = 7, height = 5, dpi = 600)


## ------------------------------- Option 2 -------------------------------####

x <- c(1, 1, 1.124, 1.124)


threats_summ %>% 
  group_by(target) %>% 
  summarise(all = sum(n)) ->
  tar_all

threats %>% 
  select(scientificName, target) %>% 
  unique() %>% 
  count(target) ->
  tar_n

tar <- full_join(tar_all, tar_n, by = "target")
tar <- mutate(tar, double = all - n)

# Calculate values
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


threats_summ %>% 
  group_by(thr_lev1name) %>% 
  summarise(all = sum(n)) ->
  thr_all

threats %>% 
  select(scientificName, thr_lev1name) %>% 
  unique() %>% 
  count(thr_lev1name) ->
  thr_n

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
  geom_text(stat = "stratum", infer.label = FALSE, size = 2, fontface = "bold",  
            colour = c(rep("grey10", 4), rep("grey90", 4), rep("grey10", 5), "grey90"),
            label = c("Other (2,055)", 
                      "Climate change & severe\nweather (1,339)",
                      "Pollution (1,472)", 
                      "Natural system modifications\n(1,517)", 
                      "Invasive & other problematic\nspecies (1,926)", 
                      "Residential & commercial\ndevelopment (2,321)", 
                      "Agriculture & aquaculture\n(4,447)", 
                      "Biological resource use (4,596)", 
                      "Target 6 - Climate change\n(1,339)", 
                      "Target 5 - Harvesting & trade\n(4,596)", 
                      "Target 4 - Pollution (1,472)", 
                      "Target 3 - Invasive species\n(1,695)", 
                      "Target 1 & 2 - Ecosystems &\nprotected areas (6,058)",
                      "Not addressed by targets\n(1,977)")) +
  scale_x_discrete(limits = c("Threat", "Post-2020 Framework"), name = "", expand = c(.001, 0)) + 
  scale_y_continuous(name = "Number of species", expand = c(0.001, 0)) + 
  scale_fill_viridis_d(option = "E", direction = 1) +
  theme_classic() +
  theme(legend.position = "none",
        text = element_text(size = 9),
        axis.ticks.x = element_blank())

ggsave("figures/flow_option2.png", width = 7, height = 5, dpi = 600) 



ggplot(b, aes(x = fct_rev(target), y = n, fill = target)) + 
  geom_col() +
  scale_fill_manual(values = c("#053061", "#053061", "#053061", "#053061","#053061", "#d6604d")) +
  scale_x_discrete(labels = c("Threats not addressed\nby target",
                              "Target 6\nClimate change",
                              "Target 5\nHarvesting & trade",
                              "Target 4\nPollution",
                              "Target 3\nInvasive species",
                              "Target 1 & 2\nEcosystems &\nprotected areas")) +
  labs(x = "Zero Draft Target", y = "Number of species") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        text = element_text(size = 9)) ->
  plota
  






threats <- read.csv("data/spp_tar.csv")

### Remove threat Geological events as they cannot be addressed by conservation action/policy
threats <- threats %>% 
  filter(!thr_lev2 %in% c(10.1, 10.2, 10.3)) %>% 
  select(scientificName, thr_lev1name, target) %>% 
  unique()

c <- threats %>% group_by(thr_lev1name, target) %>% count()# %>% 
  #gather("type", "value", 1:2)

c$target[is.na(c$target)] <- "Not addressed by targets"  

ord <- c %>% 
  group_by(thr_lev1name) %>% 
  summarise(ord = sum(n))

c <- left_join(c, ord, by = "thr_lev1name")

c$thr_lev1name[c$thr_lev1name == "Invasive & other problematic species, genes & diseases"] <-
  c("Invasive & other problematic\nspecies, genes and diseases")

ggplot(c, aes(x = fct_reorder(thr_lev1name, ord), y = n, fill = target)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("#d6604d", "#053061", "#2166ac","#4393c3", "#92c5de","#d1e5f0"), 
                    name = "Zero Draft Targets",
                    labels = c("Not addressed\nby targets", 
                               "Target 1 & 2\nEcosystems &\nprotected areas",
                               "Target 3\nInvasive species", 
                               "Target 4\nPollution", 
                               "Target 5\nHarvesting & trade", 
                               "Target 6\nClimate change")) +
  labs(x = "Threat", y = "Number of species") +
  theme_classic() +
  theme(legend.position = c(0.8, 0.4),
        text = element_text(size = 9)) ->
  plotb

c <- grid.arrange(plotb, plota, nrow = 1)
ggsave("figures/option3.jpg", c, )




## -------------------------------------- Maps -----------------------------------####

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


## Spp that need actions only:

sppact <- read.csv("data/spp_needing_act.csv") 
sppact$sppact <- "yes"



summaries <- read.csv("data/simple_summaries.csv")

summaries <- summaries %>% 
  left_join(spptar, by = "scientificName") %>% 
  left_join(sppthract, by = "scientificName") %>% 
  left_join(sppact, by = "scientificName")



## Sort country level data


countriesmatch <- read.csv("data/countrymatching.csv")

## Retain relevant categories only:

countries <- countries %>% 
  filter(origin %in% c("Native", "Reintroduced")) %>% 
  filter(presence %in% c("Extant", "Possibly Extant", "Presence Uncertain")) %>% 
  select(scientificName, name) %>% 
  unique() %>% 
  full_join(countriesmatch, by = "name") %>% 
  select(-name) %>% 
  unique()

summaries <- left_join(summaries, countries, by = "scientificName")


## Count no of spp in different groups:

summaries %>% 
  filter(spptar == "yes") %>% 
  select(scientificName, name) %>% 
  unique() %>% 
  count(name, name = "n_tar") ->
  n_tar

summaries %>% 
  filter(sppthract == "yes") %>% 
  select(scientificName, name) %>% 
  unique() %>% 
  count(name, name = "n_thract") ->
  n_thract

summaries %>% 
  filter(sppact == "yes") %>% 
  select(scientificName, name) %>% 
  unique() %>% 
  count(name, name = "n_act") ->
  n_act

spp_cou <- n_tar %>% 
  full_join(n_thract, by = "name") %>% 
  full_join(n_act, by = "name") %>% 
  replace_na(list(n_tar = 0, n_thract = 0, n_act = 0)) %>% 
  rename(region = name)


map.all <- map_data(map = "world")
map.all <- full_join(map.all, spp_cou, by = "region") 




ggplot() + 
  geom_map(data = map.all, map = map.all, 
           aes(map_id = region, x = long, y = lat, fill = n_tar), colour = "black", size = 0.1) + 
  scale_fill_distiller(palette = "YlOrRd", direction = 1, name = "N") +
  #coord_proj("+proj=cea +lat_ts=37.5") +
  labs(tag = "", x = "", y = "", title = "Species with threats not addressed by targets") + 
  guides(colour = "none") +
  theme_void() +
  theme(text = element_text(size = 7),
        legend.key.size = unit(0.4, "cm")) ->
  a

ggplot() + 
  geom_map(data = map.all, map = map.all, 
           aes(map_id = region, x = long, y = lat, fill = n_thract), colour = "black", size = 0.1) + 
  scale_fill_distiller(palette = "YlOrRd", direction = 1, name = "N") +
  #coord_proj("+proj=cea +lat_ts=37.5") +
  labs(tag = "", x = "", y = "", title = "Species which need species-specific actions") + 
  guides(colour = "none") +
  theme_void() +
  theme(text = element_text(size = 7),
        legend.key.size = unit(0.4, "cm")) ->
  b

ggplot() + 
  geom_map(data = map.all, map = map.all, 
           aes(map_id = region, x = long, y = lat, fill = n_act), colour = "black", size = 0.1) + 
  scale_fill_distiller(palette = "YlOrRd", direction = 1, name = "N") +
  #coord_proj("+proj=cea +lat_ts=37.5") +
  labs(tag = "", x = "", y = "", title = "Species which need species-specific actions only") + 
  guides(colour = "none") +
  theme_void() +
  theme(text = element_text(size = 7),
        legend.key.size = unit(0.4, "cm")) ->
  c



d <- grid.arrange(a, b, c, ncol = 1)
ggsave("figures/map_options.jpg", d, width = 4, height = 8)

c <- ggdraw() +
  draw_plot(a) +
  draw_text("A", x = 0.02, y = 0.98)

d <- ggdraw() +
  draw_plot(b) +
  draw_text("B", x = 0.02, y = 0.98)

e <- grid.arrange(c, d)

ggsave("figures/figure1.png", e, width = 12, height = 15, unit = "cm", dpi = 300)


map.all %>% 
  select(region, n) %>% 
  unique() %>% 
  filter(n > 75 & !is.na(region)) %>% 
  ggplot(aes(x = fct_reorder(region, n), y = n)) + 
  geom_col() +
  coord_flip() +
  labs(x = "Country", y = "Number of species")
ggsave("figures/spp_per_country.jpg", height = 10, width = 7)






## ---------------------------- Map 2 -------------------------------####
folders <- list.files("data/rl_download_02_03_2020/") ## make list of files in folder
countries <- data.frame()


for(i in 1:length(folders)) {
  countr <- read.csv(paste("data/rl_download_02_03_2020/", folders[i], "/countries.csv", sep = ""), na.string = 
                       c("", "NA"))
  countries <- bind_rows(countries, countr)
}



countries %>% 
  filter(scientificName %in% test$scientificName) %>% 
  filter(origin %in% c("Native", "Reintroduced")) %>% 
  filter(presence %in% c("Extant", "Possibly Extant", "Presence Uncertain")) %>% 
  select(scientificName, name) %>% 
  unique() %>% 
  count(name) ->
  countrysum

countriesmatch <- read.csv("data/countrymatching.csv")
countrysum <- countrysum %>% 
  full_join(countriesmatch, by = "name") %>% 
  select(-name) %>% 
  unique()




map.all <- map_data(map = "world")
map.all <- full_join(map.all, countrysum, by = "region") 




ggplot() + 
    geom_map(data = map.all, map = map.all, 
             aes(map_id = region, x = long, y = lat, fill = n), colour = "black", size = 0.1) + 
    #scale_fill_distiller(palette = "YlOrRd", direction = 1) +
    scale_fill_gradient2(low = "#ffffb2", high = "#bd0026", mid = "#fd8d3c", midpoint = 60, name = "Number\nof species") +
    #coord_proj("+proj=cea +lat_ts=37.5") +
    #labs(tag = "b)", x = "", y = "") + 
    guides(colour = "none") +
    theme_void() +
    theme(text = element_text(size = 7),
          legend.key.size = unit(0.4, "cm"))
ggsave("map.jpg", width = 6, height = 4)


c <- ggdraw() +
  draw_plot(a) +
  draw_text("A", x = 0.02, y = 0.98)

d <- ggdraw() +
  draw_plot(b) +
  draw_text("B", x = 0.02, y = 0.98)

e <- grid.arrange(c, d)

ggsave("figures/figure1.png", e, width = 12, height = 15, unit = "cm", dpi = 300)


map.all %>% 
  select(region, n) %>% 
  unique() %>% 
  filter(n > 75 & !is.na(region)) %>% 
  ggplot(aes(x = fct_reorder(region, n), y = n)) + 
  geom_col() +
  coord_flip() +
  labs(x = "Country", y = "Number of species")
ggsave("figures/spp_per_country.jpg", height = 10, width = 7)




## --------------------------- Code for figure 2 --------------------------####


### Count how many threats in each target
threats %>% 
  group_by(target, stress, tnames) %>% 
  summarise(n_new = sum(n)) ->
  threats_str_summ


### Reorder factors depending on size of threat, or numerical order of Target
threats_str_summ %>% group_by(tnames) %>% summarise(sum = sum(n_new)) %>% arrange(-sum)
levels(threats_str_summ$tnames)
threats_str_summ$tnames <- factor(threats_str_summ$tnames, levels(threats_str_summ$tnames)[c(2, 1, 7, 11, 3, 10, 8, 9, 12, 4, 5, 6)])



ggplot(threats_str_summ, aes(axis1 = tnames, axis2 = target, y = n_new)) +
  geom_alluvium(aes(fill = stress), alpha = 0.9, aes.bind = TRUE, width = 1/4) +
  geom_stratum(size = 0.5, colour = "grey95", width = 1/4, fill = "grey90") +
  geom_text(stat = "stratum", #infer.label = TRUE, 
            size = 2.2, fontface = "bold", min.y = 1000, 
            label = c("Other\n(4,705)", "Natural system modifications\n(2,683)", "Pollution\n(4,221)", 
                      "Climate change & severe\nweather (4,318)", "Residential & commercial\ndevelopment\n(4,869)", 
                      "Invasive & other\nproblematic species\n(5,873)", 
                      "Agriculture & aquaculture\n(9,284)", "Biological resource use\n(13,815)", 
                      "Target 6 - Climate change\n(4,318)", "Target 5 - Harvesting & trade\n(9,009)", 
                      "Target 4 - Pollution\n(4,221)", "Target 3 - Invasive species\n(3,864)", 
                      "Target 1 & 2 - Ecosystems\n& protected areas\n(22,488)")) +
  scale_x_discrete(limits = c("Threat", "Post-2020\nFramework"), name = "", expand = c(.001, 0)) + 
  scale_y_continuous(name = "Number of species", expand = c(0.001, 0)) + 
  scale_fill_viridis_d(option = "E", direction = 1, name = "Stress") +
  theme_classic() +
  theme(legend.position = c(0.5, -0.087),
        legend.direction = "horizontal",
        text = element_text(size = 10),
        axis.ticks.x = element_blank()) +
  annotate("rect", xmin = 1.875, xmax = 2.125, ymin = 43900, ymax = 49768, fill = "grey10", colour = "grey90") +
  annotate("text", x = 2, y = 46834, label = "Not addressed\nby targets\n(5,868)", size = 2.2, fontface = "bold", 
           colour = "grey95")

ggsave("figures/flow_option2.png", width = 7, height = 6, dpi = 600)



## --------------------------- No of spp --------------------------####

spp <- read.csv("spp_thr_str.csv")

## Make df with threats and stresses linked to targets and join

threats2 <- select(threats, thr_lev2, stress, target)
threats2 <- unique(threats2)
spp <- left_join(spp, threats2, by = c("thr_lev2", "stress"))


## Number of species which have at least 1 unaddressed threat:

spp %>% filter(is.na(target)) %>% select(scientificName) %>% unique() %>% nrow()
spp %>% select(scientificName) %>% unique() %>% nrow() ## this is no of all spp



## Number of species for which all threats are unaddressed:

## find all spp with unaddressed threats:
spp %>% filter(is.na(target)) %>% select(scientificName) %>% unique() -> a 

## Retain only those spp with unaddressed threats:
spp %>% filter(scientificName %in% a$scientificName) -> selspp

## Find spp for which all threats are unaddressed:
selspp %>% count(scientificName, target) %>% count(scientificName) %>% filter(n == 1) %>% nrow() 

## Retain only those spp for which all threats are unaddressed:
selspp %>% count(scientificName, target) %>% count(scientificName) %>% filter(n == 1) #-> b

spp %>% filter(scientificName %in% b$scientificName) %>% select(scientificName, thr2_name) %>% unique() %>% 
  count(scientificName) %>% count(n)



## Spp benefitting from different targets:
#No:
spp %>% select(scientificName, target) %>% unique() %>% count(target) %>% arrange(-n)

#%:
spp %>% select(scientificName, target) %>% unique() %>% count(target) %>% mutate(n = n/9191*100) %>% arrange(-n)
