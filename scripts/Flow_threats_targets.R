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


### Add threats Transportation & service corridors,Energy Production & Mining, 
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

ggplot(threats_summ, aes(axis1 = thr_lev1name, axis2 = target, y = n)) +
  geom_alluvium(aes(fill = thr_lev1name), alpha = 0.9, aes.bind = TRUE, width = 1/4) +
  geom_stratum(size = 0.5, colour = "grey20", width = 1/4, fill = 
                 c(NA, NA, NA, NA, NA, NA, NA, NA, 
                   "grey90", "grey90", "grey90", "grey90", "grey90", "grey90")) +
  geom_text(stat = "stratum", infer.label = TRUE, 
            size = 1.3, fontface = "bold", min.y = 1000, 
            colour = c("grey20", "grey20", "grey20", "grey20", 
                       "grey90", "grey90", "grey90", "grey90",
                       "grey20", "grey20", "grey20", "grey20", "grey20", "grey20"),
            label = c("Other\n(3,119)", 
                      "Climate change & severe\nweather (1,320)",
                      "Pollution\n(1,460)", 
                      "Natural system modifications\n(1,837)", 
                      "Invasive & other\nproblematic species\n(2,407)", 
                      "Residential & commercial\ndevelopment\n(2,492)", 
                      "Agriculture & aquaculture\n(4,882)", 
                      "Biological resource use\n(7,948)", 
                      "Target 6 - Climate change\n(4,318)", 
                      "Target 5 - Harvesting & trade\n(9,009)", 
                      "Target 4 - Pollution\n(4,221)", 
                      "Target 3 - Invasive species\n(3,864)", 
                      "Target 1 & 2 - Ecosystems\n& protected areas\n(13,506)", "")) +
  scale_x_discrete(limits = c("Threat", "Post-2020\nFramework"), name = "", expand = c(.001, 0)) + 
  scale_y_continuous(name = "Number of species", expand = c(0.001, 0)) + 
  scale_fill_viridis_d(option = "E", direction = 1) +
  theme_classic() +
  theme(legend.position = "none",
        text = element_text(size = 7),
        axis.ticks.x = element_blank()) #+
 # annotate("rect", xmin = 1.875, xmax = 2.125, ymin = 43900, ymax = 49768, fill = "grey30", colour = "grey20") +
  #annotate("text", x = 2, y = 46834, label = "Not addressed\nby targets\n(5,868)", size = 1.3, fontface = "bold", 
   #        colour = "grey90") #->
  #a

ggsave("figures/flow_option1.png", width = 7, height = 5, dpi = 600)




## ---------------------------- Map -------------------------------####
folders <- list.files("data/rl_download_02_03_2020/") ## make list of files in folder
countries <- data.frame()


for(i in 1:length(folders)) {
  countr <- read.csv(paste("data/rl_download_02_03_2020/", folders[i], "/countries.csv", sep = ""), na.string = 
                     c("", "NA"))
  countries <- bind_rows(countries, countr)
}

sppmatched <- read.csv("data/spp_thr_str.csv")

tarmatched <- read.csv("data/thr_str_tar_matched.csv")
sppmatched <- sppmatched %>% 
  left_join(tarmatched, by = c("thr_lev2", "stress", "thr2_name")) %>% 
  filter(is.na(target)) %>% 
  select(scientificName) %>% 
  unique()

summaries <- read.csv("data/simple_summaries.csv")

sppmatched <- left_join(sppmatched, summaries, by = "scientificName")

countries %>% 
  filter(scientificName %in% sppmatched$scientificName) %>% 
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




(ggplot() + 
  geom_map(data = map.all, map = map.all, 
           aes(map_id = region, x = long, y = lat, fill = n), colour = "black", size = 0.1) + 
  #scale_fill_distiller(palette = "YlOrRd", direction = 1) +
  scale_fill_gradient2(low = "#ffffb2", high = "#bd0026", mid = "#fd8d3c", midpoint = 220, name = "Number\nof species") +
  coord_proj("+proj=cea +lat_ts=37.5") +
  #labs(tag = "b)", x = "", y = "") + 
  guides(colour = "none") +
  theme_void() +
  theme(text = element_text(size = 7),
        legend.key.size = unit(0.4, "cm")) ->
  b)



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
  annotate("rect", xmin = 1.875, xmax = 2.125, ymin = 43900, ymax = 49768, fill = "grey30", colour = "grey95") +
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
