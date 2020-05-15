## --------------------------------------------------------------------------------------------##
## ----------------------- Counting different species groups --------------------------------####
## --------------------------------------------------------------------------------------------##

library(tidyverse)
library(gridExtra)

summaries <- read.csv("data/simple_summaries.csv")
nspp <- summaries %>% select(scientificName) %>% unique() %>% nrow()


## ----------------------- Counts threats, targets, actions --------------------------------####


## Count spp threatened by each threat (level 1):

thr_str <- read.csv("data/spp_tar.csv")
thr_str %>% 
  select(scientificName, thr_lev1name) %>% 
  unique() %>% 
  count(thr_lev1name) %>% 
  mutate(perc1 = n / nspp * 100) %>% ## calculate % of threatened/EW spp
  arrange(-n)


## Percent/Count spp benefitting from each target:
thr_str <- read.csv("data/spp_tar.csv")
thr_str %>% 
  select(scientificName, target) %>% 
  unique() %>% 
  count(target) %>% 
  mutate(perc1 = n / nspp * 100) %>% ## calculate % of threatened/EW spp
  arrange(-n)


## Percent of spp benefitting from different actions:
act <- read.csv("data/actions_needed_tidy.csv")
act %>% 
  select(scientificName, name) %>% 
  unique() %>% 
  count(name) %>% 
  mutate(perc1 = n / nspp * 100) %>% ## calculate % of threatened/EW spp
  arrange(-n)




## ----------------- Identifying spp for which threats can't be tackled --------------------####


## Count corals threatened by temperature extremes:

thr_str <- read.csv("data/spp_tar.csv")
thr_str %>%  
  left_join(summaries, by = "scientificName") %>% 
  filter(phylumName == "CNIDARIA" & thr_lev2 == "11.3") %>% 
  select(scientificName) %>% 
  unique() %>% 
  nrow()


## Count amphibians threatened by chytrid disease:
threats <- read.csv("data/threats.csv")
threats %>% 
  left_join(summaries, by = "scientificName") %>% 
  filter(className == "AMPHIBIA" & ias == "Batrachochytrium dendrobatidis") %>% 
  select(scientificName) %>% 
  unique() %>% 
  nrow()




## ------------------- Identifying spp which need emergency actions ------------------------####


summaries <- read.csv("data/simple_summaries.csv")

## Turn into one row per criterion:

summaries <- summaries %>% 
  select(scientificName, kingdomName, phylumName, className, redlistCategory, 
         redlistCriteria) %>% 
  separate_rows(redlistCriteria, sep = ";") %>% 
  filter(!is.na(redlistCriteria))


## Remove empty spaces:

summaries$redlistCriteria <- str_squish(summaries$redlistCriteria)


## Add columns that extract criteria:

summaries$C <- str_detect(summaries$redlistCriteria, "C") ## any that contain C
summaries$C2ai <- str_detect(summaries$redlistCriteria, "C2a\\(i[^ii]") ##any that contain C2a(i,
## but not C2a(ii
summaries$D <- str_detect(summaries$redlistCriteria, "^D$") ##any exact matches to D
summaries$D1 <- str_detect(summaries$redlistCriteria, "^D1$") ##any exact matches to D1


summaries$Bac <- str_detect(summaries$redlistCriteria, "^B.a.*?c") ##any that start with B, followed by
## any character, followed by a. *?c means anything in between, then c



## -------------- Spp which need threat abatement AND emergency actions -------------------####

suma <- summaries

suma <- filter(suma, Bac == TRUE | C == TRUE & redlistCategory == "Critically Endangered" |
                 C2ai == TRUE | D1 == TRUE & redlistCategory == "Vulnerable" |
                 D == TRUE & redlistCategory %in% c("Critically Endangered", "Endangered"))


suma %>% select(scientificName) %>% unique() %>% nrow() / nspp * 100



suma %>% 
  group_by(redlistCategory, Bac, C, C2ai, D, D1) %>% 
  count() %>% 
  gather(-redlistCategory, -n, key = "crit", value = "TF") %>% 
  filter(TF == TRUE) %>% 
  ggplot(aes(x = redlistCategory, y = n, fill = crit)) +
  geom_col()

## Check taxonomy of those spp:

class <- count(summaries, className, name = "Countall")
class2 <- count(suma, className, name = "Countemer")
class <- class %>% 
  full_join(class2, by = "className") %>% 
  replace_na(list(Countall = 0, Countemer = 0)) %>% 
  mutate(perc = Countemer / Countall * 100)

a <- ggplot(class, aes(x = fct_reorder(className, perc), y = perc)) + geom_col() + 
  coord_flip(ylim = c(0, 100)) + labs(x = "", y = "Percent")
b <- ggplot(class, aes(x = fct_reorder(className, Countemer), y = Countemer)) + geom_col() + 
  coord_flip()  + labs(x = "", y = "Number of species")
grid.arrange(a, b, nrow = 1)  


suma %>% 
  select(scientificName) %>% 
  unique() %>% 
  write_csv("data/spp_needing_thr_aba_act.csv")


## -------------- Spp which need emergency actions only -------------------####

## Count total criteria per spp:
no_c <- count(summaries, scientificName, name = "allcrit")


## Count the selected criteria per spp:
no_a <- count(suma, scientificName, name = "selcrit")


## Combine, calculate difference, and retain only spp where nos are the same:
sumb <- suma %>% 
  left_join(no_c, by = "scientificName") %>% 
  left_join(no_a, by = "scientificName") %>% 
  mutate(diff = allcrit - selcrit) %>% 
  filter(diff == 0)

sumb %>% 
  select(scientificName) %>% 
  unique() %>% 
  write_csv("data/spp_needing_act.csv")




