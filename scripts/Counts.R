## ------------------------------------------------------------------------------------####
## -------------------- Counting different species groups -----------------------------####
## ------------------------------------------------------------------------------------####

library(tidyverse)

summaries <- read.csv("data/simple_summaries.csv")
nspp <- summaries %>% select(scientificName) %>% unique() %>% nrow()


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




## ------------------- Identifying spp which need emergency actions ------------------------####

summaries <- read.csv("data/simple_summaries.csv")

summaries <- summaries %>% 
  select(scientificName, kingdomName, phylumName, className, redlistCategory, 
         redlistCriteria) %>% 
  separate_rows(redlistCriteria, sep = ";") %>% 
  filter(!is.na(redlistCriteria))

summaries$criterion <- str_squish(summaries$redlistCriteria)

no_c <- count(summaries, scientificName)
summaries <- left_join(summaries, no_c, by = "scientificName")


a <- count(summaries, redlistCriteria)
## Create columns that indicate which spp fall into which category:

# Now excluded:
#summaries$C <- str_detect(summaries$redlistCriteria, "[C]")
#summaries$C2ai <- str_detect(summaries$redlistCriteria, "C2a\\(i\\)")

# Not working:
#summaries$Bac <- str_detect(summaries$criterion, "B.a[c]")
summaries$B <- str_detect(summaries$criterion, "B")
summaries$a <- str_detect(summaries$criterion, "[a]")
summaries$c <- str_detect(summaries$criterion, "[c]")
filter(summaries, B == TRUE & a == TRUE & c == TRUE)


test <- filter(summaries, redlistCriteria == "D" & n %in% c(1,2) | 
                 redlistCriteria == "D1" & n %in% c(1, 2) | 
                 B == TRUE & a == TRUE & c == TRUE & n %in% c(1,2)
)
n_test <- count(test, scientificName, name = "n_test")

test <- left_join(test, n_test, by = "scientificName")
test2 <- test %>% 
  filter(n == n_test) %>% 
  select(-n, -n_test, -B, -a, -c, -redlistCriteria) %>% 
  unique()


## figure out how to retain both criteria!!


## Compare to spp which have threats that are not being abated

threats <- read.csv("data/spp_tar.csv")
threats <- threats %>% 
  filter(is.na(target)) %>% 
  select(scientificName) %>% 
  unique()

test2 %>% 
  filter(scientificName %in% threats$scientificName) %>% 
  nrow() #
count(redlistCategory) ->
  a
count(test, redlistCategory) ->b
c <- full_join(a, b, by = "redlistCategory")
c <- mutate(c, perc = n.x / n.y * 100)


