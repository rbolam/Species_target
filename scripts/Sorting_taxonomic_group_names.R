## -------------- Sort out comprehensively assessed groups data -------------------####

library(tidyverse)
comp <- read.csv("data/Comprehensive_Taxonomic_Groups.csv", na.strings = c("", NA))


## Remove " from some strings:
comp$Taxonomy <- str_remove_all(comp$Taxonomy, "\"")
comp <- separate(comp, Taxonomy, sep = "[=:]", into = c("tax1", "group1"))
comp <- separate_rows(comp, group1, sep = "[Oo][r] ")

comp <- separate(comp, Taxonomy2, sep = "[=:]", into = c("tax2", "group2"))
comp <- separate_rows(comp, group2, sep = "[Oo][r] ")

for (i in 2:5) {
  comp[,i] <-str_squish(comp[,i])
}

write.csv(comp, "comprehensive_groups.csv")
