##creates five tables of metrics disaggregated by ethnicity

filter_ay <- 2016
library(data.table); library(tidyverse); library(zoo); library(scales)
full <- fread("ssm_sjcc.csv", header = "auto")
full$categoryID <- replace_na(full$categoryID, "")

#create ethnicity disaggregated dataset
race <- full %>% 
  mutate(years=paste(as.character(academicYear-1), as.character(academicYear), sep="-")) %>% 
  mutate(ids=paste(metricID, categoryID, sep=" ")) %>% 
  filter(disagg == "Ethnicity", academicYear > filter_ay) %>% 
  select(academicYear, years, ids, title, description, metricID, categoryID, subgroup, value, perc)

#create course success table disagg by ethnicity
csrace <- race %>% 
  filter(metricID == "SM 408SW") %>% 
  select(years, ids, title, description, subgroup, perc)

catids <- c(603, 631, 608, 614)

#table for grad/certs 
rgrads <- race %>% 
  filter(categoryID %in% catids) %>% 
  select(years, ids, title, description, categoryID, subgroup, value)

#disaggregation by ethnicity for each metric
rgrads603 <- rgrads %>% 
  filter(categoryID == 603)
rgrads614 <- rgrads %>% 
  filter(categoryID == 614)
rgrads608 <- rgrads %>% 
  filter(categoryID == 608)
rgrads631 <- rgrads %>% 
  filter(categoryID == 631)