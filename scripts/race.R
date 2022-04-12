##creates five tables of metrics disaggregated by ethnicity

filter_ay <- 2016
library(data.table); library(tidyverse); library(zoo); library(scales)
full <- fread("ssm_sjcc.csv", header = "auto")
full$categoryID <- replace_na(full$categoryID, "")

#create ethnicity disaggregated dataset
race <- full %>% 
  mutate(years=paste(as.character(academicYear-1), as.character(academicYear), sep="-")) %>% 
  mutate(ids=paste(metricID, categoryID, sep=" ")) %>% 
  filter(disagg == "Ethnicity") %>% 
  select(academicYear, years, ids, title, description, metricID, categoryID, subgroup, value, perc)

#create course success table disagg by ethnicity
csrace <- race %>% 
  filter(metricID == "SM 408SW", academicYear > filter_ay) %>% 
  select(years, ids, title, description, subgroup, perc)

colnames(csrace) <- c('years', 'ids', 'title', 'description',
                      'Ethnicity', "value")

catids <- c(603, 631, 608, 614)

#table for grad/certs 
rgrads <- race %>% 
  filter(academicYear > filter_ay) %>% 
  select(years, ids, title, description, categoryID, subgroup, value) %>% 
  mutate_if(is.numeric, as.character) %>% 
  mutate_if(is.character, ~replace_na(., "--"))

colnames(rgrads) <- c('years', 'ids', 'title', 'description', 'categoryID',
                      'Ethnicity', "value")

#disaggregation by ethnicity for each metric
rgrads603 <- rgrads %>% 
  filter(categoryID == 603)
rgrads631 <- rgrads %>% 
  filter(categoryID == 631)
rgrads608 <- rgrads %>% 
  filter(categoryID == 608)
rgrads614 <- race %>% 
  filter(categoryID == 614, academicYear > filter_ay-1) %>% 
  select(years, ids, title, description, categoryID, subgroup, value) %>% 
  mutate_if(is.numeric, as.character) %>% 
  mutate_if(is.character, ~replace_na(., "--"))

colnames(rgrads614) <- c('years', 'ids', 'title', 'description', 'categoryID',
                      'Ethnicity', "value")
