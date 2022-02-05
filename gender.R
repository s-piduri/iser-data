#creates a table of course success disaggregated by gender

#vector of relevant category ids (603 = cert, 631 = degree, 608 = adt)
catids <- c(603, 631, 608, 614)

filter_ay <- 2016
library(data.table); library(tidyverse); library(zoo); library(scales)
full <- fread("ssm_sjcc.csv", header = "auto")
full$categoryID <- replace_na(full$categoryID, "")

#alternative
gender <- full %>% 
  mutate(years=paste(as.character(academicYear-1), as.character(academicYear), sep="-")) %>% 
  mutate(ids=paste(metricID, categoryID, sep=" ")) %>% 
  filter(disagg == "Gender", academicYear > filter_ay) %>% 
  select(academicYear, years, ids, title, description, metricID, categoryID, subgroup, value, perc)

#course success by gender
csgender <- gender %>% 
  filter(metricID == "SM 408SW") %>% 
  select(years, ids, title, description, subgroup, perc)

ggrads <- gender %>% 
  filter(categoryID %in% catids) %>% 
  select(years, ids, title, description, categoryID, subgroup, value)

ggrads603 <- ggrads %>% 
  filter(categoryID == 603)
ggrads614 <- ggrads %>% 
  filter(categoryID == 614)
ggrads608 <- ggrads %>% 
  filter(categoryID == 608)
ggrads631 <- ggrads %>% 
  filter(categoryID == 631)