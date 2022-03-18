##creates five tables of metrics disaggregated by gender

#vector of relevant category ids (603 = cert, 631 = degree, 608 = adt)
catids <- c(603, 631, 608, 614)
filter_ay <- 2016

#read table
library(data.table); library(tidyverse); library(zoo); library(scales)
full <- fread("ssm_sjcc.csv", header = "auto")
#delete n/as in categoryID
full$categoryID <- replace_na(full$categoryID, "")

#create gender disaggregated dataset
gender <- full %>% 
  mutate(years=paste(as.character(academicYear-1), as.character(academicYear), sep="-")) %>% 
  mutate(ids=paste(metricID, categoryID, sep=" ")) %>% 
  filter(disagg == "Gender") %>% 
  select(academicYear, years, ids, title, description, metricID, categoryID, subgroup, value, perc)

#course success by gender
csgender <- gender %>% 
  filter(metricID == "SM 408SW", academicYear > filter_ay) %>% 
  select(years, ids, title, description, subgroup, perc)

colnames(csgender) <- c('years', 'ids', 'title', 'description', 'categoryID',
                      'Gender', "value")


#graduation/certificates by gender
ggrads <- gender %>% 
  filter(academicYear > filter_ay) %>% 
  select(years, ids, title, description, categoryID, subgroup, value)

colnames(ggrads) <- c('years', 'ids', 'title', 'description', 'categoryID',
                        'Gender', "value")


#put each metric into a separate tibble
#each metric is now disaggregated by gender
ggrads603 <- ggrads %>% 
  filter(categoryID == 603)
ggrads631 <- ggrads %>% 
  filter(categoryID == 631)
ggrads608 <- ggrads %>% 
  filter(categoryID == 608)
ggrads614 <- gender %>% 
  filter(categoryID == 614, academicYear > filter_ay-1) %>% 
  select(years, ids, title, description, categoryID, subgroup, value)

colnames(ggrads614) <- c('years', 'ids', 'title', 'description', 'categoryID',
                        'Gender', "value")
