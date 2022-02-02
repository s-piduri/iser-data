###
#try 

#################
filter_ay <- 2014
filter_disagg <- "Overall"
goal_col <- "2015"
allyear <- unique(full$academicYear)
allyears <- vector()
for(i in 1:6) {
  allyears[i] = toString(allyear[i])
}
#################


#import packages, read in full csv file
library(data.table); library(tidyverse)
full <- fread("ssm_sjcc.csv", header = "auto")

#alternative
overall <- full %>% 
  filter(disagg == filter_disagg, academicYear > filter_ay) %>% 
  select(academicYear, metricID, categoryID, value, perc)

#create row for actual course success metrics
coursesuccess <- overall %>% 
  filter(metricID == "SM 408SW") %>% 
  select(academicYear, perc) %>% 
  pivot_wider(names_from=academicYear, values_from = perc)

#vector of relevant category ids (603 = cert, 631 = degree, 608 = adt)
catids <- c(603, 631, 608, 614)

#table for degree and cert earners
grads <- overall %>% 
    filter(categoryID %in% catids) %>% 
    select(academicYear, categoryID, value) %>% 
    pivot_wider(names_from=academicYear, values_from = value)


#add in rows in coursesuccess for aspirational and floor goals
coursesuccess <- add_column(coursesuccess, Goal = "Actual", .before = allyears[1])
