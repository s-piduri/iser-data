###
#try 

#################
filter_ay <- 2017


#################


#import packages, read in full csv file
library(data.table); library(tidyverse)
full <- fread("ssm_sjcc.csv", header = "auto")

#create table with only overall values
overall <- filter(full, disagg == "Overall", academicYear > 2017)
overall <- select(overall, academicYear, metricID, categoryID, value, perc)

#alternative
overall <- full %>% 
  filter(disagg == "Overall", academicYear > 2017) %>% 
  select(academicYear, metricID, categoryID, value, perc)



#create row for actual course success metrics
coursesuccess <- filter(overall, metricID == "SM 408SW") 
coursesuccess <- select(coursesuccess, academicYear, perc)
coursesuccess <- pivot_wider(coursesuccess, names_from=academicYear, values_from = perc)

#vector of relevant category ids (603 = cert, 631 = degree, 608 = adt)
catids <- c(603, 631, 608)

#table for degree and cert earners
grads <- filter(overall, categoryID %in% catids)
grads <- select(grads, academicYear, categoryID, value)
grads <- pivot_wider(grads, names_from=academicYear, values_from = value)

#table for uc/csu transfers - from 2015-16
csuuc <- filter(full, disagg == "Overall", academicYear > 2016, categoryID == 614)
csuuc <- select(csuuc, academicYear, categoryID, value)
csuuc <- pivot_wider(csuuc, names_from=academicYear, values_from = value)

#add in rows in coursesuccess for aspirational and floor goals
coursesuccess <- add_column(coursesuccess, Goal = "Actual", .before = "2018")
cs <- add_row(coursesuccess, Goal = "Floor", "2018" = .653, "2019"=.657, .before=1)

