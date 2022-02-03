###
#try 

#################
filter_ay <- 2017
filter_disagg <- "Overall"
goal_col <- "2015"
#################

#import packages, read in full csv file
library(data.table); library(tidyverse); library(zoo); library(scales)
library(janitor)
full <- fread("ssm_sjcc.csv", header = "auto")

#alternative
overall <- full %>% 
  filter(disagg == filter_disagg) %>% 
  select(academicYear, metricID, categoryID, value, perc)

#create row for actual course success metrics
cslong <- overall %>% 
  filter(metricID == "SM 408SW") %>% 
  select(academicYear, perc) 

#get the floor goals
floor <- rollmean(cslong$perc, 3, align = "right")*.9
floor <- c(rep(0,2), floor)
cslong <- cslong %>% 
  add_column(floor)

#get the aspirational goals
#find the actual percentage corresponding to the first year being looked at
#take the whole row
cs_ay <- cslong %>% 
  filter(academicYear == filter_ay)
#grab the percentage
cs_base <- round(cs_ay$perc, digits = 2)
#set the increment to increase asp goal by each year
inc = .005

#filter to just the years being looked at
cslong <- cslong %>% 
  filter(academicYear >= filter_ay)

#get the number of years being looked at in total (from set starting year to end of data)
csceil <- cs_base+(inc*(length(cslong$academicYear)-1))

#create vector with aspirational goals
aspirational = seq(cs_base, csceil, by=inc)

#add aspirational goal to table
cslong <- cslong %>% add_column(aspirational)

##transpose table and add academic years as column names
cslong1 <- cslong %>% 
  column_to_rownames("academicYear")

cswide <- cslong1 %>% 
  t %>% 
  as.data.frame()

################################

#vector of relevant category ids (603 = cert, 631 = degree, 608 = adt)
catids <- c(603, 631, 608, 614)

#data for degree and cert earners
gradslong <- overall %>% 
    filter(categoryID %in% catids) %>% 
    select(categoryID, academicYear, value) %>% 
    group_by(categoryID) %>% 
    arrange(categoryID)

group_keys(gradslong)

gradlist <- group_split(gradslong)

#take data out of list format and just retain tibble
gradlist[1]


gradswide <- gradslong %>% 
    pivot_wider(names_from=academicYear, values_from = value)