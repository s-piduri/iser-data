##creates overall datasets with floor and aspirational data for the 5 standards
##uses two functions to do this - percentfloor to create a dataset of percentages
##                              - valuefloor to create a dataset of values
##these datasets are not saved in finaltbl.Rdata - those are the ones from actual.R
##the code in actual.R and floorasp.R should do the same thing

#############
beg_yr = 2017
end_yr = 2021
filter_disagg <- "Overall"
############

if (!require("zoo")) {install.packages("zoo"); require("zoo")}
library(data.table); library(tidyverse); library(zoo)
full <- fread("ssm_sjcc.csv", header = "auto")
full$categoryID <- replace_na(full$categoryID, "")

#create full datatable
full <- full %>% 
  mutate(years=paste(as.character(academicYear-1), as.character(academicYear), sep="-")) %>% 
  mutate(ids=paste(metricID, categoryID, sep=" ")) %>% 
  select(academicYear, years, ids, title, description, disagg, metricID, categoryID, value, perc)

overall <- full %>% 
  filter(disagg == filter_disagg) %>% 
  select(academicYear, years, ids, title, description, metricID, categoryID, value, perc)

#gets the floor and aspirational percentages for a certain category ID
percentfloor <- function(table, id, beg_year, end_year, pincrease){
  #get the floor goals
  yrtable <- table %>% 
    filter(grepl(id, ids, fixed=TRUE))
  floor <- rollmean(yrtable$perc, 3, align = "right")*.9
  yrtable <- yrtable %>% filter(academicYear >= beg_year & academicYear <= end_year)
  yrtable <- yrtable %>% add_column(floor, .before="perc")

  #get the aspirational goals
  #find the actual percentage corresponding to the first year being looked at
  #take the whole row
  cs_ay <- yrtable %>%
    filter(academicYear == beg_year)
  #grab the base value
  cs_base <- round(cs_ay$perc, digits = 2)
  #set percent increase needed
  pinc = pincrease

  #get the number of years being looked at in total (from set starting year to end of data)
  csceil <- cs_base+(pincrease*(length(yrtable$academicYear)-1))

  #create vector with aspirational goals
  aspirational = seq(cs_base, csceil, by=pincrease)

  #add aspirational goal to table
  yrtable %>% add_column(aspirational, .before="perc") %>%
    select(years, ids, title, description, floor, aspirational, perc)
}

#gets the floor and aspirational values for a certain category ID
#DOES not work with course success (percentages)
valuefloor <- function(table, id, beg_year, end_year, pincrease){
  yrtable <- table %>% 
  filter(grepl(id, ids, fixed=TRUE))
  floor <- floor(rollmean(yrtable$value, 3, align = "right")*.9)
  yrtable <- yrtable %>% filter(academicYear >= beg_year & academicYear <= end_year)
  yrtable <- yrtable %>% add_column(floor, .before="value")
  
  #get the aspirational goals
  #find the actual percentage corresponding to the first year being looked at
  #take the whole row
  cert_ay <- yrtable %>% 
    filter(academicYear == beg_year)
  #grab the base value
  cert_base <- cert_ay$value
  #set percent increase needed
  pinc = pincrease
  #calculate aspirational goal by multiplying by base
  aspir = floor(pinc*cert_base)
  
  #get the number of years being looked at in total (from set starting year to end of data)
  yrlngth = length(yrtable$academicYear)-1
  
  #get the percent increase per year across 5 years
  cert_incr <- (aspir/cert_base)^(1/5)
  
  #create vector with aspirational goals
  aspirational = round(cumprod(c(cert_base, rep(cert_incr,yrlngth))))
  
  #add aspirational goal to table
  yrtable %>% add_column(aspirational, .before="value") %>% 
    select(years, ids, title, description, floor, aspirational, value) %>% 
    pivot_longer(cols=c("floor", "aspirational", "value"), names_to = "type", values_to = "value")
  
}

lagfloor <- function(table, id, beg_year, end_year, pincrease){
  yrtable <- table %>% 
    filter(grepl(id, ids, fixed=TRUE))
  floor <- floor(rollmean(yrtable$value, 3, align = "right")*.9)
  #add if statement so that 614/other lagged categories can have correct floors
  if(length(floor) != (length(yrtable[[1]])-1)){
    diff <- length((yrtable[[1]])) - length(floor)
    floor <- c((floor(yrtable$value[diff]*.9)), floor)
  }
  yrtable <- yrtable %>% filter(academicYear >= beg_year & academicYear <= end_year)
  yrtable <- yrtable %>% add_column(floor, .before="value")
  
  #get the aspirational goals
  #find the actual percentage corresponding to the first year being looked at
  #take the whole row
  cert_ay <- yrtable %>% 
    filter(academicYear == beg_year)
  #grab the base value
  cert_base <- cert_ay$value
  #set percent increase needed
  pinc = pincrease
  #calculate aspirational goal by multiplying by base
  aspir = floor(pinc*cert_base)
  
  #get the number of years being looked at in total (from set starting year to end of data)
  yrlngth = length(yrtable$academicYear)-1
  
  #get the percent increase per year across 5 years
  cert_incr <- (aspir/cert_base)^(1/5)
  
  #create vector with aspirational goals
  aspirational = round(cumprod(c(cert_base, rep(cert_incr,yrlngth))))
  
  #add aspirational goal to table
  yrtable %>% add_column(aspirational, .before="value") %>% 
    select(years, ids, title, description, floor, aspirational, value) %>% 
    pivot_longer(cols=c("floor", "aspirational", "value"), names_to = "type", values_to = "value")
  
}


#gets course success rates
#aspirational percent increase of .5% per year
overallcs <- percentfloor(overall, 408, beg_yr, end_yr, .005)
#percentfloor returns a data table that is not fully long so pivot longer
overallcs <- overallcs %>%  
  pivot_longer(cols=c("floor", "aspirational", "perc"), names_to = "type", values_to = "percentage")


#gets number of certificate earners
#aspirational percent increase of 20% from 16/17 to 21/22
overall603 <- valuefloor(overall, 603, beg_yr, end_yr, 1.2)

#gets number of degree earners (as, adt, aa)
#aspirational percent increase of 20% from 16/17 to 21/22
overall631 <- valuefloor(overall, 631, beg_yr, end_yr, 1.2)

#gets number of adt earners
#aspirational percent increase of 35% from 16/17 to 21/22
overall608 <- valuefloor(overall, 608, beg_yr, end_yr, 1.35)

#gets number of csu/uc earners
#aspirational percent increase of 35% from 15/16 to 20/21
#lagged by one year, so beg_yr-1 is the starting year
overall614 <- lagfloor(overall, 614, beg_yr-1, end_yr, 1.35)

save(overallcs, overall603, overall631, overall608, overall614, 
     csrace, rgrads603, rgrads608, rgrads614, rgrads631,
     csgender, ggrads603, ggrads608, ggrads614, ggrads631,
     file = "is_standards.RData")
