###
#try 

#################
filter_ay <- 2017
filter_csu <- 2016
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

###########course success table#########
#create row for actual course success metrics
cslong <- overall %>% 
  filter(metricID == "SM 408SW") %>% 
  select(academicYear, perc) 

#get the floor goals
floor <- rollmean(cslong$perc, 3, align = "right")*.9
floor <- c(rep(0,2), floor)
cslong <- cslong %>% 
  add_column(floor, .before="perc")

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
cslong <- cslong %>% add_column(aspirational, .before="perc")

##transpose table and add academic years as column names
cslong1 <- cslong %>% 
  column_to_rownames("academicYear")

cswide <- cslong1 %>% 
  t %>% 
  as.data.frame() %>% 
  rownames_to_column(" ")

#########grad/cert/degree tables###############

#vector of relevant category ids (603 = cert, 631 = degree, 608 = adt)
catids <- c(603, 631, 608, 614)

#data for degree and cert earners
gradslong <- overall %>% 
    filter(categoryID %in% catids) %>% 
    select(categoryID, academicYear, value) %>% 
    group_by(categoryID) %>% 
    arrange(categoryID)

group_keys(gradslong)

#splits grads dataset by categoryID
grads603 <- gradslong %>% 
  filter(categoryID == 603)
grads614 <- gradslong %>% 
  filter(categoryID == 614)
grads608 <- gradslong %>% 
  filter(categoryID == 608)
grads631 <- gradslong %>% 
  filter(categoryID == 631)

####### grads603 = certificate earners = floor and aspirational data#######

floor603 <- floor(rollmean(grads603$value, 3, align = "right")*.9)
floor603 <- c(rep(0,2), floor603)
grads603 <- grads603 %>% 
  add_column(floor603, .before="value")

#get the aspirational goals
#find the actual percentage corresponding to the first year being looked at
#take the whole row
cert_ay <- grads603 %>% 
  filter(academicYear == filter_ay)
#grab the base value
cert_base <- cert_ay$value
#set percent increase needed
pinc = 1.20
#calculate aspirational goal by multiplying by base
aspir = floor(pinc*cert_base)

#filter to just the years being looked at
grads603 <- grads603 %>% 
  filter(academicYear >= filter_ay)

#get the number of years being looked at in total (from set starting year to end of data)
yrlngth = length(grads603$academicYear)-1

#get the percent increase per year
cert_incr <- (aspir/cert_base)^(1/5)

#create vector with aspirational goals
aspir603 = round(cumprod(c(cert_base, rep(cert_incr,yrlngth))))

#add aspirational goal to table
grads603 <- grads603 %>% add_column(aspir603, .before="value")

##add academic years as column names
grads6031 <- grads603 %>% 
  column_to_rownames("academicYear")

#transpose table
grads603wide <- grads6031 %>% 
  t %>% 
  as.data.frame() %>% 
  rownames_to_column(" ")

#drop categoryID row
grads603wide <- grads603wide[!(row.names(grads603wide) %in% c('categoryID')), ]

####### grads631 = degree earners = floor and aspirational data#######

floor631 <- floor(rollmean(grads631$value, 3, align = "right")*.9)
floor631 <- c(rep(0,2), floor631)
grads631 <- grads631 %>% 
  add_column(floor631, .before="value")

#get the aspirational goals
#find the actual percentage corresponding to the first year being looked at
#take the whole row
deg_ay <- grads631 %>% 
  filter(academicYear == filter_ay)
#grab the base value
deg_base <- deg_ay$value
#set percent increase needed
pinc = 1.20
aspir = floor(pinc*deg_base)

#filter to just the years being looked at
grads631 <- grads631 %>% 
  filter(academicYear >= filter_ay)

#get the number of years being looked at in total (from set starting year to end of data)
yrlngth = length(grads631$academicYear)-1
deg_incr <- (aspir/deg_base)^(1/5)

#create vector with aspirational goals
aspir631 = round(cumprod(c(deg_base, rep(deg_incr,yrlngth))))

#add aspirational goal to table
grads631 <- grads631 %>% add_column(aspir631, .before="value")

##transpose table and add academic years as column names
grads6311 <- grads631 %>% 
  column_to_rownames("academicYear")

grads631wide <- grads6311 %>% 
  t %>% 
  as.data.frame() %>% 
  rownames_to_column(" ")

grads631wide <- grads631wide[!(row.names(grads631wide) %in% c('categoryID')), ]


####### grads608 = adt earners = floor and aspirational data#####

floor608 <- round(rollmean(grads608$value, 3, align = "right")*.9)
floor608 <- c(rep(0,2), floor608)
grads608 <- grads608 %>% 
  add_column(floor608, .before="value")

#get the aspirational goals
#find the actual percentage corresponding to the first year being looked at
#take the whole row
adt_ay <- grads608 %>% 
  filter(academicYear == filter_ay)
#grab the base value
adt_base <- adt_ay$value
#set percent increase needed
pinc = 1.35
aspir = floor(pinc*adt_base)

#filter to just the years being looked at
grads608 <- grads608 %>% 
  filter(academicYear >= filter_ay)

#get the number of years being looked at in total (from set starting year to end of data)
yrlngth = length(grads608$academicYear)-1
adt_incr <- (aspir/adt_base)^(1/5)

#create vector with aspirational goals
aspir608 = round(cumprod(c(adt_base, rep(adt_incr,yrlngth))))

#add aspirational goal to table
grads608 <- grads608 %>% add_column(aspir608, .before="value")

##transpose table and add academic years as column names
grads6081 <- grads608 %>% 
  column_to_rownames("academicYear")

grads608wide <- grads6081 %>% 
  t %>%   
  as.data.frame() %>% 
  rownames_to_column(" ")

grads608wide <- grads608wide[!(row.names(grads608wide) %in% c('categoryID')), ]


####### grads614 = distinct uc/csu transfer = floor and aspirational data#######

floor614 <- floor(rollmean(grads614$value, 3, align = "right")*.9)
floor614 <- c(0, grads614$value[2], floor614)
grads614 <- grads614 %>% 
  add_column(floor614, .before="value")

#get the aspirational goals
#find the actual percentage corresponding to the first year being looked at
#take the whole row
csu_ay <- grads614 %>% 
  filter(academicYear == filter_csu)
#grab the base value
csu_base <- csu_ay$value
#set percent increase needed
pinc = 1.35
aspir = floor(pinc*csu_base)

#filter to just the years being looked at
grads614 <- grads614 %>% 
  filter(academicYear >= filter_csu)

#get the number of years being looked at in total (from set starting year to end of data)
yrlngth = length(grads614$academicYear)-1
csu_incr <- (aspir/csu_base)^(1/5)

#create vector with aspirational goals
aspir614 = round(cumprod(c(csu_base, rep(csu_incr,yrlngth))))

#add aspirational goal to table
grads614 <- grads614 %>% add_column(aspir614, .before="value")

##transpose table and add academic years as column names
grads6141 <- grads614 %>% 
  column_to_rownames("academicYear")

grads614wide <- grads6141 %>% 
  t %>%   
  as.data.frame() %>% 
  rownames_to_column(" ")

grads614wide <- grads614wide[!(row.names(grads614wide) %in% c('categoryID')), ]



save(cswide, grads603wide, grads608wide, grads614wide, grads631wide, file = "finaltbl.RData")
