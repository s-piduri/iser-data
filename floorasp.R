beg_yr = 2017
end_yr = 2020
length(a[[1]])

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
  cs_base <- cs_ay$perc
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
  ##add if statement so that 614/other lagged categories can have correct floors
  if(length(floor) != length(table[[1]])){
    
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
    select(years, ids, title, description, floor, aspirational, value)
}

#gets course success rates
overallcs <- percentfloor(overall, 408, 2017, 2020, .005)


overall603 <- valuefloor(overall, 603, 2017, 2020, 1.2)
overall631 <- valuefloor(overall, 631, 2017, 2020, 1.2)
overall608 <- valuefloor(overall, 608, 2017, 2020, 1.35)
overall614 <- valuefloor(overall, 614, beg_yr-1, end_yr-1, 1.35)


