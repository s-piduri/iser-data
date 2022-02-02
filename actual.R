###
#try 

#################
filter_ay <- 2014
filter_disagg <- "Overall"
goal_col <- "2015"
#################


#import packages, read in full csv file
library(data.table); library(tidyverse)
full <- fread("ssm_sjcc.csv", header = "auto")


allyear <- unique(full$academicYear)
allyears <- vector()
for(i in 1:6) {
  allyears[i] = toString(allyear[i])
}

#alternative
overall <- full %>% 
  filter(disagg == filter_disagg, academicYear > filter_ay) %>% 
  select(academicYear, metricID, categoryID, value, perc)

#create row for actual course success metrics
cslong <- overall %>% 
  filter(metricID == "SM 408SW") %>% 
  select(academicYear, perc) 
cswide <- cslong %>% 
  pivot_wider(names_from=academicYear, values_from = perc)

#vector of relevant category ids (603 = cert, 631 = degree, 608 = adt)
catids <- c(603, 631, 608, 614)

#table for degree and cert earners
grads <- overall %>% 
    filter(categoryID %in% catids) %>% 
    select(academicYear, categoryID, value) %>% 
    pivot_wider(names_from=academicYear, values_from = value)

#add in rows in coursesuccess for aspirational and floor goals
cswide <- add_column(cswide, Goal = "Actual", .before = allyears[1])

seq_along(cswide)
cswide3 <- vector("double", ncol(cswide))

for(i in 4:length(cswide)){
  cswide2[[i]] <- mean(cswide[[i]], cswide[[i-1]], cswide[[i-2]])*.9
}

#creates a set of empty vectors to hold floor values
for(j in seq_along(catids)){
  assign(paste0("grads", catids[j], sep=""), vector("integer", ncol(grads)))
}

floors <- function(x, y) {
  for(i in 4:length(x)){
    y[[i]] <- mean(x[[i]], x[[i-1]], x[[i-2]])*.9
  }
}

floors(cswide, cswide)

rbind(cswide, as.list(cswide2)) 
