race <- full %>% 
  filter(disagg == "Ethnicity", academicYear > filter_ay) %>% 
  select(academicYear, metricID, categoryID, subgroup, value, perc)

csrace <- race %>% 
  filter(metricID == "SM 408SW") %>% 
  select(academicYear, subgroup, perc) %>% 
  pivot_wider(names_from=academicYear, values_from = perc) %>% 
  drop_na()

catids <- c(603, 631, 608, 614)

#table for degree and cert earners
rgrads <- race %>% 
  filter(categoryID == catids[1]) %>% 
  select(academicYear, categoryID, subgroup, value)%>% 
  pivot_wider(names_from=academicYear, values_from = value) 
  
