#creates a table of course success disaggregated by gender

gender <- full %>% 
  filter(disagg == "Gender", academicYear > filter_ay) %>% 
  select(academicYear, metricID, categoryID, subgroup, value, perc)

#course success by gender
csgender <- gender %>% 
  filter(metricID == "SM 408SW") %>% 
  select(academicYear, subgroup, perc) %>% 
  pivot_wider(names_from=academicYear, values_from = perc) %>% 
  drop_na()
