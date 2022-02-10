###
library(tidyverse); library(readxl)

pop <- "C:\\Users\\spiduri\\San Jose-Evergreen Community College District\\Accreditation ISER 2023 - env_scan_data\\Total Population Projections by County.xlsx"

pop_proj <- read_excel(path=pop, sheet = 2, skip =2)

#table of population projections of the greater bay area and santa clara county
projections <- pop_proj %>% 
  filter(Geography == pop_proj$Geography[2] | Geography == pop_proj$Geography[45]) %>% 
  select("Geography", "2020", "2030", "2040", "2050", "2060")

