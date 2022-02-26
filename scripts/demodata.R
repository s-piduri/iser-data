###
library(tidyverse); library(readxl)

pop <- "C:\\Users\\spiduri\\San Jose-Evergreen Community College District\\Accreditation ISER 2023 - env_scan_data\\Total Population Projections by County.xlsx"

pop_proj <- read_excel(path=pop, sheet = 2, skip =2)

#table of population projections of the greater bay area and santa clara county
projections <- pop_proj %>% 
  filter(Geography == pop_proj$Geography[2] | Geography == pop_proj$Geography[45]) %>% 
  select("Geography", "2020", "2030", "2040", "2050", "2060") %>% 
  pivot_longer(cols = c("2020", "2030", "2040", "2050", "2060"), names_to = "years", values_to = "population") %>% 
  arrange(desc(Geography))


ggplot(projections, aes(x=years, y=population, fill=Geography)) + geom_bar(stat = "identity", position='dodge')

census <- "C:\\Users\\spiduri\\San Jose-Evergreen Community College District\\Accreditation ISER 2023 - env_scan_data\\Census - Population Characteristics by County.xlsx"

cen_raw <- read_excel(path=census, sheet = 2)

cen_total <- subset(cen_raw, select = -c(4:17, 20:25))

colnames(cen_total) <- c(1, 'Bay Area: Estimate', 'Bay Area: Percent', 'SC County: Estimate', 'SC County: Percent')

cen_total = cen_total[-1,]
i <- c(2:5)
x <- cen_total[ ,i] <- apply(cen_total[ , i], 2, function(x) as.numeric(as.character(x)))

ethnicity <- cen_total[66:71,] %>%
  mutate(across(where(is.numeric), round, 4)) 

