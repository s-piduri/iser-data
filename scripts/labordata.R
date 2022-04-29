library(tidyverse); library(readxl);

labor <- "C:\\Users\\spiduri\\San Jose-Evergreen Community College District\\Accreditation ISER 2023 - env_scan_data\\Industry Snapshot Aggregate.xlsx"
occupations <- "C:\\Users\\spiduri\\San Jose-Evergreen Community College District\\Accreditation ISER 2023 - env_scan_data\\Occupation Snapshot.xlsx"
occ_gaps <- "C:\\Users\\spiduri\\San Jose-Evergreen Community College District\\Accreditation ISER 2023 - env_scan_data\\Occupation Gaps 2-yr degree and up.xlsx"

topten <- read_excel(path=labor, sheet = 3, skip =2, n_max=10)
broad_ten <- read_excel(path=labor, sheet=1, skip=2, n_max=10)
toptech <- read_excel(path=labor, sheet = 5, skip =2, n_max=10)

ogaps <- read_excel(path=occ_gaps, skip=1, n_max=20)
gaps <- ogaps[c(1:10,16,17,18,19,20),]

some_college <- read_excel(path=occupations, sheet = 1, skip = 2, n_max=6)
nondegree <- read_excel(path=occupations, sheet = 2, skip = 2, n_max=46)
as <- read_excel(path=occupations, sheet = 3, skip = 2, n_max=47)
bs <- read_excel(path=occupations, sheet = 4, skip = 2, n_max=175)

occ <- rbind(some_college, nondegree, as, bs)

most_growth <- occ[order(-occ$`% Change`),] %>% 
  select("Occupation",  '% Change', 'Median Ann Wages2', 
         'Unempl Rate',)
most_growth <- most_growth[c(1:10),]
colnames(most_growth) <- c("Occupation", "% Change", "Median Ann. Wage", "Unemployment")

most_openings <- occ[order(-occ$`Annual Openings`),] %>% 
  filter(occ$`Annual Openings` > 3000) %>% 
  select('Occupation', 'Annual Openings', 'Median Ann Wages2', 'Unempl Rate')
most_openings$`Annual Openings` <- round(most_openings$'Annual Openings', 0)
most_openings <- most_openings[c(1:10),]
colnames(most_openings) <- c("Occupation", "Annual Job Openings", "Median Ann. Wage", "Unemployment")

topten <- topten %>% 
  select("Industry", "Empl %")

broad_ten <- broad_ten %>% 
  select("Industry", "% Empl") %>% 
  add_row("Industry" = "Other Industries", "% Empl" = 0.216071755)

toptech <- toptech %>% 
  select("Industry", "% Empl", "% Growth")

colnames(broad_ten) <- c("Industry", "Share of Total Employment")
colnames(topten) <- c("Industry", "Share of Total Employment")
colnames(toptech) <- c("Industry", "Share of Total Employment", "Projected Growth, 2020-2030")

colnames(gaps) <- c("Occupation (Average Salary)", "Gap")

save(topten, toptech, broad_ten, most_openings, 
     gaps, most_growth, file="labor_data.RData")
