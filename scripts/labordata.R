library(tidyverse); library(readxl);

#paths for loading
labor <- "C:\\Users\\spiduri\\San Jose-Evergreen Community College District\\Accreditation ISER 2023 - env_scan_data\\Industry Snapshot Aggregate.xlsx"
occupations <- "C:\\Users\\spiduri\\San Jose-Evergreen Community College District\\Accreditation ISER 2023 - env_scan_data\\Occupation Snapshot.xlsx"
occ_gaps <- "C:\\Users\\spiduri\\San Jose-Evergreen Community College District\\Accreditation ISER 2023 - env_scan_data\\Occupation Gaps 2-yr degree and up.xlsx"
ind_div_race <- "C:\\Users\\spiduri\\San Jose-Evergreen Community College District\\Accreditation ISER 2023 - env_scan_data\\Industry Characteristics - Race Ethnicity.xlsx"
ind_div_gen <- "C:\\Users\\spiduri\\San Jose-Evergreen Community College District\\Accreditation ISER 2023 - env_scan_data\\Industry Characteristics - Gender.xlsx"

#load top ten industries, top tech
topten <- read_excel(path=labor, sheet = 3, skip =2, n_max=10)
broad_ten <- read_excel(path=labor, sheet=1, skip=2, n_max=10)
toptech <- read_excel(path=labor, sheet = 5, skip =2, n_max=10)

#load industries diversity
##by race
ind_r_prof <- read_excel(path=ind_div_race, sheet = 1, skip = 132)
ind_r_data <- read_excel(path=ind_div_race, sheet = 2, skip= 2)
ind_r_comp <- read_excel(path=ind_div_race, sheet = 3, skip = 2)
##by gender
ind_g_prof <- read_excel(path=ind_div_gen, sheet = 1, skip = 2)
ind_g_data <- read_excel(path=ind_div_gen, sheet = 2, skip = 2, n_max = 3)
ind_g_comp <- read_excel(path=ind_div_gen, sheet = 3, skip = 2, n_max =5)

#format ind diversity by race
colnames(ind_r_prof)[1] <- "Industry"
colnames(ind_r_prof)[5] <- "% Latino"
ind_rp <- ind_r_prof[1:5] %>% 
  pivot_longer(cols = 2:5, names_to = "disagg", values_to = "perc")

ind_rd <- ind_r_data[c(1:3, 5, 7, 9, 11), c(2:4, 11)] %>% 
  mutate(disagg = case_when(Ethnicity == "Hispanic or Latino" ~ "% Latino", 
                            (Race == "White Alone" & Ethnicity != "Hispanic or Latino") ~ "% White",
                            Race == "Asian Alone" ~ "% Asian",
                            Race == "Black or African American Alone" ~ "% Black")) %>% 
  select(Industry, disagg, perc) %>% 
  drop_na(disagg)

ind_rc <- ind_r_comp[c(2:4, 8, 15:17, 21), c(2:4, 11)] %>% 
  mutate(disagg = case_when(Ethnicity == "Hispanic or Latino" ~ "% Latino", 
                            (Race == "White Alone" & Ethnicity != "Hispanic or Latino") ~ "% White",
                            Race == "Asian Alone" ~ "% Asian",
                            Race == "Black or African American Alone" ~ "% Black")) %>% 
  select(Industry, disagg, perc) %>% 
  drop_na(disagg)

#format ind diversity by gender
ind_gp <- ind_g_prof[c(3, 5, 6), c(13,16,18)] %>% 
  pivot_longer(cols = 2:3, names_to = 'disagg', values_to = 'perc')
colnames(ind_gp)[1] <- "Industry"

ind_gc <- ind_g_comp[c(4:5), c(2, 10:11)] %>% 
  pivot_longer(cols = 2:3, names_to = 'disagg', values_to = 'perc')

ind_gd <- ind_g_data[c(1:2), c(2:3, 10)] %>% 
  mutate(disagg = if_else(Gender == "Male", "% Male", "% Female")) %>% 
  select(Industry, disagg, "% gender")
colnames(ind_gd)[3] <- "perc"

#bind diversity tables
ind_div <- rbind(ind_rc, ind_rd, ind_rp, ind_gp, ind_gc, ind_gd)
colnames(ind_div) <- c("Industry", "Demographic", "Percentage")
#separate tables by industry
id_arch <- ind_div %>% 
  filter(grepl("Arch", Industry))
id_comp <- ind_div %>% 
  filter(grepl("Compu", Industry))
id_data <- ind_div %>% 
  filter(grepl("Data", Industry))
id_semi <- ind_div %>% 
  filter(grepl("Semi", Industry))
id_man <- ind_div %>% 
  filter(grepl("Management", Industry))
id_nav <- ind_div %>% 
  filter(grepl("Nav", Industry))


#occupational gaps
ogaps <- read_excel(path=occ_gaps, skip=1, n_max=20)
gaps <- ogaps[c(1:10,16,17,18,19,20),]

#occupations table
some_college <- read_excel(path=occupations, sheet = 1, skip = 2, n_max=6)
nondegree <- read_excel(path=occupations, sheet = 2, skip = 2, n_max=46)
as <- read_excel(path=occupations, sheet = 3, skip = 2, n_max=47)
bs <- read_excel(path=occupations, sheet = 4, skip = 2, n_max=175)
#total occupations table
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
     gaps, most_growth, ind_div, id_arch, id_comp, id_data, id_man,
     id_nav, id_semi, file="labor_data.RData")
A