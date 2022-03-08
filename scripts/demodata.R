###
if (!require("naniar")) {install.packages("naniar"); require("naniar")}
library(tidyverse); library(readxl); library(naniar)

###read projection tables into r
pop <- "C:\\Users\\spiduri\\San Jose-Evergreen Community College District\\Accreditation ISER 2023 - env_scan_data\\Total Population Projections by County.xlsx"
pop_proj <- read_excel(path=pop, sheet = 2, skip =2)
r <- "C:\\Users\\spiduri\\San Jose-Evergreen Community College District\\Accreditation ISER 2023 - env_scan_data\\Total Race Population Projections by County.xlsx"
race_proj <- read_excel(path=r, sheet = 2, skip =2)

#table of population projections of the greater bay area and santa clara county
projections <- pop_proj %>% 
  filter(Geography == pop_proj$Geography[2] | Geography == pop_proj$Geography[45]) %>% 
  select("Geography", "2020", "2030", "2040", "2050", "2060") %>% 
  pivot_longer(cols = c("2020", "2030", "2040", "2050", "2060"), names_to = "years", values_to = "population") %>% 
  arrange(desc(Geography))

#table of race projections
years <- c("2020","2060")
raceproj <- race_proj[418:424,] %>% 
  select("Race/Ethnicity Recode", "2020", "2060") %>% 
  mutate("Percent Change" = .data[[years[[2]]]]-.data[[years[[1]]]])

#bay area language table
#read into r
l <- "C:\\Users\\spiduri\\San Jose-Evergreen Community College District\\Accreditation ISER 2023 - env_scan_data\\Census - Language Spoken at Home by County.xlsx"
lang <- read_excel(path=l, sheet =2, skip= 3)

baylang <- lang[c(2,5:20), c(1,3,5,7)]
colnames(baylang) <- c("Age/Language spoken at home","Total", "Only or Very Well", "Less than Very Well")
baylang <- baylang %>% 
  replace_with_na(replace = list("Only or Very Well" = 0, "Less than Very Well" =0))

#education
ed <- "C:\\Users\\spiduri\\San Jose-Evergreen Community College District\\Accreditation ISER 2023 - env_scan_data\\Census - Ed Attainment by County.xlsx"
educ <- read_excel(path=ed, sheet=2)

i <- c(2:5)
education <- educ[c(8:17), c(1:3, 18:19)] %>%
  mutate(across(where(is.numeric), round, 2)) 
e <- education[ ,i] <- apply(education[ , i], 2, function(x) as.numeric(as.character(x)))
colnames(education) <- c(" ", 'Bay Area: Estimate', 'Bay Area: Percent', 'SC County: Estimate', 'SC County: Percent')

#ggplot(projections, aes(x=years, y=population, fill=Geography)) + geom_bar(stat = "identity", position='dodge')


#read in census table to create race and ethnicity tables
census <- "C:\\Users\\spiduri\\San Jose-Evergreen Community College District\\Accreditation ISER 2023 - env_scan_data\\Census - Population Characteristics by County.xlsx"

cen_raw <- read_excel(path=census, sheet = 2)

cen_total <- subset(cen_raw, select = -c(4:17, 20:25))

colnames(cen_total) <- c(" ", 'Bay Area: Estimate', 'Bay Area: Percent', 'SC County: Estimate', 'SC County: Percent')

cen_total = cen_total[-1,]

x <- cen_total[ ,i] <- apply(cen_total[ , i], 2, function(x) as.numeric(as.character(x)))

race <- cen_total[67:72,] %>%
  mutate(across(where(is.numeric), round, 4)) 

ethnicity <- rbind(cen_total[75,], cen_total[81:87,])

##socioeconomic data


save(projections, baylang, raceproj, race, education, ethnicity, file="demo.RData")

