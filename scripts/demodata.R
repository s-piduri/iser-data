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

#table of age projections
a <- "C:\\Users\\spiduri\\San Jose-Evergreen Community College District\\Accreditation ISER 2023 - env_scan_data\\Total Age Population Projections by County.xlsx"
age_proj <- read_excel(path=a, sheet = 3, skip =2)
ageproj <- age_proj %>% 
  filter(County == "Santa Clara County") %>% 
  select("Age Group", "2010", "2020", "2030", "2040", "2050", "2060")
  
ageproj1 <- ageproj %>% 
  separate('Age Group', into = c("lower", 'higher')) %>% 
  mutate(Age = case_when(lower == '0' | lower == '5' | lower == '10' |
                                 lower == '1' ~ "0-14",
                                 lower == '20' ~ '20-24',
                                 lower == '15' ~ '15-19',
                                 lower == '25' | lower == '30' | lower == '35' ~ "25-39",
                                 lower == '40' | lower == '45' | lower == '50' |
                                   lower == '55' | lower == '60' ~ "40-64",
                                 TRUE ~ as.character("65+"))) %>% 
  select(Age, "2010", "2020", "2030", "2040", "2050", "2060") %>%
  group_by(Age) %>% 
  summarize(across(where(is.numeric), sum))

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
education <- educ[c(8:15), c(1:3, 18:19)] %>%
  mutate(across(where(is.numeric), round, 2))
e <- education[ ,i] <- apply(education[ , i], 2, function(x) as.numeric(as.character(x)))
colnames(education) <- c("level", 'Bay Area: Estimate', 'Bay Area: Percent', 'Santa Clara County: Estimate', 'Santa Clara County: Percent')
education[1,1] <- "Total Population Age 25+"
education1<- education %>% 
  mutate_if(is.numeric, ~replace_na(., 1))
sumed <- education1 %>% 
  mutate(aa = case_when(grepl("Bachelor", level) | grepl("professional", level) ~ "Associate's degree or higher",
                        grepl("Total", level) ~ "Total Population Age 25+",
                        TRUE ~ as.character("Lower than Associate's degree"))) %>% 
  group_by(aa) %>% 
  summarize(bae = sum(`Bay Area: Estimate`),
            bap = sum(`Bay Area: Percent`),
            scce = sum(`Santa Clara County: Estimate`),
            sccp = sum(`Santa Clara County: Percent`)) %>% 
  filter(grepl("degree", aa)) %>% 
  ungroup() %>% 
  arrange(desc(aa))
colnames(education1) <- c(" ", 'Bay Area: Estimate', 'Bay Area: Percent', 'Santa Clara County: Estimate', 'Santa Clara County: Percent')
colnames(sumed) <- c(" ", 'Bay Area: Estimate', 'Bay Area: Percent', 'Santa Clara County: Estimate', 'Santa Clara County: Percent')
educ_total <- rbind(education1, sumed)

reducation <- educ[c(30:57), c(1:3, 18:19)]
  
#ggplot(projections, aes(x=years, y=population, fill=Geography)) + geom_bar(stat = "identity", position='dodge')


#read in census table to create race and ethnicity tables
census <- "C:\\Users\\spiduri\\San Jose-Evergreen Community College District\\Accreditation ISER 2023 - env_scan_data\\Census - Population Characteristics by County.xlsx"

cen_raw <- read_excel(path=census, sheet = 2)

cen_total <- subset(cen_raw, select = -c(4:17, 20:25))

colnames(cen_total) <- c(" ", 'Bay Area: Estimate', 'Bay Area: Percent', 'Santa Clara County: Estimate', 'Santa Clara County: Percent')

cen_total = cen_total[-1,]

x <- cen_total[ ,i] <- apply(cen_total[ , i], 2, function(x) as.numeric(as.character(x)))

race <- cen_total[67:72,] %>%
  mutate(across(where(is.numeric), round, 4)) 

ethnicity <- rbind(cen_total[75,], cen_total[81:87,])
colnames(ethnicity) <- c('Ethnicity', 'bae', 'bap', 'scce', 'sccp')

load("raceenrollment.RData")

percentage <- function(y) {
  paste0(y, "%")
} 


ethnicity1 <- ethnicity %>% 
  left_join(annual_hc_r, by = "Ethnicity") %>% 
  arrange(desc(bap)) %>% 
  select(!annual) %>% 
  select(!race) %>% 
  mutate_at(c(7), as.character) %>% 
  mutate_at(c(7), ~percentage(.))

colnames(ethnicity1) <- c("Ethnicity", 'Bay Area: Estimate', 'Bay Area %', 'Santa Clara County: Estimate', 'Santa Clara County %',
                         "SJCC Headcount", "SJCC Headcount %")


##socioeconomic data
income <- "C:\\Users\\spiduri\\San Jose-Evergreen Community College District\\Accreditation ISER 2023 - env_scan_data\\Census - Income by County.xlsx"
inc <- read_excel(path=income, sheet =2, skip=2)
median <- inc[c(13:14),c(1, 5:7)]
median[1,1] <- "Median Income (dollars)"
median1 <- median[1] %>% cbind(round(median[2:4], digits =0))
colnames(median1) <- c("Income Level", 'Greater Bay Area', 'Santa Clara County', 'USA')

inc_per <- inc[c(3:12),c(1:4)]
colnames(inc_per) <- c("Income Level", 'Greater Bay Area', 'Santa Clara County', 'USA')

pov <- "C:\\Users\\spiduri\\San Jose-Evergreen Community College District\\Accreditation ISER 2023 - env_scan_data\\Census - Poverty Level.xlsx"
poverty <- read_excel(path=pov, skip=1, n_max=1)
poverty <- poverty[,c(4,28)]
colnames(poverty) <- c("Greater Bay Area", "Santa Clara County")
poverty1 <- read_excel(path=pov, skip = 1)
poverty_race <- poverty1[c(16:24),c(1, 4, 28)]
colnames(poverty_race) <- c("Race", "Greater Bay Area", "Santa Clara County")
p <- poverty_race[ ,3] <- apply(poverty_race[ , 3], 2, function(x) as.numeric(as.character(x)))
pdemo <- function(x, na.rm = FALSE){
  round(x*100, digits = 1)
}
percentage <- function(y) {
  paste0(y, "%")
} 

poverty_race1 <- poverty_race %>% 
  mutate_at(c(2:3), pdemo) %>% 
  arrange(desc(`Greater Bay Area`)) %>% 
  mutate_at(c(2:3), percentage)



save(projections, baylang, ageproj, raceproj, race, educ_total, ethnicity1, 
     median1, inc_per, poverty, poverty_race1, file="demo.RData")

