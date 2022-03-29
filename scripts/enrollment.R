library(tidyverse); library(readxl)

#head count by special populations
sp <- "C:\\Users\\spiduri\\Downloads\\SPStudentCountSumm.csv"
special_pop <- read_csv(sp, skip=2)

sp_hc <- special_pop[,c(1:2, 7, 12, 17, 22)]
sp_hc <- sp_hc[c(-1),]
colnames(sp_hc) <- c("Special Characteristic", "2017", "2018", "2019",
                     "2020", "2021")
sp_hc <- sp_hc %>% 
  replace(is.na(.), 0)

# length(unique(fa2018$student_id))

####overall headcounts#####
#headcount by year
annual_hc <- enrlmnt_raw %>% 
  mutate(student_yr = paste(student_id, term_reporting_year, sep="_")) %>% 
  mutate(annual = paste(term_reporting_year, (term_reporting_year+1), sep="-")) %>% 
  group_by(student_yr) %>% 
  arrange(student_yr, annual) %>% 
  mutate(acad_year_unique = row_number()) %>%
  filter(acad_year_unique == 1) %>%
  ungroup() %>% 
  group_by(annual) %>% 
  summarize(headcount = n()) 
colnames(annual_hc) <- c("Academic Year", "Headcount")

#credit headcount by year
annual_c_hc <- enrlmnt_raw %>% 
  mutate(student_yr = paste(student_id, term_reporting_year, sep="_")) %>% 
  mutate(annual = paste(term_reporting_year, (term_reporting_year+1), sep="-")) %>%
  filter(!(grepl("-5", crs_name,fixed=TRUE))) %>% 
  group_by(student_yr) %>% 
  arrange(student_yr, annual) %>% 
  mutate(acad_year_unique = row_number()) %>%
  filter(acad_year_unique == 1) %>%
  ungroup() %>% 
  group_by(annual) %>% 
  summarize(headcount = n()) 
colnames(annual_c_hc) <- c("Academic Year", "Headcount")

#noncredit headcount by year
annual_nc_hc <- enrlmnt_raw %>% 
  mutate(student_yr = paste(student_id, term_reporting_year, sep="_")) %>% 
  mutate(annual = paste(term_reporting_year, (term_reporting_year+1), sep="-")) %>%
  filter((grepl("-5", crs_name,fixed=TRUE))) %>% 
  group_by(student_yr) %>% 
  arrange(student_yr, annual) %>% 
  mutate(acad_year_unique = row_number()) %>%
  filter(acad_year_unique == 1) %>%
  ungroup() %>% 
  group_by(annual) %>% 
  summarize(headcount = n()) 
colnames(annual_nc_hc) <- c("Academic Year", "Headcount")

#####overall seatcounts#####
#overall seatcount by year
annual_sc <- enrlmnt_raw %>% 
  mutate(annual = paste(term_reporting_year, (term_reporting_year+1), sep="-")) %>%
  group_by(annual) %>% 
  summarize(seatcount = n())
colnames(annual_sc) <- c("Academic Year", "Seatcount")

#credit seatcount by year
annual_c_sc <- enrlmnt_raw %>% 
  mutate(annual = paste(term_reporting_year, (term_reporting_year+1), sep="-")) %>%
  filter(!(grepl("-5", crs_name,fixed=TRUE))) %>% 
  group_by(annual) %>% 
  summarize(seatcount = n())
colnames(annual_c_sc) <- c("Academic Year", "Seatcount")

#noncredit seatcount by year
annual_nc_sc <- enrlmnt_raw %>% 
  mutate(annual = paste(term_reporting_year, (term_reporting_year+1), sep="-")) %>%
  filter(grepl("-5", crs_name,fixed=TRUE)) %>% 
  group_by(annual) %>% 
  summarize(seatcount = n())
colnames(annual_nc_sc) <- c("Academic Year", "Seatcount")


#####headcount by term overall#####
#headcount by term
term_hc <- enrlmnt_raw %>% 
  mutate(term_cid = case_when(
    grepl("SPI", term_id,fixed=TRUE) ~ substr(term_id,1,nchar(term_id)-1),
    TRUE ~ as.character(term_id))) %>% 
  mutate(student_yr = paste(student_id, term_id, sep="_")) %>% 
  group_by(student_yr) %>% 
  arrange(student_yr, term_reporting_year) %>% 
  mutate(acad_year_unique = row_number()) %>%
  filter(acad_year_unique == 1) %>%
  ungroup() %>% 
  group_by(term_id, term_reporting_year) %>% 
  summarize(headcount = n())
colnames(term_hc) <- c("Term", "Headcount")

#specifically fall headcounts
fall_hc <- enrlmnt_raw %>% 
  filter(grepl("FA", term_id)) %>% 
  mutate(student_term = paste(student_id, term_id, sep="_")) %>% 
  group_by(student_term) %>% 
  arrange(student_term, term_reporting_year) %>% 
  mutate(fall_unique = row_number()) %>% 
  filter(fall_unique == 1) %>%
  ungroup() %>% 
  group_by(term_id) %>% 
  summarize(headcount = n())
colnames(fall_hc) <- c("Fall Term", "Headcount")

#####fall credit headcounts#####
#fall credit headcounts disaggregation
fall_cred_hc <- enrlmnt_raw %>% 
  filter(grepl("FA", term_id)) %>% 
  filter(!(grepl("-5", crs_name,fixed=TRUE))) %>% 
  #filter(!(cred_type_desc== "Noncredit Course")) %>% 
  mutate(student_term = paste(student_id, term_id, sep="_")) %>% 
  group_by(student_term) %>% 
  arrange(student_term, term_reporting_year) %>% 
  mutate(fall_unique = row_number()) %>% 
  filter(fall_unique == 1) %>%
  ungroup() %>% 
  group_by(term_id) %>% 
  summarize(headcount = n())
colnames(fall_cred_hc) <- c("Fall Term", "Headcount")

#credit headcounts by student type
fall_type_hc <- enrlmnt_raw %>% 
  filter(grepl("FA", term_id)) %>% 
  filter(!grepl("-5", crs_name,fixed=TRUE)) %>% 
  mutate(student_term = paste(student_id, term_id, sep="_")) %>% 
  group_by(student_term) %>% 
  arrange(student_term, term_reporting_year) %>% 
  mutate(fall_unique = row_number()) %>% 
  filter(fall_unique == 1) %>%
  ungroup() %>% 
  group_by(term_id, sb15_term_status) %>% 
  summarize(headcount = n())
colnames(fall_type_hc) <- c("Fall Term", "Student Type", "Headcount")

#credit headcounts by goal
fall_goal_hc <- enrlmnt_raw %>% 
  filter(grepl("FA", term_id)) %>% 
  filter(!grepl("-5", crs_name,fixed=TRUE)) %>% 
  mutate(student_term = paste(student_id, term_id, sep="_")) %>% 
  group_by(student_term) %>% 
  arrange(student_term, term_reporting_year) %>% 
  mutate(fall_unique = row_number()) %>% 
  filter(fall_unique == 1) %>%
  ungroup() %>% 
  group_by(term_id, educ_goal_desc) %>% 
  summarize(headcount = n())
colnames(fall_goal_hc) <- c("Fall Term", "Educational Goal", "Headcount")

#credit headcounts by age
fall_age_hc <- enrlmnt_raw %>% 
  filter(grepl("FA", term_id)) %>% 
  filter(!grepl("-5", crs_name,fixed=TRUE)) %>% 
  mutate(student_term = paste(student_id, term_id, sep="_")) %>% 
  group_by(student_term) %>% 
  arrange(student_term, term_reporting_year) %>% 
  mutate(fall_unique = row_number()) %>% 
  filter(fall_unique == 1) %>%
  ungroup() %>% 
  group_by(term_id, age) %>% 
  summarize(headcount = n())
colnames(fall_age_hc) <- c("Fall Term", "Age Group", "Headcount")

#credit headcounts by race
fall_race_hc <- enrlmnt_raw %>% 
  filter(grepl("FA", term_id)) %>% 
  filter(!grepl("-5", crs_name,fixed=TRUE)) %>% 
  mutate(student_term = paste(student_id, term_id, sep="_")) %>% 
  group_by(student_term) %>% 
  arrange(student_term, term_reporting_year) %>% 
  mutate(fall_unique = row_number()) %>% 
  filter(fall_unique == 1) %>%
  ungroup() %>% 
  group_by(term_id, race) %>% 
  summarize(headcount = n())
colnames(fall_race_hc) <- c("Fall Term", "Race", "Headcount")

#fall credit headcounts by gender
fall_gender_hc <- enrlmnt_raw %>% 
  filter(grepl("FA", term_id)) %>% 
  filter(!grepl("-5", crs_name,fixed=TRUE)) %>% 
  mutate(student_term = paste(student_id, term_id, sep="_")) %>% 
  group_by(student_term) %>% 
  arrange(student_term, term_reporting_year) %>% 
  mutate(fall_unique = row_number()) %>% 
  filter(fall_unique == 1) %>%
  ungroup() %>% 
  group_by(term_id, gender) %>% 
  summarize(headcount = n())
colnames(fall_gender_hc) <- c("Fall Term", "Gender", "Headcount")


##save####
save(sp_hc, annual_hc, annual_c_hc, annual_nc_hc,
     annual_sc, annual_c_sc, annual_nc_sc, 
     fall_age_hc, fall_cred_hc, fall_gender_hc, fall_goal_hc,
     fall_hc, fall_race_hc, fall_type_hc, 
     file="entable.RData")