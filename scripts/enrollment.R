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

enr <- "C:\\Users\\spiduri\\Downloads\\2022-03-08enrollment_data_ISER_[1] (2).csv"
enroll <- read_csv(enr)

#join enroll with goals so that goals data (educ_goal_id, educ_goal_desc) is 
#appended onto enroll
enroll_cred <- enroll %>% 
  mutate(max_cred = pmax(attempted_credit, section_min_creds, na.rm=TRUE)) %>% 
  group_by(student_id, term_id) %>% 
  mutate(total_att_units = sum(max_cred, na.rm=TRUE)) %>% 
  mutate(nc_flag = case_when(grepl("-5", crs_name,fixed=TRUE) ~ "Y", 
                            TRUE ~ as.character("N"))) %>% 
  group_by(term_id, student_id) %>% 
  mutate(cr_ncr = if_else(all(nc_flag == "Y"), "Y", "N")) %>% 
  ungroup() %>% 
  mutate(load = case_when(total_att_units >= 12 ~ "Full-time",
                   cr_ncr == "Y" ~ "Noncredit",
                   TRUE ~ as.character("Part-time"))) %>% 
  mutate_at(vars(gender, sb15_term_status), ~replace_na(., "Unknown"))
  
####overall headcounts#####
#headcount by year
annual_hc <- enroll %>%
  filter(location == "San Jose City College") %>% 
  mutate(student_yr = paste(student_id, term_reporting_year, sep="_")) %>% 
  mutate(annual = paste(term_reporting_year, (term_reporting_year+1), sep="-")) %>% 
  group_by(student_yr) %>% 
  arrange(student_yr, annual) %>% 
  mutate(acad_year_unique = row_number()) %>%
  filter(acad_year_unique == 1) %>%
  ungroup() %>% 
  group_by(annual) %>% 
  summarize(headcount = n()) 


#overall seatcount by year
annual_sc <- enroll %>% 
  filter(location == "San Jose City College") %>% 
  mutate(annual = paste(term_reporting_year, (term_reporting_year+1), sep="-")) %>%
  group_by(annual) %>% 
  summarize(seatcount = n())

annual_ftes <- enroll %>% 
  filter(location == "San Jose City College") %>% 
  mutate(annual = paste(term_reporting_year, (term_reporting_year+1), sep="-")) %>%
  group_by(annual) %>% 
  summarize(ftes = trunc(sum(total_FTES)))

annual <- annual_hc %>% 
  left_join(annual_sc, by="annual") %>% 
  left_join(annual_ftes, by="annual")
colnames(annual) <- c("Academic Year", "Headcount", "Seatcount", "Full-Time Equivalent")
annual <- annual %>% pivot_longer(cols=c("Headcount", "Seatcount", 
                               "Full-Time Equivalent"), names_to="Metric", 
                        values_to="value") %>% 
  pivot_wider(names_from = "Academic Year")


#####headcount by term overall#####
#headcount by term
term_hc <- enroll %>% 
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
fall_hc <- enroll_cred %>% 
  filter(grepl("FA", term_id)) %>% 
  filter(location == "San Jose City College") %>% 
  mutate(student_term = paste(student_id, term_id, sep="_")) %>% 
  group_by(student_term) %>% 
  arrange(student_term, term_reporting_year) %>% 
  mutate(fall_unique = row_number()) %>% 
  filter(fall_unique == 1) %>%
  ungroup() %>% 
  group_by(term_id) %>% 
  summarize(headcount = n())
colnames(fall_hc) <- c("Fall Term", "Headcount")

#credit headcounts by student type
fall_type_hc <- enroll_cred %>% 
  filter(grepl("FA", term_id)) %>% 
  filter(location == "San Jose City College") %>% 
  mutate(student_term = paste(student_id, term_id, sep="_")) %>% 
  mutate(term_status = case_when(
    sb15_term_status=="CONT" ~ "Continuing",
    sb15_term_status=="K12" ~ "K-12 (Special Admit)",
    sb15_term_status=="NEW" ~ "New",
    sb15_term_status=="RET" ~ "Returning",
    sb15_term_status=="TRAN" ~ "First-time Transfer",
    TRUE ~ as.character(sb15_term_status))) %>% 
  group_by(student_term) %>% 
  arrange(student_term, term_reporting_year) %>% 
  mutate(fall_unique = row_number()) %>% 
  filter(fall_unique == 1) %>%
  ungroup() %>% 
  group_by(term_id, term_status) %>% 
  summarize(headcount = n()) %>% 
  ungroup() %>% 
  add_row(term_id = '2021FA', term_status = 'Unknown', headcount=0)
colnames(fall_type_hc) <- c("Fall Term", "Student Type", "Headcount")

#credit headcounts by goal
fall_goal_hc <- enroll_cred %>% 
  filter(grepl("FA", term_id)) %>% 
  filter(location == "San Jose City College") %>% 
  mutate(student_term = paste(student_id, term_id, sep="_")) %>% 
  group_by(student_term) %>% 
  arrange(student_term, term_reporting_year) %>% 
  mutate(fall_unique = row_number()) %>% 
  filter(fall_unique == 1) %>%
  ungroup() %>% 
  group_by(term_id, educ_goal_id, educ_goal_desc) %>% 
  summarize(headcount = n()) %>% 
  ungroup()
colnames(fall_goal_hc) <- c("Fall Term", "id", "Educational Goal", "Headcount")
fall_goal_hc1 <- fall_goal_hc %>% 
  pivot_wider(names_from="Fall Term", values_from = "Headcount") %>% 
  select("Educational Goal", "2017FA", "2018FA", "2019FA", "2020FA", "2021FA")

#credit headcounts by age
fall_age_hc <- enroll_cred %>% 
  filter(grepl("FA", term_id)) %>%  
  filter(location == "San Jose City College") %>% 
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
fall_race_hc <- enroll_cred %>% 
  filter(grepl("FA", term_id)) %>% 
  filter(location == "San Jose City College") %>% 
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
fall_gender_hc <- enroll_cred %>% 
  filter(grepl("FA", term_id)) %>% 
  filter(location == "San Jose City College") %>% 
  mutate(student_term = paste(student_id, term_id, sep="_")) %>% 
  group_by(student_term) %>% 
  arrange(student_term, term_reporting_year) %>% 
  mutate(fall_unique = row_number()) %>% 
  filter(fall_unique == 1) %>%
  ungroup() %>% 
  group_by(term_id, gender) %>% 
  summarize(headcount = n()) %>% 
  replace(is.na(.), 'Unknown')
colnames(fall_gender_hc) <- c("Fall Term", "Gender", "Headcount")


##save####
save(sp_hc, annual, 
     fall_age_hc, fall_cred_hc, fall_gender_hc, fall_goal_hc, fall_goal_hc1,
     fall_hc, fall_race_hc, fall_type_hc, 
     file="entable.RData")
