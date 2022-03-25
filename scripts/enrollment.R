

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


total_hc <- enrlmnt_raw %>% 
  mutate(student_term = paste(student_id, term_id, sep="_")) %>% 
  group_by(student_term) %>% 
  arrange(student_term, term_reporting_year) %>% 
  mutate(row_n = row_number())


save(sp_hc, file="entable.RData")

