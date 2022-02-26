library(tidyverse); library(readxl)

path <- "C:\\Users\\spiduri\\San Jose-Evergreen Community College District\\SJCC RPIE - Documents\\ACCJC Annual Reports\\CTE Licence and Job Placement Data\\Licensing Exam Pass Rates For Ann Rpt2022.xlsx"
licensure <- read_excel(path=path, sheet = 1)
jobplace <- read_excel(path=path, sheet = 2)

#drop the rows that don't contain any data
licensure <- drop_na(licensure, `FY18/19`)
jobplace <- drop_na(jobplace, `FY18/19`)


save(licensure, jobplace, file="jobs.RData")
