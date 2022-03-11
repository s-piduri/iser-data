library(tidyverse); library(readxl)

path <- "C:\\Users\\spiduri\\San Jose-Evergreen Community College District\\SJCC RPIE - Documents\\ACCJC Annual Reports\\CTE Licence and Job Placement Data\\Licensing Exam Pass Rates For Ann Rpt2022.xlsx"
licensure <- read_excel(path=path, sheet = 1)
jobplace <- read_excel(path=path, sheet = 2)

#drop the rows that don't contain any data
licensure <- drop_na(licensure, `FY18/19`)
jobplace <- drop_na(jobplace, `FY18/19`)

yr1618 <- "C:\\Users\\spiduri\\San Jose-Evergreen Community College District\\SJCC RPIE - Documents\\ACCJC Annual Reports\\CTE Licence and Job Placement Data\\Licensing Exam Pass Rates For Ann Rpt2020.xlsx"
lic1618 <- read_excel(path=yr1618, sheet=1) %>% drop_na(`FY18/19`)
lic1618 <- lic1618[,c(1,4:5)]
licensure <- licensure %>% 
  add_column('FY16/17' = lic1618$`FY16/17`, .before = "FY18/19") %>% 
  add_column('FY17/18' = lic1618$`FY17/18`, .before = "FY18/19")

licensure[5,8] <- "N/A**"
licensure[5, 10] <- "**No one took the exam this year due to covid"

save(licensure, jobplace, file="jobs.RData")
