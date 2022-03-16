library(tidyverse)
library(odbc)
library(DBI)
library(janitor)

## connect to the server
ODS <- dbConnect(odbc(), Driver = "SQL Server", Server = "redrobin", 
                 Database = "ODS_production", Trusted_Connection = "True", 
                 UID = "SJECCD\\shoribe", Port = 1433)


# Read enrolled students -----------------------------------------------------


enrlmnt_raw <- dbGetQuery(ODS, paste(read_lines("sql/enrlmnt.sql"), collapse = "\n")) %>% clean_names()



# save data ---------------------------------------------------------------

save(enrlmnt_raw, file="enrlmnt_data.RData")

