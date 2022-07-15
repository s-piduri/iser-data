library(tidyverse)
library(janitor)
library(flextable)


# read retention data -----------------------------------------------------
#define data path
#this one is a bit heavy
load(file = "../2022-05-20ssm-scraping/raw_ssm.RData")

#define colleges to include
colleges <- c("Cabrillo College", "Chabot College", "Las Positas College", "De Anza College",
              "Foothill College", "Gavilan College", "Ohlone College", "Laney College",
              "Evergreen Valley College", "San Jose City College", "Mission College", "West Valley College", "College of San Mateo", "Canada College", "Skyline College", "Statewide")

#filter to keep only overall group
ssm_retention <- raw_ssm %>%
  filter(metric_id %in% c("SM 424SW", "SM 424SZ", "SM 424SU"), category_id %in% c("406", "407"), disagg == "Overall") %>% 
  filter(locale_name %in% colleges) %>% 
  mutate(academic_year = str_c(academic_year-1, "-", str_sub(academic_year, 3)))

# for disagg data, disagg degree transfer students only
ssm_retention_disagg <- raw_ssm %>%
  filter(metric_id %in% c("SM 424SW", "SM 424SZ", "SM 424SU"), category_id %in% c("406"),
         disagg %in% c("Overall", "Ethnicity", "Gender"),
         journey_status == "DegreeTransferStudents",
         locale_name == "San Jose City College") %>%
  mutate(academic_year = str_c(academic_year-1, "-", str_sub(academic_year, 3))) %>% 
  filter(!academic_year %in% c("2014-15", "2015-16")) %>%
  bind_rows(group_by(., disagg, subgroup) %>%
              summarise(perc = sum(value, na.rm = TRUE) / sum(denom, na.rm = TRUE),
                        academic_year = "5yr_avg",
                        .groups = "drop")
            )

#remove raw data to save space
rm(raw_ssm)

#create 5 year average data and append
ssm_retention_sum <- ssm_retention %>%
  filter(!academic_year %in% c("2014-15", "2015-16")) %>%
  bind_rows(group_by(., locale_name, journey_status, category_id) %>%
              summarise(perc = sum(value, na.rm = TRUE) / sum(denom, na.rm = TRUE),
                        academic_year = "5yr_avg")
  )

#save data
saveRDS(object = ssm_retention_sum,
        file = "ssm_retention_sum.rds")
saveRDS(object = ssm_retention_disagg,
        file = "ssm_retention_disagg.rds")
