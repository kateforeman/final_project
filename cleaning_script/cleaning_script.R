library(tidyverse) 

cancer_incidences <- read_csv("raw_data/raw_data_cancer_incidences.csv") 

#Converting the data into a clean format - coercing the raw number of cases and 
#crude rate from 1993 to 1998 into doubles, dropping unnecessary columns, 
#pivoting the data into a long format and extracting the year from the 
#year column. 
#NOTE: 
#NA's were introduced during the coercion process because there were "x"'s in 
#the data indicating a missing value and blank fields that were not classed 
#as NA until they were coerced into a double. 
cancer_incidences_clean <- cancer_incidences %>% 
  mutate(trans1.1993 = as.double(trans1.1993)) %>% 
  mutate(trans1.1994 = as.double(trans1.1994)) %>% 
  mutate(trans1.1995 = as.double(trans1.1995)) %>% 
  mutate(trans1.1996 = as.double(trans1.1996)) %>% 
  mutate(trans1.1997 = as.double(trans1.1997)) %>% 
  mutate(trans1.1998 = as.double(trans1.1998)) %>% 
  mutate(trans1.1999 = as.double(trans1.1999)) %>% 
  select(-c(id, hbnew, sitenew, sexnew, label)) %>% 
  pivot_longer(cols = 5:29, names_to = "year", 
               values_to = "incidence_no_and_rates") %>% 
  mutate(year = str_extract(string = year, pattern = "[19|20][0-9][0-9]{2}"))  

write_csv(cancer_incidences_clean, "cancer_incidences_in_scotland.csv")

cancer_mortality <- read_csv("raw_data/raw_data_cancer_mortality.csv") 

#Following the same cleaning process as above. 
cancer_mortality_clean <- cancer_mortality %>% 
  select(-c(id, hbnew, sitenew, sexnew, label)) %>% 
  pivot_longer(cols = 5:30, names_to = "year", 
               values_to = "incidence_no_and_rates") %>% 
  mutate(year = str_extract(string = year, pattern = "[19|20][0-9][0-9]{2}")) 

write_csv(cancer_mortality_clean, "cancer_mortality_in_scotland.csv")

cancer_prevalence <- read_csv("raw_data/raw_data_cancer_prevalence.csv") 

#Giving the variables more informative names. 
cancer_prevalence_clean_2017 <- cancer_prevalence %>% 
  select(-c(id, sitenew, sexnew, section)) %>% 
  rename(time_survived_and_age = sectlab) %>% 
  rename(number_of_cases = stat.1) %>% 
  rename(rate_per_100000 = stat.2) %>% 
  rename(rate_population = stat.3) %>% 
  rename(percentage_in_time_or_age = stat.4) 

write_csv(cancer_prevalence_clean_2017, "scotland_cancer_prevalence_2017.csv") 

cancer_deprivation <- read_csv("raw_data/raw_data_cancer_deprivation.csv") %>% 
  janitor::clean_names() 

#Hard recoding the SIMD column and transforming it to a factor as it has levels 
cancer_deprivation_clean <- cancer_deprivation %>% 
  mutate(simd_2016_deprivation_quintile = recode(simd_2016_deprivation_quintile, 
                                                 "1=Most deprived" = "1", 
                                                 "5=Least deprived" = "5")) %>% 
  mutate(simd_2016_deprivation_quintile = as.factor(simd_2016_deprivation_quintile)) 

write_csv(cancer_deprivation_clean, "cancer_by_simd_scotland.csv") 

post_surgery_experiences_2018 <- readxl::read_xlsx("raw_data/raw_data_post_surgery_experience_2018.xlsx") 

tumour_reports_in_2018 <- read_csv("raw_data/raw_data_tumour_grades.csv") 

england_reports_new_cases <- read_csv("raw_data/newly_diagnosed_england.csv") 

updated_cancer_incidences <- read_csv("raw_data/updated_cancer_incidences_2018.csv") 

updated_cancer_incidences_clean <- updated_cancer_incidences %>% 
  select(-c(id, id_1, id_3, id_5, id_7)) %>% 
  rename(hb_label = id_2) %>% 
  rename(site_label = id_4) %>% 
  rename(sex_label = id_6) %>% 
  rename(age_label = id_8) %>% 
  rename(trans1.1994 = id_9) %>% 
  mutate(trans1.1994 = as.double(trans1.1994)) %>% 
  mutate(trans1.1995 = as.double(trans1.1995)) %>% 
  mutate(trans1.1996 = as.double(trans1.1996)) %>% 
  mutate(trans1.1997 = as.double(trans1.1997)) %>% 
  mutate(trans1.1998 = as.double(trans1.1998)) %>% 
  mutate(trans1.1999 = as.double(trans1.1999)) %>% 
  pivot_longer(cols = 5:29, names_to = "year", 
               values_to = "incidence_no_and_rates") %>% 
  mutate(year = str_extract(string = year, pattern = "[19|20][0-9][0-9]{2}")) 

write_csv(updated_cancer_incidences_clean, "more_recent_cancer_incidences.csv")

survival_data <- read_csv("raw_data/raw_data_survival_recent.csv") %>% 
  janitor::clean_names() 

survival_data 

cancer_survival_clean <- survival_data %>% 
  select(-c(patients_remaining_at_risk_at_timepoint_t, 
            lower_95_percent_ci_for_observed_survival, 
            upper_95_percent_ci_for_observed_survival, 
            lower_95_percent_ci_for_net_survival, 
            upper_95_percent_ci_for_net_survival)) %>%  
  mutate(cancer_site_grouping = 
           recode(cancer_site_grouping, 
                  "Brain and other CNS (ICD-9 191-192; ICD-10 C70-C72, C75.1-C75.3)" = 
                    "Brain and other CNS Cancer")) %>% 
  mutate(observed_survival_percent = as.double(observed_survival_percent)) 

write_csv(cancer_survival_clean, "raw_data_cancer_survival.csv") 

