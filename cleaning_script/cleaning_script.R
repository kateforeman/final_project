library(tidyverse) 

cancer_incidences <- read_csv("raw_data/raw_data_cancer_incidences.csv") 

glimpse(cancer_incidences_clean) 

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
  
sum(is.na(cancer_incidences_clean)) 

sum(is.na(cancer_incidences)) 

cancer_mortality <- read_csv("raw_data/raw_data_cancer_mortality.csv") 

glimpse(cancer_mortality) 

#Following the same cleaning process as above. 
cancer_mortality_clean <- cancer_mortality %>% 
  select(-c(id, hbnew, sitenew, sexnew, label)) %>% 
  pivot_longer(cols = 5:30, names_to = "year", 
               values_to = "incidence_no_and_rates") %>% 
  mutate(year = str_extract(string = year, pattern = "[19|20][0-9][0-9]{2}"))  

cancer_prevalence <- read_csv("raw_data/raw_data_cancer_prevalence.csv") 

glimpse(cancer_prevalence) 

#Giving the variables more informative names. 
cancer_prevalence_clean_2017 <- cancer_prevalence %>% 
  select(-c(id, sitenew, sexnew, section)) %>% 
  rename(time_survived_and_age = sectlab) %>% 
  rename(number_of_cases = stat.1) %>% 
  rename(rate_per_100000 = stat.2) %>% 
  rename(rate_population = stat.3) %>% 
  rename(percentage_in_time_or_age = stat.4) 

cancer_deprivation <- read_csv("raw_data/raw_data_cancer_deprivation.csv") %>% 
  janitor::clean_names() 

#Hard recoding the SIMD column and transforming it to a factor as it has levels 
cancer_deprivation_clean <- cancer_deprivation %>% 
  mutate(simd_2016_deprivation_quintile = recode(simd_2016_deprivation_quintile, 
                                                 "1=Most deprived" = "1", 
                                                 "5=Least deprived" = "5")) %>% 
  mutate(simd_2016_deprivation_quintile = as.factor(simd_2016_deprivation_quintile)) 

post_surgery_experiences_2018 <- readxl::read_xlsx("raw_data/raw_data_post_surgery_experience_2018.xlsx") 

tumour_reports_in_2018 <- read_csv("raw_data/raw_data_tumour_grades.csv") 

england_reports_new_cases <- read_csv("raw_data/newly_diagnosed_england.csv") 