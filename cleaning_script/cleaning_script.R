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
cancer_mortality_clean <- cancer_mortality %>% 
  select(-c(id, hbnew, sitenew, sexnew, label)) %>% 
  pivot_longer(cols = 5:30, names_to = "year", 
               values_to = "incidence_no_and_rates") %>% 
  mutate(year = str_extract(string = year, pattern = "[19|20][0-9][0-9]{2}"))  