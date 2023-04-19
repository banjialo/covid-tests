#############################################################
#   Purpose: Convert Worldcare lab extract from wide to long#
#                Developed by: Banji Alo                    #
#                  Date: April 25, 2021                     #      
#############################################################

#clear work environment
rm(list = ls())

#load required libaries
library (tidyverse)
library (readxl)
library (data.table)
library (janitor)


#Set working directory

#read csv file
lab_fread <- fread (file = "labs_csv.csv", stringsAsFactors = FALSE)

#convert data to a nicer csv format
lab_fread_b <-  as_tibble (lab_fread)

#clean column names, rename and remove unwanted columns
labs_fread_c <- lab_fread_b %>%  clean_names () %>%  select (-c(1:3))  %>%  rename (clientid = client_id )


#Convert dataset from wide to long 
labs_long <- bind_cols (
  labs_fread_c %>% 
    arrange (clientid) %>% 
    select ( clientid:dob, starts_with("test")) %>% 
    pivot_longer (cols = -c("clientid", "first_name", "last_name", "dob"),
                  names_to = "test_code",
                  values_to = "test_type") %>% 
    filter (test_type != ""),
  labs_fread_c %>% 
    arrange (clientid) %>% 
    select ( clientid, starts_with("spec")) %>% 
    pivot_longer (cols = -c("clientid"),
                  names_to = "spec_collected_date",
                  values_to = "test_date") %>% 
    filter (test_date != ""),
  labs_fread_c %>% 
    arrange (clientid) %>% 
    select ( clientid, starts_with("result_")) %>% 
    pivot_longer (cols = -c("clientid"),
                  names_to = "result_value",
                  values_to = "test_result") %>% 
    filter (test_result != ""))

#rename and remove unwanted columns
labs_long_b <- labs_long %>% 
  select (-c(clientid...7 , clientid...10)) %>% 
  rename (client_id = clientid...1)

#save output inthe working directory
write.csv (labs_long_b, "lab_extracts_long.csv", row.names = FALSE, na = "")

#test per person
# labs_long_b %>% 
#   group_by(client_id) %>% 
#   summarise (n_test = n()) %>% 
#   ungroup %>% 
#   arrange (desc(n_test))


#frequency of number of tests
# labs_long_b %>% 
#   group_by(client_id) %>% 
#   summarise (n_test = n()) %>% 
#   ungroup %>% 
#   group_by (n_test) %>% 
#   summarise (freq = n()) %>% 
#   ungroup() %>% 
#   tail(10)
  
