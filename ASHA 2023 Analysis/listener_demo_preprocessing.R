# Participant Data Processing 

# Author: Micah E. Hirsch, mhirsch@fsu.edu

# Date: 10/26/2023

## Purpose: To load and clean listener demographic information 
## and export a cleaned version of the data. This preprocessing was completed
## for a poster presentation at the Annual American Speech Language Hearing 
## Association Convention held in Boston, MA in November 2023.

library(rio) # install.packages("rio")
library(tidyverse) # install.packages("tidyverse")

# Load in listener demographic data from RedCap

## Set working directory

setwd("~/Documents/Listening-Effort-in-Dysarthria/Raw Data")

demo <- rio::import("participant_demo_raw.csv")

## NOTE: LE03 withdrew from the study. Their data was removed from 
## RedCap prior to uploading the df here.

# Cleaning demographic data

demo1 <- demo %>%
  # Removing identifiable and unneeded variables from df
  dplyr::select(!c(date_participated, dob, sex, fluent_eng, demographic_information_complete)) %>%
  # Recoding variables so they are easily interpretable
  dplyr::mutate(gender = case_when(gender == 1 ~ "man",
                                   gender == 2 ~ "woman",
                                   gender == 3 ~ "nonbinary", 
                                   gender == 4 ~ "questioning",
                                   gender == 5 ~ "prefer not to answer",
                                   TRUE ~ "gender not listed"),
                ethnicity = case_when(ethnicity == 1 ~ "Hispanic/Latino(a/e)",
                                      ethnicity == 2 ~ "Not Hispanic/Latino(a/e)",
                                      TRUE ~ "prefer not to answer"),
                race = case_when(race == 1 ~ "white/Caucasian",
                                 race == 2 ~ "Black/African American",
                                 race == 3 ~ "Asian/Asian American",
                                 race == 4 ~ "Native Hawaiian or Other Pacific Islander",
                                 race == 5 ~ "Native American or Alaska Native",
                                 race == 6 ~ "Biracial or Multiracial",
                                 race == 7 ~ "prefer not to answer",
                                 TRUE ~ "race not listed"),
                native_lang = ifelse(native_lang == 1, "American English", "Not American English"),
                commdis_hx = ifelse(commdis_hx == 1, "no", "yes"),
                fam_commdis = ifelse(fam_commdis == 1, "no", "yes"),
                fam_comdis_2 = case_when(fam_comdis_2 == 1 ~ "no experience",
                                         fam_comdis_2 == 2 ~ "healthcare worker",
                                         fam_comdis_2 == 3 ~ "family member",
                                         fam_comdis_2 == 4 ~ "friend",
                                         TRUE ~ "teacher"),
                freq_commdis = case_when(freq_commdis == 1 ~ "never",
                                         freq_commdis == 2 ~ "once",
                                         freq_commdis == 3 ~ "yearly",
                                         freq_commdis == 4 ~ "monthly",
                                         freq_commdis == 5 ~ "weekly",
                                         TRUE ~ "daily"))

# Removing participants who did not complete pupillometry part of the study

## Technical issues occurred during the recordings for these participants. 
## So we are removing them from our df.

demo1 <- demo1 %>%
  dplyr::filter(id != "LE14" & id != "LE17")
  

# Exporting cleaned demographic df

## Set working directory

setwd("~/Documents/Listening-Effort-in-Dysarthria/ASHA 2023 Analysis/Cleaned Data")

## Export

rio::export(demo1, "cleaned_listener_demo.csv")
