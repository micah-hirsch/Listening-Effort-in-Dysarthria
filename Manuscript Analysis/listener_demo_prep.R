# Participant Demographic Data Prep

# Author: Micah Hirsch, mhirsch@fsu.edu

# Date: 10/15/2024

## Purpose: To clean exported listener demographic data from RedCap and to
## prepare it for analysis.

library(rio) # install.packages("rio")
library(tidyverse) # install.packages("tidyverse")

# Load in listener demographic data from RedCap

## Set working directory
setwd("D:\\Listening Effort Study\\Raw Data\\Participant Info and Transcriptions")

demo <- rio::import("participant_demo_raw.csv")

## NOTE: LE03 withdrew from the study. Their data was removed from 
## RedCap prior to uploading the df here.

# Cleaning demographic data
## Note: The code below fixes an error in previous versions of this script 
## (seen in the ASHA and MSC folders)

demo1 <- demo |>
  # Removing identifiable and unneeded variables from df
  dplyr::select(!c(fluent_eng, demographic_information_complete)) %>%
  # Re-coding variables so they are easily interpretable
  ## NOTE: A coding error from previous file versions was fixed down below. 
  ## This is the correct version.
  dplyr::mutate(gender = case_when(gender == 1 ~ "Man",
                                   gender == 2 ~ "Woman",
                                   gender == 3 ~ "Nonbinary", 
                                   gender == 4 ~ "Questioning",
                                   gender == 5 ~ "Prefer not to answer",
                                   TRUE ~ "Gender not listed"),
                ethnicity = case_when(ethnicity == 1 ~ "Hispanic/Latino(a/e)",
                                      ethnicity == 2 ~ "Not Hispanic/Latino(a/e)",
                                      TRUE ~ "Prefer not to answer"),
                race = case_when(race == 1 ~ "white/Caucasian",
                                 race == 2 ~ "Black/African American",
                                 race == 3 ~ "Asian/Asian American",
                                 race == 4 ~ "Native Hawaiian or Other Pacific Islander",
                                 race == 5 ~ "Native American or Alaska Native",
                                 race == 6 ~ "Biracial or Multiracial",
                                 race == 7 ~ "Prefer not to answer",
                                 TRUE ~ "Race not listed"),
                native_lang = ifelse(native_lang == 1, "American English", "Not American English"),
                commdis_hx = ifelse(commdis_hx == 1, "no", "yes"),
                fam_commdis = ifelse(fam_commdis == 1, "no", "yes"),
                fam_comdis_2 = case_when(fam_comdis_2 == 1 ~ "no experience",
                                         fam_comdis_2 == 2 ~ "healthcare worker",
                                         fam_comdis_2 == 3 ~ "family member",
                                         fam_comdis_2 == 4 ~ "friend",
                                         fam_comdis_2 == 5 ~ "teacher",
                                         TRUE ~ NA),
                freq_commdis = case_when(freq_commdis == 1 ~ "never",
                                         freq_commdis == 2 ~ "once",
                                         freq_commdis == 3 ~ "yearly",
                                         freq_commdis == 4 ~ "monthly",
                                         freq_commdis == 5 ~ "weekly",
                                         freq_commdis == 6 ~ "daily",
                                         TRUE ~ NA))

# Marking participants who did not complete pupillometry part of the study
## Technical issues occurred during the recordings for these participants. 

demo1 <- demo1 |>
  dplyr::mutate(pupil_complete = case_when(id == "LE14" ~ "incomplete",
                                           id == "LE17" ~ "incomplete",
                                           id == "LE37" ~ "incomplete",
                                           id == "LE39" ~ "incomplete",
                                           TRUE ~ "complete"))

# Exporting cleaned demographic df

## Set working directory
setwd("C:\\Users\\mehirsch\\Documents\\GitHub\\Listening-Effort-in-Dysarthria\\Manuscript Analysis\\Cleaned Data")

## Export
rio::export(demo1, "cleaned_listener_demo.csv")
