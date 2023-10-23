# Pupil Data Preprocessing

# Author: Micah E. Hirsch, mhirsch@fsu.edu 

# Date: 10/23/2023

## Purpose: To load in raw data from Eyelink and prepare pupil data viz and analysis.
## This data processing and analysis is being completed for a poster presentation 
## at the Annual American Speech Language Hearing Association Convention being
## held in Boston, MA in November 2023.

# Loading Required Packages

library(rio) # install.packages("rio")
library(tidyverse) # install.packages("tidyverse")
## Need remotes to install gazer and saccades packages
library(remotes) # install.packages("remotes")
library(gazer) # remotes::install_github("dmirman/gazer")
library(saccades) # remotes::install_github("tmalsburg/saccades/saccades")
library(zoo) # install.packages("zoo")
library(knitr) # install.packages("knitr")

# Set the working directory to load data

setwd("~/Documents/Listening-Effort-in-Dysarthria/Raw Data")

# Load raw pupil files

## Get file names from raw data folder
file_list <- list.files(path = ".", pattern = ".txt")

## Create empty list to temporarily store imported participant data
data_list <- list()

## Initiating loop to load in data for each participant
for (file in file_list) {
  
  # Import file
  data <- rio::import(file) 
  
  data <- data %>%
    # Creating pupil and blink variables. 
    # Most participants had their right eye tracked, but some had their left eye tracked.
    dplyr::mutate(pupil = ifelse(EYE_TRACKED == "Right", RIGHT_PUPIL_SIZE, LEFT_PUPIL_SIZE),
                  blink = ifelse(EYE_TRACKED == "Right", RIGHT_IN_BLINK, LEFT_IN_BLINK)) %>%
    # Bring variable names to lower case
    dplyr::rename_all(., .funs = tolower) %>%
    # Renaming variables so it is consistent with gazer's requirements
    dplyr::rename(subject = recording_session_label,
                  trial = trial_index) %>%
    # Selecting needed variables for processing
    dplyr::select(subject, trial, eye_tracked, blink, timestamp, pupil, ip_start_time, 
                  sample_message, effort_rating, code, speaker, practicetrial, 
                  targetphrase, counterbalance)

  # Add each participant's data to the empty data list
  data_list[[length(data_list)+1]] <- data
  
}

# Merging each participant's data into one dataframe
pupil_data <- do.call(rbind, data_list)
