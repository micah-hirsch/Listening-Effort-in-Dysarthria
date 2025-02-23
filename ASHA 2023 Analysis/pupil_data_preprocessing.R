# Pupil Data Preprocessing

# Author: Micah E. Hirsch, mhirsch@fsu.edu 

# Date: 10/26/2023

## Purpose: To load in raw data from Eyelink and prepare pupil data viz and analysis.
## This particular data processing and analysis is being completed for a poster 
## presentation at the Annual American Speech Language Hearing Association Convention 
## being held in Boston, MA in November 2023.

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
  
  ## Import file
  data <- rio::import(file) 
  
  ## For subject id's ending in _2, we have to discard their first 15 trials
  ## because the experiment was restarted for them after the 15th trial
  ## (due to technical errors). The first 15 trials were recorded in 
  ## the initial output file for the participant.
  if (any(grepl("_2", data$RECORDING_SESSION_LABEL))) {
    
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
                    targetphrase, counterbalance) %>%
      # Removing the first 15 trials
      dplyr::filter(!trial %in% (1:15)) %>%
      # removing _2 from these subject ids
      dplyr::mutate(subject = str_replace(subject, "_2", ""))
    
  } else {
    
    ## This block of code does the same thing as above, except the first 15 trials
    ## are not removed
    data <- data %>%
      dplyr::mutate(pupil = ifelse(EYE_TRACKED == "Right", RIGHT_PUPIL_SIZE, LEFT_PUPIL_SIZE),
                    blink = ifelse(EYE_TRACKED == "Right", RIGHT_IN_BLINK, LEFT_IN_BLINK)) %>%
      dplyr::rename_all(., .funs = tolower) %>%
      dplyr::rename(subject = recording_session_label,
                    trial = trial_index) %>%
      dplyr::select(subject, trial, eye_tracked, blink, timestamp, pupil, ip_start_time, 
                    sample_message, effort_rating, code, speaker, practicetrial, 
                    targetphrase, counterbalance)
    
  }
  

  ## Add each participant's data to the empty data list
  data_list[[length(data_list)+1]] <- data
  
}

## Merging each participant's data into one dataframe
pupil_data <- do.call(rbind, data_list)

## Removing unneeded items from the environment

rm(data, data_list, file, file_list)

# Filtering out rows before the trial start and after the response cue

## Extracting trial start times

trial_start <- pupil_data %>%
  dplyr::filter(sample_message == "TRIAL_START") %>%
  dplyr::select(subject, trial, start_time = timestamp)

## Extracting phrase start times

phrase_start <- pupil_data %>%
  dplyr::filter(sample_message == "PHRASE_START") %>%
  dplyr::select(subject, trial, phrase_start = timestamp)

## Extracting phrase end times

phrase_end <- pupil_data %>%
  dplyr::filter(sample_message == "PHRASE_END") %>%
  dplyr::select(subject, trial, phrase_end = timestamp)

## Extracting trial end times (e.g. time response cue was presented)

trial_end <- pupil_data %>%
  dplyr::filter(sample_message == "RESPONSE_CUE") %>%
  dplyr::select(subject, trial, end_time = timestamp)

## Merging the dfs together

pupil_data2 <- pupil_data %>%
  dplyr::left_join(trial_start, by = c("subject", "trial")) %>%
  dplyr::left_join(phrase_start, by =c("subject", "trial")) %>%
  dplyr::left_join(phrase_end, by = c("subject", "trial")) %>%
  dplyr::left_join(trial_end, by = c("subject", "trial"))

## Filter out unneeded rows and trials

trimmed_pupil_data <- pupil_data2 %>%
  ## Filtering out rows before trial start and after trial end
  dplyr::filter(timestamp >= start_time & timestamp <= end_time) %>%
  ## Removing practice trials from df
  dplyr::filter(practicetrial != 'Practice') %>%
  dplyr::select(!practicetrial) %>%
  ## Aligning data to onset of phrase presentation
  dplyr::mutate(time_c = timestamp - phrase_start) %>%
  ## Removing unneeded variables
  dplyr::select(!c(timestamp, start_time:end_time)) %>%
  dplyr::relocate(time_c, .after = pupil)

## Remove unneeded items from environment 

rm(phrase_end, phrase_start, pupil_data, pupil_data2, trial_end, trial_start)

## Removing trials/participants with too much data loss

trimmed_pupil_data <- gazer::count_missing_pupil(trimmed_pupil_data, missingthresh = 0.5)

# Fill in missing data from blinks

## Deblinking

pupil_extend <- trimmed_pupil_data %>%
  dplyr::group_by(subject, trial) %>%
  dplyr::mutate(extendpupil = extend_blinks(pupil, 
                                            fillback = 50, 
                                            fillforward = 160, 
                                            hz = 1000))

## linear interpolation
interp <- interpolate_pupil(pupil_extend,
                            extendblinks = T, 
                            type = "linear", 
                            hz = 1000)

## 10 Hz 5-point moving average filter
smoothed <- interp %>%
  dplyr::mutate(smoothed_pupil = moving_average_pupil(interp, n = 5)) %>%
  ## Selecting relevant variables
  dplyr::select(c(subject, trial, sample_message, time_c, effort_rating, 
                  code, speaker, targetphrase, counterbalance,smoothed_pupil)) %>%
  dplyr::relocate(smoothed_pupil, .after = time_c) %>%
  dplyr::rename(time = time_c)

# Baseline correction

baseline_pupil <- baseline_correction_pupil(smoothed, pupil_colname = "smoothed_pupil",
                                            baseline_window = c(-500, 0))

# Artifact Rejection

## Looking for rapid changes in pupil dilation using median absolute deviation

mad_removal <- baseline_pupil %>%
  dplyr::group_by(subject, trial) %>%
  dplyr::mutate(speed = speed_pupil(baselinecorrectedp, time)) %>%
  dplyr::mutate(MAD = calc_mad(speed, n=16)) %>%
  dplyr::filter(speed < MAD)

## Proportion of rows removed (as of 10/24/2023: 1.28%)

((nrow(baseline_pupil) - nrow(mad_removal)) / nrow(baseline_pupil)) * 100

### Checking to see if whole trials were removed from any of the participants (1 trial from LE10)

mad_removal %>%
  select(subject, trial) %>%
  distinct() %>%
  group_by(subject) %>%
  summarize(n = n())

## Removing unneeded items from the environment

rm(baseline_pupil, interp, pupil_extend, smoothed, trimmed_pupil_data)

# Downsampling

bin.length <- 20

data.binned <- mad_removal %>%
  mutate(timebins = round(time/bin.length)*bin.length) %>%
  dplyr::group_by(subject, trial, speaker, timebins, effort_rating,
                  code, targetphrase, counterbalance) %>%
  dplyr::summarize(pupil.binned = mean(baselinecorrectedp)) %>%
  dplyr::ungroup()

# Set working directory to export data

setwd("~/Documents/Listening-Effort-in-Dysarthria/ASHA 2023 Analysis/Cleaned Data")

rio::export(data.binned, "cleaned_pupil_data.csv")
