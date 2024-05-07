# Data Preparation

# Author: Micah E. Hirsch, mhirsch@fsu.edu

## Data: 5/7/2024

## Purpose: To prepare the pupil dilation data for analysis.

# Loading Needed Packages

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
    
    data <- data |>
      # Creating pupil and blink variables. 
      # Most participants had their right eye tracked, but some had their left eye tracked.
      dplyr::mutate(pupil = ifelse(EYE_TRACKED == "Right", RIGHT_PUPIL_SIZE, LEFT_PUPIL_SIZE),
                    blink = ifelse(EYE_TRACKED == "Right", RIGHT_IN_BLINK, LEFT_IN_BLINK)) %>%
      # Bring variable names to lower case
      dplyr::rename_all(., .funs = tolower) |>
      # Renaming variables so it is consistent with gazer's requirements
      dplyr::rename(subject = recording_session_label,
                    trial = trial_index) |>
      # Selecting needed variables for processing
      dplyr::select(subject, trial, eye_tracked, blink, timestamp, pupil, ip_start_time, 
                    sample_message, effort_rating, code, speaker, practicetrial, 
                    targetphrase, counterbalance) |>
      # Removing the first 15 trials
      dplyr::filter(!trial %in% (1:15)) |>
      # removing _2 from these subject ids
      dplyr::mutate(subject = str_replace(subject, "_2", ""))
    
  } else {
    
    ## This block of code does the same thing as above, except the first 15 trials
    ## are not removed
    data <- data |>
      dplyr::mutate(pupil = ifelse(EYE_TRACKED == "Right", RIGHT_PUPIL_SIZE, LEFT_PUPIL_SIZE),
                    blink = ifelse(EYE_TRACKED == "Right", RIGHT_IN_BLINK, LEFT_IN_BLINK)) %>%
      dplyr::rename_all(., .funs = tolower) |>
      dplyr::rename(subject = recording_session_label,
                    trial = trial_index) |>
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
trial_start <- pupil_data |>
  dplyr::filter(grepl("TRIAL_START", sample_message)) |>
  dplyr::select(subject, trial, start_time = timestamp)

## Extracting phrase start times
phrase_start <- pupil_data |>
  dplyr::filter(grepl("PHRASE_START", sample_message)) |>
  dplyr::select(subject, trial, phrase_start = timestamp)

## Extracting phrase end times
phrase_end <- pupil_data |>
  dplyr::filter(grepl("PHRASE_END", sample_message)) |>
  dplyr::select(subject, trial, phrase_end = timestamp)

## Extracting trial end times (e.g. time response cue was presented)
trial_end <- pupil_data |>
  dplyr::filter(grepl("RESPONSE_CUE", sample_message)) |>
  dplyr::select(subject, trial, end_time = timestamp)

## Merging the dfs together 
time_landmarks <- trial_start |>
  dplyr::left_join(phrase_start, by = c("subject", "trial")) |>
  dplyr::left_join(phrase_end, by = c("subject", "trial")) |>
  dplyr::left_join(trial_end, by = c("subject", "trial"))

rm(trial_start, trial_end, phrase_start, phrase_end)

pupil_data <- dplyr::left_join(pupil_data, time_landmarks, by = c("subject", "trial"))

rm(time_landmarks)

## Filter out unneeded rows and trials
trimmed_pupil_data <- pupil_data |>
  ## Filtering out rows before trial start and after trial end
  dplyr::filter(timestamp >= start_time & timestamp <= end_time) |>
  ## Removing practice trials from df
  dplyr::filter(practicetrial != 'Practice') |>
  dplyr::select(!practicetrial) |>
  ## Aligning data to onset of phrase presentation
  dplyr::mutate(time = timestamp - phrase_start) |>
  ## Removing unneeded variables
  dplyr::select(!c(timestamp, start_time:end_time)) |>
  dplyr::relocate(time, .after = pupil)

rm(pupil_data)

trimmed_pupil_data <- gazer::count_missing_pupil(trimmed_pupil_data, missingthresh = 0.5)

# Detect amount of missing data per trial due to blinks

per_blinks <- trimmed_pupil_data |>
  dplyr::group_by(subject, trial) |>
  dplyr::filter(time >= -500) |>
  dplyr::filter(time < max(time) - 2000) |>
  dplyr::ungroup() |>
  dplyr::group_by(subject, trial, blink) |>
  dplyr::summarize(blinks = n()) |>
  dplyr::ungroup() |>
  dplyr::mutate(blink = ifelse(blink == 0, "no_blink", "blink")) |>
  tidyr::pivot_wider(names_from = blink, values_from = blinks) |>
  dplyr::mutate(percent_missing = (blink/(no_blink + blink))*100)



# Fill in missing data from blinks

## Deblinking
pupil_extend <- trimmed_pupil_data |>
  dplyr::group_by(subject, trial) |>
  dplyr::mutate(extendpupil = extend_blinks(pupil, 
                                            fillback = 50, 
                                            fillforward = 160, 
                                            hz = 1000))

## Linear interpolation
interp <- interpolate_pupil(pupil_extend,
                            extendblinks = T, 
                            type = "linear", 
                            hz = 1000)

## 10 Hz 5-point moving average filter
smoothed <- interp |>
  dplyr::mutate(smoothed_pupil = moving_average_pupil(interp, n = 5)) |>
  ## Selecting relevant variables
  dplyr::select(c(subject, trial, sample_message, time, effort_rating, 
                  code, speaker, targetphrase, counterbalance,smoothed_pupil)) |>
  dplyr::relocate(smoothed_pupil, .after = time)

# Baseline Pupil Correction

baseline_pupil <- baseline_correction_pupil(smoothed, pupil_colname = "smoothed_pupil",
                                            baseline_window = c(-500, 0))

# Artifact Rejection

## Looking for rapid changes in pupil dilation using median absolute deviation
mad_removal <- baseline_pupil |>
  dplyr::group_by(subject, trial) |>
  dplyr::mutate(speed = speed_pupil(baselinecorrectedp, time)) |>
  dplyr::mutate(MAD = calc_mad(speed, n=16)) |>
  dplyr::filter(speed < MAD)

## Proportion of rows removed (as of 4/15/2024: 1.45%)
((nrow(baseline_pupil) - nrow(mad_removal)) / nrow(baseline_pupil)) * 100

## Checking to see if whole trials were removed from any of the participants (No Trials Removed)
trial_check <- mad_removal |>
  select(subject, trial) |>
  distinct() |>
  group_by(subject) |>
  summarize(n = n())

## Removing unneeded items from the environment
rm(baseline_pupil, interp, pupil_extend, smoothed, trimmed_pupil_data, trial_check)

# Downsampling

bin.length <- 20

data.binned <- mad_removal |>
  mutate(timebins = round(time/bin.length)*bin.length) |>
  dplyr::group_by(subject, trial, speaker, timebins, effort_rating,
                  code, targetphrase, counterbalance) |>
  dplyr::summarize(pupil.binned = mean(baselinecorrectedp)) |>
  dplyr::ungroup()

# Export Cleaned Data

## Set working directory
setwd("~/Documents/Listening-Effort-in-Dysarthria/Manuscript Analysis/Cleaned Data")

## Export
rio::export(data.binned, "cleaned_pupil_data.csv")

# Downsampling ALS speaker trials
## Based on findings from the 2023 ASHA Convention data analysis,the trials from the ALS speaker are much longer than the control talker.
## Therefore we are creating a separate df that downsamples the ALS speaker's trials.

bin.length <- 26.23

ALS_trials <- data.binned |>
  dplyr::filter(speaker == "ALS") |>
  dplyr::mutate(time_n = round(timebins/bin.length)*bin.length) |>
  dplyr::group_by(subject, trial, speaker, time_n, effort_rating, code, targetphrase, counterbalance) |>
  dplyr::summarize(normed_pupil = mean(pupil.binned)) |>
  dplyr::ungroup()

control_trials <- data.binned |>
  dplyr::filter(speaker == "Control") |>
  dplyr::rename(time_n = timebins,
                normed_pupil = pupil.binned)

normed_data <- rbind(ALS_trials, control_trials)

normed_data <- normed_data |>
  dplyr::mutate(time_norm = case_when(speaker == "ALS" ~ time_n/1.5,
                                      TRUE ~ time_n))

normed_data |>
  dplyr::filter(time_norm >= 0) %>%
  dplyr::group_by(speaker, code) %>%
  dplyr::summarize(length = max(time_norm) - min(time_norm)) %>%
  dplyr::group_by(speaker) %>%
  dplyr::summarize(av_length = mean(length),
                   av_phrase = av_length - 3000,
                   av_end_roi = av_length - 2000)

# Export Normalized dataset

rio::export(normed_data, "cleaned_pupil_data_normalized.csv")
