# Data Preprocessing

# Author: Micah E. Hirsch

# Date: 7/26/2023 (Current Version - Pilot Data, Note: This script is a work-in-progress)

## Purpose: To load in raw pupil dilation data from EyeLink, 
## check timing intervals for baseline and retention periods, 
## and filter out excess rows.

# Loading required packages

library(rio) # install.packages("rio")
library(tidyverse) # install.packages("tidyverse")
## Need remotes to install gazer and saccades packages
library(remotes) # install.packages("remotes")
library(gazer) # remotes::install_github("dmirman/gazer")
library(saccades) # remotes::install_github("tmalsburg/saccades/saccades")
library(zoo) # install.packages("zoo")
library(knitr) # install.packages("knitr")

# Setting the working directory

setwd("~/Documents/Listening-Effort-in-Dysarthria/Data Preprocessing/Raw Data")

# Loading raw pupil data

file_list <- list.files(path = ".", pattern = ".txt")

pupil_data <- merge_pupil(file_list, blink_colname = "RIGHT_IN_BLINK", pupil_colname = "RIGHT_PUPIL_SIZE")

# Checking the interval timing for the baseline and post-phrase retention periods

## Creating mode function

mode = function(x) {
  u = unique(x)
  tab <-tabulate(match(x, u))
  u[tab == max(tab)]
}

## Filtering out landmarks of interest relating to start of trial, phrase onsets and offsets, and presentation of response cue

pupil_data1 <- pupil_data %>%
  dplyr::filter(sample_message %in% c("TRIAL_START", "PHRASE_START", "PHRASE_END", "RESPONSE_CUE")) %>%
  dplyr::select(c('subject', 'trial', 'timestamp', 'sample_message')) %>%
  tidyr::pivot_wider(names_from = 'sample_message', values_from = 'timestamp') %>%
  dplyr::mutate(baseline_ret = PHRASE_START - TRIAL_START,
                retention = RESPONSE_CUE - PHRASE_END) %>%
  dplyr::select(!c(TRIAL_START:RESPONSE_CUE)) %>%
  tidyr::pivot_longer(cols = c(baseline_ret, retention),
                      names_to = "silence",
                      values_to = "interval") %>%
  dplyr::group_by(silence) %>%
  dplyr::summarize(min = min(interval), max = max(interval), mean = mean(interval), median = median(interval), mode = mode(interval))

# Filtering rows before the start of a trial and after the response cue.

## Extract timestamps for start of each trial
trial_start <- pupil_data %>%
  dplyr::filter(sample_message == "TRIAL_START") %>%
  dplyr::select(subject, trial, start_time = timestamp)

## Extract end points (i.e. when the response cue was presented) of each trial
trial_end <- pupil_data %>%
  dplyr::filter(sample_message == "RESPONSE_CUE") %>%
  dplyr::select(subject, trial, end_time = timestamp)

## Merging the dfs together
pupil_data2 <- pupil_data %>%
  dplyr::left_join(trial_start, by = c("subject", "trial"))

pupil_data2 <- pupil_data2 %>%
  dplyr::left_join(trial_end, by = c("subject", "trial"))

## Filtering rows based on whether the timestamp value is before or after the start/end times
trimmed_pupil_data <- pupil_data2 %>%
  dplyr::filter(timestamp >= start_time & timestamp <= end_time)

## removing unneeded objects from the environment

rm(trial_start, trial_end, file_list, pupil_data2, pupil_data)

# Filtering out trials/participants with too much data loss.
## None removed 

trimmed_pupil_data <- gazer::count_missing_pupil(trimmed_pupil_data, missingthresh = 0.5)

# Removing unneeded variables from df and filtering out practice trials.

trimmed_pupil_data <- trimmed_pupil_data %>%
  dplyr::select(!c(time:averageMissingTrial)) %>%
  dplyr::filter(practiceTrial != 'Practice')

