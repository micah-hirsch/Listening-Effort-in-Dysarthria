# Data Preprocessing

# Author: Micah E. Hirsch

# Date: 7/27/2023 
# Current Version - Pilot Data Preprocessing

## Purpose: To load in raw pupil dilation data from EyeLink and 
## prepare data for analysis and visualization.

## Note: This script is a work-in-progress.

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

# Checking the interval timing for the baseline and post-phrase retention periods.
## We are doing this because there were some concerns about the timing of some experimental events.

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
                      values_to = "interval") 

interval_summary <- pupil_data1 %>%
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

# Filtering out trials/participants with too much data loss.
## None removed 

trimmed_pupil_data <- gazer::count_missing_pupil(trimmed_pupil_data, missingthresh = 0.5)

# Prepping the df for pupil processing

## Extracting phrase onset times
phrase_start <- pupil_data %>%
  dplyr::filter(sample_message == "PHRASE_START") %>%
  dplyr::select(subject, trial, phrase_start_time = timestamp)

## Merging phrase onset times with pupil df
trimmed_pupil_data <- trimmed_pupil_data %>%
  dplyr::left_join(phrase_start, by = c("subject", "trial"))

trimmed_pupil_data <- trimmed_pupil_data %>%
  ## Removing practice trials from df
  dplyr::filter(practicetrial != 'Practice') %>%
  dplyr::select(!practicetrial) %>%
  ## Aligning data to onset of phrase presentation
  dplyr::mutate(time_c = timestamp - phrase_start_time) %>%
  ## Removing unneeded variables
  dplyr::select(!c(timestamp, time:phrase_start_time)) %>%
  dplyr::relocate(time_c, .after = pupil)
  

## removing unneeded objects from the environment

rm(trial_start, trial_end, phrase_start, file_list, pupil_data2, pupil_data1, pupil_data, interval_summary)

# Deblinking

pupil_extend <- trimmed_pupil_data %>%
  dplyr::group_by(subject, trial) %>%
  dplyr::mutate(extendpupil = extend_blinks(pupil, fillback = 50, fillforward = 160, hz = 1000))

# Interpolation and Smoothing

## linear interpolation
interp <- interpolate_pupil(pupil_extend,
                            extendblinks = T, 
                            type = "linear", 
                            hz = 1000)

## 10 Hz 5-point moving average filter
smoothed <- interp %>%
  dplyr::mutate(smoothed_pupil = moving_average_pupil(interp, n = 5)) %>%
  ## Selecting relevant variables
  dplyr::select(c(subject, trial, sample_message, time_c, code, speaker, targetphrase, counterbalance, smoothed_pupil)) %>%
  dplyr::relocate(smoothed_pupil, .after = time_c) %>%
  dplyr::rename(time = time_c)

# Baseline Correction

baseline_pupil <- baseline_correction_pupil(smoothed, pupil_colname = "smoothed_pupil",
                                            baseline_window = c(-500, 0))

# Artifact Rejection

## Looking for rapid changes in pupil dilation using median absolute deviation

mad_removal <- baseline_pupil %>%
  dplyr::group_by(subject, trial) %>%
  dplyr::mutate(speed = speed_pupil(baselinecorrectedp, time)) %>%
  dplyr::mutate(MAD = calc_mad(speed, n=16)) %>%
  dplyr::filter(speed < MAD)

## Proportion of rows removed (7/27/23: 0.104%)
((nrow(baseline_pupil) - nrow(mad_removal)) / nrow(baseline_pupil)) * 100

# Removing unneeded items from environment

rm(baseline_pupil, interp, pupil_extend, smoothed, trimmed_pupil_data)

# Downsampling

bin.length <- 20

data.binned <- mad_removal %>%
  mutate(timebins = round(time/bin.length)*bin.length) %>%
  dplyr::group_by(subject, trial, speaker, timebins, code, targetphrase, counterbalance) %>%
  dplyr::summarize(pupil.binned = mean(baselinecorrectedp)) %>%
  dplyr::ungroup()

# Exporting Data (7/27/23: Pilot Data)

rio::export(data.binned, "cleaned_pupil_data.csv")
