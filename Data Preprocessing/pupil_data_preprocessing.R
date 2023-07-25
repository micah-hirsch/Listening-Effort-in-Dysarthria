# Data Preprocessing

# Author: Micah E. Hirsch

# Date: 7/25/2023 (currently wip - Preliminary Processing)

## Purpose: To load in raw pupil dilation data from EyeLink

# Loading required packages

library(rio) # install.packages("rio")
library(tidyverse) # install.packages("tidyverse")
## Need this to install gazer and saccades packages
library(remotes) # install.packages("remotes")
library(gazer) # remotes::install_github("dmirman/gazer")
library(saccades) # remotes::install_github("tmalsburg/saccades/saccades")
library(zoo) # install.packages("zoo")
library(knitr) # install.packages("knitr")

setwd("~/Documents/Listening-Effort-in-Dysarthria/Data Preprocessing/Raw Data")

# Loading raw pupil data

file_list <- list.files(path = ".", pattern - ".txt")

pupil_data <- merge_pupil(file_list, blink_colname = "RIGHT_IN_BLINK", pupil_colname = "RIGHT_PUPIL_SIZE")

mode = function(x) {
  u = unique(x)
  tab <-tabulate(match(x, u))
  u[tab == max(tab)]
}

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

rio::export(pupil_data1, "interval_timing.csv")
