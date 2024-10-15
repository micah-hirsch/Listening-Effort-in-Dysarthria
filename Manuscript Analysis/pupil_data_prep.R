# Data Preparation

# Author: Micah E. Hirsch, mhirsch@fsu.edu

## Date: 10/10/2024

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

setwd("D:\\Listening Effort Study\\Raw Data\\Extracted Pupil and PLE Ratings")

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

# Creating a separate df for perceived listening effort ratings 
## (will be exported later)
ple_data <- pupil_data |>
  dplyr::select(subject, trial, code, speaker, targetphrase, counterbalance, effort_rating) |>
  dplyr::distinct() |>
  ## Removing Practice Trials
  dplyr::filter(speaker != "Practice") |>
  ## I noted some erroneous trials, so I am removing them
  dplyr::filter(speaker != "UNDEFINED") |>
  ## Adjusting trial order labels since the erroneous trials were extra trials
  dplyr::mutate(trial = case_when(subject == "LE31" & trial >= 24 ~ trial - 1,
                                  subject == "LE34" ~ trial -1,
                                  TRUE ~ trial))

## Need to make the same adjustments to the pupil dilation df
pupil_data <- pupil_data |>
  dplyr::select(!effort_rating) |>
  dplyr::filter(speaker != "UNDEFINED") |>
  dplyr::mutate(trial = case_when(subject == "LE31" & trial >= 24 ~ trial - 1,
                                  subject == "LE34" ~ trial -1,
                                  TRUE ~ trial))


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

# Detect amount of missing data per trial due to blinks

## Calculating Percent of Missing Data
missing_pupil <- trimmed_pupil_data |>
  dplyr::group_by(subject, trial) |>
  ## Restricting this to the eventual analysis region of interest
  dplyr::filter(time >= -500) |>
  dplyr::filter(time < max(time) - 2000) |>
  dplyr::ungroup() |>
  # Counting number of blink/no blink rows per trial
  dplyr::group_by(subject, trial, blink) |>
  dplyr::summarize(blinks = n()) |>
  dplyr::ungroup() |>
  dplyr::mutate(blink = ifelse(blink == 0, "no_blink", "blink")) |>
  tidyr::pivot_wider(names_from = blink, values_from = blinks) |>
  dplyr::mutate(percent_missing = (blink/(no_blink + blink))*100) |>
  dplyr::select(subject, trial, percent_missing)

## Merging with main df
trimmed_pupil_data <- trimmed_pupil_data |>
  dplyr::left_join(missing_pupil, by = c("subject", "trial"))

## Finding out how many trials are removed due to blinks (6 trials)
missing <- trimmed_pupil_data |>
  filter(percent_missing >= 50) |>
  dplyr::select(subject, trial) |>
  dplyr::distinct()

## Filtering out trials with greater than 50% of missing data
trimmed_pupil_data <- trimmed_pupil_data |>
  dplyr::mutate(percent_missing = ifelse(is.na(percent_missing), 0, percent_missing)) |>
  dplyr::filter(percent_missing < 50)

rm(missing, missing_pupil)

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
  dplyr::select(c(subject, trial, sample_message, time, 
                  code, speaker, targetphrase, counterbalance,
                  smoothed_pupil)) |>
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

## Proportion of rows removed (as of 5/9/2024: 1.44%)
((nrow(baseline_pupil) - nrow(mad_removal)) / nrow(baseline_pupil)) * 100

## Checking to see if whole trials were removed from any of the participants (No Additional Trials Removed)
trial_check <- mad_removal |>
  select(subject, trial) |>
  distinct() |>
  group_by(subject) |>
  summarize(n = n())

## Removing unneeded items from the environment
rm(baseline_pupil, interp, pupil_extend, smoothed, trimmed_pupil_data, trial_check)

# Outlier Flags

## Baseline Deviation
baseline_dev <- mad_removal |>
  dplyr::group_by(subject) |>
  dplyr::summarize(mean_base = mean(baseline, na.rm = T),
                   sd_base = sd(baseline, na.rm = T)) |>
  dplyr::ungroup() |>
  dplyr::mutate(base_max = mean_base + (2*sd_base),
                base_min = mean_base - (2*sd_base))

## Peak Pupil Value Deviation
peak_pupil_dev <- mad_removal |>
  dplyr::group_by(subject, trial) |>
  dplyr::summarize(peak_pupil = max(baselinecorrectedp)) |>
  dplyr::ungroup() |>
  dplyr::group_by(subject) |>
  dplyr::summarize(mean_peak = mean(peak_pupil, na.rm = T),
                   sd_peak = sd(peak_pupil, na.rm = T)) |>
  dplyr::ungroup() |>
  dplyr::mutate(peak_max = mean_peak + (2*sd_peak),
                peak_min = mean_peak - (2*sd_peak))

## Trial-by-trial Baseline Deviation
baseline_flags <- mad_removal |>
  dplyr::select(subject, trial, baseline) |>
  dplyr::distinct() |>
  dplyr::group_by(subject) |>
  dplyr::mutate(speed = speed_pupil(baseline, trial),
                MAD = calc_mad(speed, n=16)) |>
  dplyr::ungroup()

## Odd Pupil Slope Detection

slope_df <- mad_removal |>
  ### Limiting range to first 500 ms after stimulus onset
  dplyr::filter(time >= 0 & time <= 500) |>
  group_by(subject, trial) |>
  ### Calculating change in pupil dilation (i.e. slope) in the first 500 ms
  dplyr::mutate(pupil_slope = (last(baselinecorrectedp) - first(baselinecorrectedp))/ (last(time) - first(time))) |>
  dplyr::ungroup()


### Visually determining a cutoff point for steep downward slope by plotting the histogram
### Based on this visualization, pupil slope change greater than -.55 will be the cutoff
slope_df |>
  dplyr::select(subject, trial, speaker, pupil_slope) |>
  distinct() |>
  ggplot() +
  aes(x = pupil_slope,
      fill = speaker,
      color = speaker) +
  geom_histogram() +
  geom_vline(xintercept = -.55)

### Calculating mean and sd of pupil slopes (M = .0194, sd = .271)
slope_df |>
  distinct() |>
  dplyr::summarize(mean = mean(pupil_slope,),
                   sd = sd(pupil_slope))

### Flagging Trials with Steep Negative Pupil Dilation Slopes
slope_df <- slope_df |>
  dplyr::mutate(steep_slope = ifelse(pupil_slope <= -.55, TRUE, FALSE))

## Creating Flag Variables and merging with original df

### Baseline Deviation

baseline_dev <- baseline_dev |>
  dplyr::select(subject, base_min, base_max) 

mad_removal <- mad_removal |>
  dplyr::left_join(baseline_dev, by = "subject") |>
  dplyr::mutate(base_dev = ifelse(baseline < base_min | baseline > base_max, TRUE, FALSE)) 

### Peak Pupil Deviation
  
peak_pupil_dev <- peak_pupil_dev |>
  dplyr::select(subject, peak_min, peak_max) 

mad_removal <- mad_removal |>
  dplyr::left_join(peak_pupil_dev, by = "subject") |>
  dplyr::group_by(subject, trial) |>
  dplyr::mutate(peak_dev = ifelse(max(baselinecorrectedp) < peak_min | max(baselinecorrectedp) > peak_max, TRUE, FALSE)) |>
  dplyr::ungroup()

### Trial by Trial Baseline Deviation

baseline_flags <- baseline_flags |>
  mutate(trial_base_dev = ifelse(speed >= MAD, TRUE, FALSE)) |>
  dplyr::select(subject, trial, trial_base_dev)

mad_removal <- mad_removal |>
  dplyr::left_join(baseline_flags, by = c("subject", "trial"))

### Steep Pupil Slope

slope_df <- slope_df |>
  dplyr::select(subject, trial, steep_slope) |>
  dplyr::distinct()

mad_removal <- mad_removal |>
  dplyr::left_join(slope_df, by = c("subject", "trial"))

### Removing Extra Variables

mad_removal <- mad_removal |>
  dplyr::select(!c(base_min, base_max, peak_min, peak_max))

### Identifying outlier trials that will be removed (21 trials)
removed_df <- mad_removal |>
  group_by(subject, trial) |>
  dplyr::filter(rowSums(across(base_dev:steep_slope)) >= 2)

removed_df <- removed_df |>
  dplyr::select(subject, trial, speaker) |>
  dplyr::distinct()

### Filtering out those responses 

filtered_df <- mad_removal |>
  group_by(subject, trial) |>
  dplyr::filter(rowSums(across(base_dev:steep_slope)) < 2)

# Removing unneeded objects from the environment
rm(baseline_dev, baseline_flags, mad_removal, peak_pupil_dev, removed_df, slope_df)

# Downsampling

bin.length <- 20

data.binned <- filtered_df |>
  mutate(timebins = round(time/bin.length)*bin.length) |>
  dplyr::group_by(subject, trial, speaker, timebins,
                  code, targetphrase, counterbalance) |>
  dplyr::summarize(pupil.binned = mean(baselinecorrectedp)) |>
  dplyr::ungroup()

# Downsampling ALS speaker trials

## Based on findings from the 2023 ASHA Convention data analysis,the trials from the ALS speaker are much longer than the control talker.
## Therefore we are creating a separate df that downsamples the ALS speaker's trials.

bin.length <- 48.5

ALS_trials <- data.binned |>
  dplyr::filter(speaker == "ALS") |>
  dplyr::group_by(subject, trial) |>
  dplyr::mutate(time_n = case_when(timebins >= 0 & timebins <= max(timebins) - 3000 ~ round(timebins/bin.length)*bin.length,
                TRUE ~ timebins)) |>
  dplyr::ungroup() |>
  dplyr::group_by(subject, trial, speaker, time_n, code, targetphrase, counterbalance) |>
  dplyr::summarize(normed_pupil = mean(pupil.binned)) |>
  dplyr::ungroup()
    
control_trials <- data.binned |>
  dplyr::filter(speaker == "Control") |>
  dplyr::rename(time_n = timebins,
                normed_pupil = pupil.binned)

normed_data <- rbind(ALS_trials, control_trials)

normed_data <- normed_data |>
  dplyr::mutate(time_norm = case_when(speaker == "ALS" & time_n > 0 ~ time_n/1.5,
                                      TRUE ~ time_n))
normed_data |>
  dplyr::filter(time_norm >= 0) %>%
  dplyr::group_by(speaker, code) %>%
  dplyr::summarize(length = max(time_norm) - min(time_norm)) %>%
  dplyr::group_by(speaker) %>%
  dplyr::summarize(av_length = mean(length),
                   av_phrase = av_length - 3000,
                   av_end_roi = av_length - 2000)

# Export data

## Set working directory
setwd("C:\\Users\\mehirsch\\Documents\\GitHub\\Listening-Effort-in-Dysarthria\\Manuscript Analysis\\Cleaned Data")

## Export Pupil Dilation DF
rio::export(data.binned, "cleaned_pupil_data.csv")
rio::export(normed_data, "cleaned_pupil_data_normalized.csv")

## Export PLE Ratings
rio::export(ple_data, "cleaned_ple_data.csv")


