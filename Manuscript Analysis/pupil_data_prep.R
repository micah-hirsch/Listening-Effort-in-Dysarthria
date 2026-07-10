# Data Preparation

# Author: Micah E. Hirsch, mhirsch@fsu.edu

## Date: 7/10/2026

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
library(dtw) # install.packages("dtw")
library(gsignal) # install.packages("gsignal")
library(signal) # install.packages("signal")

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

# Finding number of people who had the left eye tracked (Left eye tracked: n = 9; 
# LE18 had both eyes tracked because the eye was switched in the middle of the experiment)
pupil_data |>
  dplyr::select(subject, eye_tracked) |>
  dplyr::distinct() |>
  dplyr::group_by(eye_tracked) |>
  dplyr::summarize(n = n()) 

# Filtering out rows before the trial start and after the response cue

audio_lengths <- rio::import("C:\\Users\\mehirsch\\Documents\\GitHub\\Listening-Effort-in-Dysarthria\\Manuscript Analysis\\Cleaned Data\\audio_lengths.csv")

## Extracting phrase start times
time_landmarks <- pupil_data |>
  full_join(audio_lengths, by = c("speaker", "code")) |>
  dplyr::filter(practicetrial != "Practice") |>
  dplyr::filter(grepl("PHRASE_START", sample_message)) |>
  dplyr::mutate(
    phrase_start = timestamp,
    trial_start = floor(phrase_start - 3000),
    phrase_end = round(phrase_start + duration_ms),
    trial_end = round(phrase_end + 3000)) |>
  dplyr::select(subject, trial, phrase_start:trial_end)


## Filter out unneeded rows and trials
trimmed_pupil_data <- pupil_data |>
  dplyr::left_join(time_landmarks, by = c("subject", "trial")) |>
  ## Filtering out rows before trial start and after trial end
  dplyr::filter(timestamp >= trial_start & timestamp <= trial_end) |>
  ## Removing practice trials from df
  dplyr::select(!practicetrial) |>
  ## Aligning data to onset of phrase presentation
  dplyr::mutate(time = timestamp - phrase_start,
                pupil = dplyr::na_if(pupil, "."),
                pupil = as.numeric(pupil)) |>
  ## Removing unneeded variables
  dplyr::select(!c(timestamp, phrase_start:trial_end)) |>
  dplyr::relocate(time, .after = pupil)

rm(pupil_data, time_landmarks, audio_lengths)

 # Smooth Data

## 10 Hz low pass filter
samp_rate <- 1000
cutoff <- 10
filter_order <- 1

nyquist <- samp_rate/2
w_c <- cutoff/nyquist

butter <- butter(filter_order, w_c, type = "low")

pupil_smoothed <- trimmed_pupil_data |>
  dplyr::group_by(subject, trial) |>
  dplyr::arrange(time, .by_group = T) |>
  dplyr::mutate(
    temp_clean = na.approx(pupil, na.rm = F),
    temp_clean = na.locf(na.locf(temp_clean, na.rm = F), fromLast = T),
    smoothed_pupil = {
      f_val <- dplyr::first(temp_clean)
      l_val <- dplyr::last(temp_clean)
      padded <- c(rep(f_val, 300), temp_clean, rep(l_val, 300))
      filtered <- filtfilt(filt = butter, x = padded)
      filtered[301:(length(filtered) - 300)] # Slices back to original length
    }, 
    smoothed_pupil = ifelse(is.na(pupil), NA_real_, smoothed_pupil)) |>
  dplyr::ungroup() |>
  ## Selecting relevant variables
  dplyr::select(c(subject, trial, sample_message, time, 
                  code, speaker, targetphrase, counterbalance, pupil,
                  smoothed_pupil)) |>
  dplyr::relocate(smoothed_pupil, .after = time)

interp |>
  dplyr::filter(speaker == "ALS") |>
  dplyr::filter(targetphrase == "account for who could knock") |>
  ggplot() +
  aes(
    x = time,
    y = final_pupil,
    group = subject) +
  geom_line(alpha = .6) +
  #coord_cartesian(xlim = c(4000, 5000)) +
  theme_bw()

pupil_blinks <- pupil_smoothed |>
  dplyr::group_by(subject, targetphrase) |>
  dplyr::mutate(
    is_na_samp = is.na(pupil),
    dt = time - dplyr::lag(time),
    dilation_vel = (smoothed_pupil - dplyr::lag(smoothed_pupil)) / dt,
    v_mad = median(abs(dilation_vel - median(dilation_vel, na.rm = TRUE)), na.rm = TRUE),
    dil_neg = ifelse(dilation_vel >= 0, 0, dilation_vel),
    is_blink = is_na_samp | (abs(dil_neg) > (8 * v_mad)),
    smoothed_pupil = ifelse(is_blink, NA, smoothed_pupil)
  )


## Deblinking
pupil_extend <- pupil_blinks |>
  dplyr::group_by(subject, trial) |>
  dplyr::mutate(
    near_blink = rollapply(
      is.na(smoothed_pupil),
      width = list(-160:50),
      FUN = any,
      partial = T
    ),
  extended = ifelse(near_blink, NA_real_, smoothed_pupil)
  )


# Detect amount of missing data per trial due to blinks

## Calculating Percent of Missing Data
missing_pupil <- pupil_extend |>
  dplyr::group_by(subject, trial) |>
  ## Restricting this to the eventual analysis region of interest
  dplyr::filter(time >= -500) |>
  dplyr::filter(time < max(time) - 2000) |>
  dplyr::ungroup() |>
  # Counting number of blink/no blink rows per trial
  dplyr::group_by(subject, trial, is_blink) |>
  dplyr::summarize(blinks = n()) |>
  dplyr::ungroup() |>
  dplyr::mutate(blink = ifelse(is_blink, "blink", "no_blink")) |>
  dplyr::select(-is_blink) |>
  tidyr::pivot_wider(names_from = blink, values_from = blinks) |>
  dplyr::mutate(percent_missing = (blink/(no_blink + blink))*100) |>
  dplyr::select(subject, trial, percent_missing)

## Merging with main df
pupil_extend <- pupil_extend |>
  dplyr::left_join(missing_pupil, by = c("subject", "trial"))

## Finding out how many trials are removed due to blinks (6 trials)
missing <- pupil_extend |>
  dplyr::filter(percent_missing >= 50) |>
  dplyr::select(subject, trial, percent_missing) |>
  dplyr::distinct()

## Filtering out trials with greater than 50% of missing data
pupil_extend <- pupil_extend |>
  dplyr::mutate(percent_missing = ifelse(is.na(percent_missing), 0, percent_missing)) |>
  dplyr::filter(percent_missing < 50)

## Linear interpolation
interp <- pupil_extend |>
  dplyr::group_by(subject, trial) |>
  dplyr::mutate(interp = na.approx(extended, maxgap = 1000, na.rm = F),
                final_pupil = na.locf(na.locf(interp, na.rm = FALSE), fromLast = TRUE))

# Baseline Pupil Correction

baseline_pupil <- baseline_correction_pupil(interp, pupil_colname = "final_pupil",
                                            baseline_window = c(-500, 0))

rm(butter, interp, missing, missing_pupil, pupil_blinks, pupil_extend, pupil_smoothed, trimmed_pupil_data,
   cutoff, filter_order, nyquist, samp_rate, w_c)

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


## Odd Pupil Slope Detection


## Creating Flag Variables and merging with original df


### Baseline Deviation


### Trial by Trial Baseline Deviation


### Steep Pupil Slope



### Filtering out those responses 



# Downsampling using fraction resampling

downsampled <- baseline_pupil |>
  group_by(subject, trial) |>
  dplyr::reframe(
    pupil = gsignal::resample(baselinecorrectedp, 1, 2),
    time_ms = -3000 + (seq_along(pupil) - 1) * 2
  ) |>
  ungroup()

filtered_df <- baseline_pupil |>
  dplyr::select(subject, trial, code, speaker, targetphrase, counterbalance) |>
  dplyr::distinct() 

downsampled <- filtered_df |>
  dplyr::full_join(downsampled, by = c("subject", "trial"))

# Dynamic Time Warping

control_templates <- downsampled |>
  dplyr::filter(speaker == "Control") |>
  group_by(targetphrase, time_ms) |>
  summarize(mean_pupil = mean(pupil), .groups = "drop") |>
  nest(template_data = c(time_ms, mean_pupil))

dtw_speakers <- function(als_time, als_pupil, current_phrase, templates) {
  
  template_df <- templates |>
    dplyr::filter(targetphrase == current_phrase) |>
    unnest(template_data)
  
  ref_pupil <- template_df$mean_pupil
  ref_time <- template_df$time_ms
  
  if(length(als_pupil) < 2 || length(ref_pupil) < 2) {
    return(tibble(time_ms = NA_real_, warped_pupil = NA_real_))
  }
  
  alignment <- dtw(x = als_pupil,
                   y = ref_pupil,
                   keep.internals = TRUE,
                   step.pattern = symmetric2)
  
  warped_indicies <- warp(alignment, index.reference = FALSE)
  
  warped_pupil_values <- als_pupil[warped_indicies]
  
  return(tibble(
    time_ms = ref_time,
    warped_pupil = warped_pupil_values
  ))
  
}

als_nested <- downsampled |>
  dplyr::filter(speaker == "ALS") |>
  dplyr::group_by(subject, trial, targetphrase) |>
  nest(trial_data = c(time_ms, pupil))

als_nested_1 <- als_nested |>
  dplyr::filter(subject == "LE10") |>
  dplyr::filter(targetphrase == "account for who could knock")

control_templates_1 <- control_templates |>
  dplyr::filter(targetphrase == "account for who could knock")

als_wraped <- als_nested_1 |>
  dplyr::mutate(
    warped = map2(
      trial_data, targetphrase,
      ~ dtw_speakers(
        als_time = .x$time_ms,
        als_pupil = .x$pupil,
        current_phrase = .y,
        templates = control_templates_1
      )
    )
  ) |>
  select(-trial_data) |>
  unnest(warped) |>
  ungroup()

als_wraped |>
  ggplot() +
  aes(x = time_ms,
      y = warped_pupil) +
  geom_line()

trial  <- als_nested_1 |>
  unnest()

trial |>
  ggplot() +
  aes(x = time_ms,
      y = pupil) +
  geom_line()

template <- control_templates_1 |>
  unnest()

template |>
  ggplot() +
  aes(x = time_ms,
      y = mean_pupil) +
  geom_line()

normed_data <- normed_data |>
  dplyr::select(-time_n) |>
  dplyr::mutate(speaker = factor(speaker, levels = c("Control", "ALS")))
                
data.binned <- data.binned |>
  dplyr::mutate(speaker = factor(speaker, levels = c("Control", "ALS")))

ple_data <- ple_data |>
  dplyr::mutate(speaker = factor(speaker, levels = c("Control", "ALS")))

# Export data

## Set working directory
setwd("C:\\Users\\mehirsch\\Documents\\GitHub\\Listening-Effort-in-Dysarthria\\Manuscript Analysis\\Cleaned Data")

## Export Pupil Dilation DF
rio::export(data.binned, "cleaned_pupil_data.csv")
rio::export(normed_data, "cleaned_pupil_data_normalized.csv")

## Export PLE Ratings
rio::export(ple_data, "cleaned_ple_data.csv")

# Creating Data Dictionaries for Pupil Dilation and PLE datasets

library(datadictionary)

labels_normed <- c(subject = "Participant ID",
                   trial = "Trial Number",
                   speaker = "Speaker (Control or ALS)",
                   code = "Stimulus Code",
                   targetphrase = "Target Phrase",
                   counterbalance = "Counterbalanced Condition",
                   normed_pupil = "Processed Pupil Dilation (Arbitrary Units)",
                   time_norm = "Normalized Trial Time (ms)")

labels_data <- c(subject = "Participant ID",
                 trial = "Trial Number",
                 speaker = "Speaker (Control or ALS)",
                 time_norm = "Trial Time (ms)",
                 code = "Stimulus Code",
                 targetphrase = "Target Phrase",
                 counterbalance = "Counterbalanced Condition",
                 pupil.binned = "Processed Pupil Dilation (Arbitrary Units)")

labels_ple <- c(subject = "Participant ID",
                trial = "Trial Number",
                code = "Stimulus Code",
                speaker = "Speaker (Control or ALS)",
                targetphrase = "Target Phrase",
                counterbalance = "Counterbalanced Condition",
                effort_rating = "Perceived Listening Effort Rating")

data_dict <- create_dictionary(data.binned, var_labels = labels_data)

data_dict_normed <- create_dictionary(normed_data, var_labels = labels_normed)

data_dict_ple <- create_dictionary(ple_data, var_labels = labels_ple)

rio::export(data_dict, "cleaned_pupil_data_dictionary.csv")

rio::export(data_dict_normed, "cleaned_pupil_data_normalized_dictionary.csv")

rio::export(data_dict_ple, "cleaned_ple_data_dictionary.csv")
