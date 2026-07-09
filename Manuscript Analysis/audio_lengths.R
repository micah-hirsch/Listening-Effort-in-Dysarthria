# Extracting Audio Lengths

# Micah Hirsch, Ph.D. mehirsch@bu.edu

# Purpose: To get the length of audio for pupil dilation preparation

library(tidyverse)
library(rPraat)
library(rio)

audio_dir <- "D:\\Listening Effort Study\\Audio Files"

setwd(audio_dir)

wav_files <- list.files(audio_dir, pattern = "\\.wav$")

audio_durations <- tibble(file_name = wav_files) |>
  mutate(
    duration_ms = map_dbl(file_name, \(f) snd.read(f)$duration * 1000),
    speaker = ifelse(grepl("ALS", file_name, ignore.case = T), "ALS", "Control"),
    code = str_extract(file_name, "EXP_\\d+")
  )

setwd("C:\\Users\\mehirsch\\Documents\\GitHub\\Listening-Effort-in-Dysarthria")

rio::export(audio_durations, "Manuscript Analysis\\Cleaned Data\\audio_lengths.csv")
