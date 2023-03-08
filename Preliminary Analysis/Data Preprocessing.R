# Listening Effort Preliminary Analysis: Data Preprocessing 

# Author: Micah E. Hirsch, M.S., mhirsch@fsu.edu

# Purpose: To load in raw data files and process it to prepare for analysis 
# Raw data files were first exported using the Data Viewer software
# provided by EyeLink. Deblinking, smoothing, downsampling, and merging pupil
# data with behavioral data will be done in this script.

# Load Packages

library(gazer) #remotes::install_github("dmirman/gazer")
library(tidyverse) #install.packages("tidyverse")
library(zoo) # install.packages("zoo")
library(knitr) 
library(devtools) #install.packages("devtools")
library(edfR) #install_github("jashubbard/edfR")

# Set working directories

edf.path <- "iCloud Drive/Documents/Listening-Effort-in-Dysarthria/Preliminary Analysis/Raw_Data"
csv.path <- "iCloud Drive/Documents/Listening-Effort-in-Dysarthria/Preliminary Analysis/Prepped_Data"

# Loading and Reading the edf files from Pilot Collection

##Loads and parses each edf file
file_list_edf <- list.files(path = edf.path, pattern = ".edf")
parse_edf(file_list = file_list_edf, output_dir = csv.path, type = "pupil")

## saving csv file list and merging all sample files 
file_list_csv <- list.files(path = csv.path, pattern = ".csv")
sample_gaze_data <- merge_gazer_files(file_list_csv, type = "edf")
