# Randomizing Counterbalance List Assignment

## Author: Micah E. Hirsch, M.S.
## Purpose: To randomly assign future participants to either counterbalance list 1 or 2.


# Loading Needed Packages

library(rio)
library(tidyverse)

# Set Working Directory

setwd("Stimuli List and Data Source/Counterbalance List Assignment")

# Setting a Seed 

set.seed(1000)

# Create data frame

list_assignment <- data.frame(id = paste0("LE", formatC(1:50, width = 2, flag = "0")),
                              counterbalance = sample(c(rep(1, 25), rep(2, 25)), replace = T))

# Exporting the df

rio::export(list_assignment, "Counterbalance List Assignment.csv")

