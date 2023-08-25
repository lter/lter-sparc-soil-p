## ------------------------------------------ ##
         # SPARC Soil P -- Statistics
## ------------------------------------------ ##
# Script author(s): Nick J Lyon

# Purpose:
## Perform needed pre-stats wrangling and statistical analysis
## Export / display summaries of tests

## ------------------------------------------ ##
              # Housekeeping -----
## ------------------------------------------ ##

# Load necessary libraries
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive)

# Create necessary sub-folder(s)
dir.create(path = file.path("tidy_data"), showWarnings = F)

# Identify and download the tidied megadata object from the Drive
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1pjgN-wRlec65NDLBvryibifyx6k9Iqy9")) %>%
  dplyr::filter(name == "tidy_soil_p.csv") %>%
  googledrive::drive_download(file = .$id, path = file.path("tidy_data", .$name), overwrite = T)

# Clear environment
rm(list = ls())

## ------------------------------------------ ##
          # Pre-Stats Wrangling ----
## ------------------------------------------ ##

# Read in full megadata
mega <- read.csv(file.path("tidy_data", "tidy_soil_p.csv"))

# Check structure
dplyr::glimpse(mega)

# Megadata includes *a lot* of information and we only really need a subset of it for stats




# End ----
