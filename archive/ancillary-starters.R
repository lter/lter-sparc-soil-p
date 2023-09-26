## ------------------------------------------ ##
  # SPARC Soil P -- Ancillary Data Templates
## ------------------------------------------ ##
# Script author(s): Nick J Lyon

# Purpose:
## Create starting templates for left-most columns of 'ancillary data' GoogleSheets

## ------------------------------------------ ##
                # Housekeeping -----
## ------------------------------------------ ##

# Load necessary libraries
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive)

# Create necessary sub-folder(s)
dir.create(path = file.path("tidy_data"), showWarnings = F)
dir.create(path = file.path("ancillary_templates"), showWarnings = F)

# Identify the needed data file(s) in the Drive
( file_ids <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1pjgN-wRlec65NDLBvryibifyx6k9Iqy9")) %>%
    dplyr::filter(name %in% c("stats-ready_tidy-soil-p.csv")) )

# Download those files
purrr::walk2(.x = file_ids$id, .y = file_ids$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T, 
                                                path = file.path("tidy_data", .y)))

# Clear environment
rm(list = ls())

# Read in stats/viz-ready file
main_df <- read.csv(file.path("tidy_data", "stats-ready_tidy-soil-p.csv"))

# Check structure
dplyr::glimpse(main_df)

## ------------------------------------------ ##

## ------------------------------------------ ##







# End ----
