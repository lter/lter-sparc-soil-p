## ------------------------------------------ ##
      # SPARC Soil P -- Data Wrangling
## ------------------------------------------ ##
# Script author(s): Nick J Lyon

# Purpose:
## Harmonize / wrangle soil phosphorus & nitrogen concentration data
## Ultimately creates a "megadata" file that can be used by downstream analysis/plotting

## ------------------------------------------ ##
              # Housekeeping -----
## ------------------------------------------ ##

# Load necessary libraries
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive, supportR)

# Create necessary sub-folder(s)
dir.create(path = file.path("raw_data"), showWarnings = F)

# Download data key (connects raw column names with synonymized equivalents)
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1WIAo08Jnmp7BdvN8xxobZ_txcFCWZ35w"), pattern = "LTER_P_DataKey.csv") %>%
  googledrive::drive_download(file = ., overwrite = T)

# Identify raw data files
raw_ids <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/10igyNjNSEJDrz5mUtYyxgbUPDUO7bsuW"), type = "csv")

# Download each raw data file
for(k in 1:nrow(raw_ids)){
  
  # Download file (but silence how chatty this function is)
  googledrive::with_drive_quiet(
    googledrive::drive_download(file = raw_ids[k, ]$id, overwrite = T,
                                path = file.path("raw_data", raw_ids[k, ]$name)) )
  
  # Print success message
  message("Downloaded file ", k, " of ", nrow(raw_ids)) }

# Clear environment
rm(list = ls())

## ------------------------------------------ ##
# Data Wrangling / Harmonizing ----
## ------------------------------------------ ##

# Identify the downloaded raw files
raw_files <- dir(path = file.path("raw_data"))

# Retrieve the data key
key_v0 <- read.csv(file = file.path("LTER_P_DataKey.csv"))

# Compare the two to see if all file names in the key were in the Drive
supportR::diff_check(old = raw_files, unique(key_v0$FilenameRaw))








# End ----
