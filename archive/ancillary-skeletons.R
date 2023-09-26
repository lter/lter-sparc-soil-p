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
dir.create(path = file.path("skeletons"), showWarnings = F)

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
        # Create Ancillary Templates ----
## ------------------------------------------ ##

# Want to make simple dataframes for each spatial organization level

# Dataset
anc_dataset <- main_df %>%
  dplyr::select(lter, dataset_simp, dataset) %>%
  dplyr::distinct()

# Site
anc_site <- main_df %>%
  dplyr::select(lter, dataset_simp, dataset, site) %>%
  dplyr::distinct()

# Block
anc_block <- main_df %>%
  dplyr::select(lter, dataset_simp, dataset, site, block) %>%
  dplyr::distinct()

# Plot
anc_plot <- main_df %>%
  dplyr::select(lter, dataset_simp, dataset, site, block, plot) %>%
  dplyr::distinct()

# Core
anc_core <- main_df %>%
  dplyr::select(lter, dataset_simp, dataset, site, block, plot, core) %>%
  dplyr::distinct()

## ------------------------------------------ ##
          # Export Skeletons Locally ----
## ------------------------------------------ ##

# Write each out as a CSV
## Dataset
write.csv(x = anc_dataset, row.names = F, na = '',
          file = file.path("skeletons", "ancillary-skeleton_dataset.csv"))
## Site
write.csv(x = anc_site, row.names = F, na = '',
          file = file.path("skeletons", "ancillary-skeleton_site.csv"))
## Block
write.csv(x = anc_block, row.names = F, na = '',
          file = file.path("skeletons", "ancillary-skeleton_block.csv"))
## Plot
write.csv(x = anc_plot, row.names = F, na = '',
          file = file.path("skeletons", "ancillary-skeleton_plot.csv"))
## Core
write.csv(x = anc_core, row.names = F, na = '',
          file = file.path("skeletons", "ancillary-skeleton_core.csv"))

## ------------------------------------------ ##
              # Export to Drive ----
## ------------------------------------------ ##

# Identify relevant folder
skeleton_drive <- googledrive::as_id("https://drive.google.com/drive/u/0/folders/1vAjPynDwWjtto4WjLxW-S8rpzyl-XCCq")

# Identify files
( bones <- dir(path = file.path("skeletons")) )

# Loop across to the Drive
for(file in bones){
  
  # And upload to relevant folder
  googledrive::drive_upload(media = file.path("skeletons", file),
                            path = skeleton_drive, overwrite = T) }

# End ----
