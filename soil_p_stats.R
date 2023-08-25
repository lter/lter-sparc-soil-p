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
librarian::shelf(tidyverse, googledrive, supportR)

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

# Depth subsetting is used to restrict core depth of samples used in analysis
## Decide how many centimeters from the first measured depth (of the A horizon) are allowed
depth_cutoff <- 15

# Megadata includes *a lot* of information and we only really need a subset of it for stats
stat_df <- mega %>%
  # Pare down to only columns of interest
  ## Unspecified columns are implicitly removed
  dplyr::select(lter, dataset, site, plot, block, core, dplyr::starts_with("treatment"),
                dplyr::starts_with("horizon_"), dplyr::starts_with("depth_"),
                core_length_cm, pH:soil_mass_g_m2,
                dplyr::starts_with("N_"), dplyr::starts_with("C_")) %>%
  # Only interested in mineral layer
  ## Assuming that un-specified horizons are mineral layer
  dplyr::filter(horizon_binary == "mineral" | nchar(horizon_binary) == 0) %>%
  # Identify minimum depth of remaining data within sample info column groups
  dplyr::group_by(dplyr::across(c(lter:treatment_years))) %>%
  dplyr::mutate(min_depth = ifelse(!all(is.na(depth_start_cm)),
                                   yes = min(depth_start_cm, na.rm = T),
                                   no = NA),
                max_allowed_depth = (min_depth + depth_cutoff),
                .after = horizon_binary) %>%
  dplyr::ungroup() %>%
  # Filter to only samples in that range
  dplyr::filter(depth_start_cm >= min_depth & depth_end_cm <= max_allowed_depth)

# Check out the structure of the data
dplyr::glimpse(stat_df)

# Check to make sure we're okay with the columns we dropped
supportR::diff_check(old = names(mega), new = names(stat_df), sort = F)


# End ----
