## ------------------------------------------ ##
# SPARC Soil P -- Join Ancillary Data
## ------------------------------------------ ##
# Script author(s): Nick J Lyon

# Purpose:


# Pre-Requisites:


## ------------------------------------------ ##
              # Housekeeping -----
## ------------------------------------------ ##

# Load necessary libraries
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive, magrittr)

# Create necessary sub-folder(s)
dir.create(path = file.path("data", "tidy_data"), showWarnings = F)
dir.create(path = file.path("data", "ancillary_data"), showWarnings = F)

# Clear environment
rm(list = ls())

# Identify needed tidy file(s)
tidy_drive <- googledrive::as_id("https://drive.google.com/drive/u/0/folders/1pjgN-wRlec65NDLBvryibifyx6k9Iqy9")

# Identify the archival data in that folder and download it
googledrive::drive_ls(path = tidy_drive) %>%
  dplyr::filter(name == "sparc-soil-p_full-data-p-sums.csv") %>%
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "tidy_data", .$name))

# Read that file in
full_v1 <- read.csv(file = file.path("data", "tidy_data", 
                                     "sparc-soil-p_full-data-p-sums.csv"))

# Glimpse it!
dplyr::glimpse(full_v1)

## ------------------------------------------ ##
          # Acquire Ancillary Data ----
## ------------------------------------------ ##

# Identify ancillary data file names
anc_names <- c(paste0("Ancillary_", c("dataset", "site", "block", "plot", "core")))

# Identify the desired ancillary data files
anc_files <- googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1TwN8AwUKc3iLBsTRRzm68owNlUOgkQeI")) %>%
  dplyr::filter(name %in% anc_names)

# Did that get all five?
anc_files

# Download files into that
purrr::walk2(.x = anc_files$id, .y = anc_files$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("data", 
                                                                 "ancillary_data", .y)))

## ------------------------------------------ ##
            # Attach Ancillary Data ----
## ------------------------------------------ ##

# Make a new version of our primary data to avoid bad errors
full_v2 <- full_v1

# Identify the available ancillary data granularity levels
gran_levels <- c("dataset", "site", "block", "plot", "core")

# Loop across desired/available ancillary data to integrate with data
for(granularity in gran_levels){
  
  # Starting message
  message("Integrating ", granularity, "-level ancillary data")
  
  # Identify the file
  gran_path <- file.path("data", "ancillary_data", paste0("Ancillary_", granularity, ".xlsx"))
  
  # Read it in
  gran_df <- readxl::read_xlsx(path = gran_path)
  
  # Identify spatial organization columns (in this level of the ancillary data)
  spatial_cols <- intersect(x = c("lter", "dataset_simp", gran_levels), y = names(gran_df))
  
  # Make the non-granularity columns specific to the level from which they were entered
  ## Note: manual assignment of column names is *risky*! DO NOT ATTEMPT ELSEWHERE!!
  names(gran_df) <- c(spatial_cols, paste0(granularity, "_", 
                                           setdiff(x = names(gran_df), y = spatial_cols)))
  
  # Now join onto the larger SPARC data
  full_v2 %<>%
    dplyr::left_join(y = gran_df, by = spatial_cols) }

# Check what that leaves us with
dplyr::glimpse(full_v2)

# Drop the data object left over from the loop
rm(list = "gran_df")

## ------------------------------------------ ##
        # Streamline Ancillary Data ----
## ------------------------------------------ ##

# Now need to collapse 'duplicate' columns from ancillary data into single 'actual' value
## Will do one pipe/object per set of related columns

# Fix latitude/longitude columns
full_v3a <- full_v2 %>%
  ## Collapse columns together into a single 'actual' one
  dplyr::mutate(lat_actual = dplyr::coalesce(lat, as.numeric(core_latitude), 
                                             as.numeric(plot_latitude), 
                                             as.numeric(block_latitude),
                                             as.numeric(site_latitude),
                                             as.numeric(dataset_latitude)),
                lon_actual = dplyr::coalesce(lon, 
                                             as.numeric(core_longitude), 
                                             as.numeric(plot_longitude), 
                                             as.numeric(block_longitude), 
                                             as.numeric(site_longitude),
                                             as.numeric(dataset_longitude)),
                .after = raw_filename) %>%
  # Throw away (sorry) all component columns now that we have 'actual'
  dplyr::select(-lat, -lon, -dplyr::ends_with("_latitude"), 
                -dplyr::ends_with("_longitude"), -dplyr::ends_with("_coordinate_source")) %>%
  # Rename actual
  dplyr::rename(latitude = lat_actual,
                longitude = lon_actual)

# Glimpse it
dplyr::glimpse(full_v3a)

# Streamline precipitation
full_v3b <- full_v3a %>%
  # Collapse into one column
  dplyr::mutate(precip_actual = dplyr::coalesce(precipitation_mm, site_MAP_mm),
                .after = core) %>%
  # Drop old columns
  dplyr::select(-precipitation_mm, -site_MAP_mm) %>%
  # Rename remaining column
  dplyr::rename(mean.annual.precip_mm = precip_actual)

# Look at it
dplyr::glimpse(full_v3b)

# Do temperature next
full_v3c <- full_v3b %>%
  # Collapse
  dplyr::mutate(temp_actual = dplyr::coalesce(site_MAT_C),
                .after = mean.annual.precip_mm) %>%
  # Drop old
  dplyr::select(-site_MAT_C) %>%
  # Rename new
  dplyr::rename(mean.annual.temp_C = temp_actual)

# Look at it
dplyr::glimpse(full_v3c)

## ------------------------------------------ ##
     # Export Ancillary + P Sum Data ----
## ------------------------------------------ ##

# Create a final data object
sparc_tidy <- sparc_v8a %>%
  # Drop the one row with an unreasonably high 'total P' value
  dplyr::filter(is.na(total.P_conc_mg.kg) | total.P_conc_mg.kg <= 5250)

# Check its structure
dplyr::glimpse(sparc_tidy)

# Define the tidy file name
tidy_name <- "full-data_tidy-soil-p.csv"

# Save out the final data object
write.csv(x = sparc_tidy, file = file.path("tidy_data", tidy_name), 
          row.names = F, na = "")


# End ----
