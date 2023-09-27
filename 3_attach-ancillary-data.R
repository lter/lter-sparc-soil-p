

## ------------------------------------------ ##
# Acquire Ancillary Data ----
## ------------------------------------------ ##

# Identify the desired ancillary data files
anc_files <- googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1TwN8AwUKc3iLBsTRRzm68owNlUOgkQeI")) %>%
  dplyr::filter(name %in% c(paste0("Ancillary_", c("dataset", "site", "block", "plot", "core"))))

# Did that get all five?
anc_files

# Create a folder for local storage
dir.create(path = file.path("ancillary_data"), showWarnings = F)

# Download files into that
purrr::walk2(.x = anc_files$id, .y = anc_files$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("ancillary_data", .y)))



## ------------------------------------------ ##
# Attach Ancillary Data ----
## ------------------------------------------ ##

# Make a new version of our primary data to avoid bad errors
sparc_v7 <- sparc_v6

# Identify the available ancillary data granularity levels
gran_levels <- c("dataset", "site", "block", "plot", "core")

# Loop across desired/available ancillary data to integrate with data
for(granularity in gran_levels){
  
  # Starting message
  message("Integrating ", granularity, "-level ancillary data")
  
  # Identify the file
  gran_path <- file.path("ancillary_data", paste0("Ancillary_", granularity, ".xlsx"))
  
  # Read it in
  gran_df <- readxl::read_xlsx(path = gran_path)
  
  # Identify spatial organization columns (in this level of the ancillary data)
  spatial_cols <- intersect(x = c("lter", "dataset_simp", gran_levels), y = names(gran_df))
  
  # Make the non-granularity columns specific to the level from which they were entered
  ## Note: manual assignment of column names is *risky*! DO NOT ATTEMPT ELSEWHERE!!
  names(gran_df) <- c(spatial_cols, paste0(granularity, "_", 
                                           setdiff(x = names(gran_df), y = spatial_cols)))
  
  # Now join onto the larger SPARC data
  sparc_v7 %<>%
    dplyr::left_join(y = gran_df, by = spatial_cols) }

# Check what that leaves us with
dplyr::glimpse(sparc_v7)

## ------------------------------------------ ##
# Streamline Ancillary Data ----
## ------------------------------------------ ##

# Now need to collapse 'duplicate' columns from ancillary data into single 'actual' value
## Will do one pipe/object per set of related columns

# Fix latitude/longitude columns
sparc_v8a <- sparc_v7 %>%
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
                coord_source = dplyr::coalesce(core_coordinate_source, plot_coordinate_source, 
                                               block_coordinate_source, site_coordinate_source,
                                               dataset_coordinate_source),
                .after = raw_filename) %>%
  # Throw away (sorry) all component columns now that we have 'actual'
  dplyr::select(-lat, -lon, -dplyr::ends_with("_latitude"), 
                -dplyr::ends_with("_longitude"), -dplyr::ends_with("_coordinate_source")) %>%
  # Rename actual
  dplyr::rename(latitude = lat_actual,
                longitude = lon_actual)

# Glimpse it
dplyr::glimpse(sparc_v8a)

## ------------------------------------------ ##
# Export Full SPARC Data ----
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
