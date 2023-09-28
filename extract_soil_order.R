## ------------------------------------------ ##
#     SPARC Soil P -- Extract Soil Order
## ------------------------------------------ ##

# Life stage: PRELIMINARY 

# Script author(s): Angel Chen

# Purpose:
## Extract soil order data
## Creates a tidy csv file containing soil order data for lon/lat coordinates

## ------------------------------------------ ##
#             Housekeeping -----
## ------------------------------------------ ##
# Load necessary libraries
# install.packages("librarian")
librarian::shelf(tidyverse, scicomptools, sf, terra)

# Create necessary sub-folder(s)
dir.create(path = file.path("raw_data"), showWarnings = F)

# Identify raw data files
raw_soil_order_ids <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1q9WhGM0ZFQfpJczIc3fym85H--ayPfeb")) %>%
  dplyr::filter(name %in% c("TAXOUSDA_250m_suborder_classes.tif",
                            "TAXOUSDA_250m_suborder_classes_legend.csv"))

# Identify raw data files
# CHANGE INPUT SOURCE AS SOON AS OVERALL WORKFLOW IS FINALIZED
raw_latlon_ids <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1pjgN-wRlec65NDLBvryibifyx6k9Iqy9")) %>%
  dplyr::filter(name %in% c("sparc-soil-p_stats-ready_mineral_0-10.csv"))

# Combine file IDs
raw_ids <- rbind(raw_soil_order_ids, raw_latlon_ids)

# For each raw data file
for(k in 1:nrow(raw_ids)){
  
  # Download file (but silence how chatty this function is)
  googledrive::with_drive_quiet(
    googledrive::drive_download(file = raw_ids[k, ]$id, overwrite = T,
                                path = file.path("raw_data", raw_ids[k, ]$name)) )
  
  # Print success message
  message("Downloaded file ", k, " of ", nrow(raw_ids)) }

# Clear environment
rm(list = ls())

## ------------------------------------------------------- ##
#             Site Locations - Assemble ----
## ------------------------------------------------------- ##

# Read in csv with lat/lon coordinates
locations <- read_csv(file.path("raw_data","sparc-soil-p_stats-ready_mineral_0-10.csv")) %>%
  dplyr::select(lter, dataset_simp, dataset, raw_filename, site, plot, block, core, longitude, latitude) 

# Convert the dataframe to a terra SpatVector object
locations_spatvector <- terra::vect(locations, geom=c("longitude", "latitude"), crs="+proj=longlat +datum=WGS84", keepgeom=T)

# Another way to set the CRS
# locations_spatvector <- terra::vect(locations, geom=c("x", "y"), crs="EPSG:4326", keepgeom=FALSE)

## ------------------------------------------------------- ##
#           Soil Order - Extract ----
## ------------------------------------------------------- ##
# Pull in the raw soil order data
soil_raw <- terra::rast(x = file.path("raw_data", "TAXOUSDA_250m_suborder_classes.tif"))

# Check CRS
sf::st_crs(soil_raw)

# Experimental plotting over North America
frame_rast <- terra::rast(terra::ext(-150, -66, 10, 80))
plot(frame_rast, axes = F, reset = F)
terra::plot(soil_raw, add = T, axes = F)
terra::plot(locations_spatvector, add = T, axes = F)

# Extract lithology data
# Each code number corresponds to a rock type, see below for a list of what each code number represents
soil_out <- terra::extract(soil_raw, locations_spatvector, bind = T) %>%
  as.data.frame() %>%
  dplyr::rename(soil_code = TAXOUSDA_250m_suborder_classes)

# Check it out
dplyr::glimpse(soil_out)

## ------------------------------------------------------- ##
#           Soil Order - Index Prep ----
## ------------------------------------------------------- ##
# Read in soil order index
soil_index_raw <- read.csv(file = file.path("raw_data", "TAXOUSDA_250m_suborder_classes_legend.csv"))

# Glimpse it
dplyr::glimpse(soil_index_raw)

# See if there are any differences between "Group" and "Generic"
unique(soil_index_raw$Group)
unique(soil_index_raw$Generic)

# Simplify this object to just what we need
soil_index <- soil_index_raw %>%
  # Coerce soil class columns to lowercase
  dplyr::mutate(specific_soil = tolower(x = Group),
                generic_soil = tolower(x = Generic)) %>%
  # Pare down to only desired columns
  ## Also rename integer code column to match how it is called in the extracted dataframe
  dplyr::select(soil_code = Number, specific_soil, generic_soil) %>%
  # Drop the group column (pending group input to the contrary)
  dplyr::select(-specific_soil)

# Glimpse this as well
dplyr::glimpse(soil_index)


## ------------------------------------------------------- ##
#                 Soil Order - Export ----
## ------------------------------------------------------- ##

# Get ready to export by joining the extracted data with the index
soil_export <- soil_out %>%
  dplyr::left_join(y = soil_index)

# Check it out
dplyr::glimpse(soil_export)

# Create folder to export to
dir.create(path = file.path("extracted_data"), showWarnings = F)

# Export the summarized soil order data
write.csv(x = soil_export, na = '', row.names = F,
          file = file.path("extracted_data", "soil-p-extract_soil-order.csv"))

# Upload to GoogleDrive
# CHANGE FINAL EXPORT DESTINATION LATER
googledrive::drive_upload(media = file.path("extracted_data", 
                                            "soil-p-extract_soil-order.csv"),
                          overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1VUIPgcB3VsF12YWpLobmddgOiwhzvmil"))

# End ----

