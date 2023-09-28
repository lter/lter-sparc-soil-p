## ------------------------------------------ ##
# SPARC Soil P -- Extract Lithology & Soil Order
## ------------------------------------------ ##

# Life stage: PRELIMINARY 

# Script author(s): Angel Chen

# Purpose:
## Extract lithology data
## Creates a tidy csv file containing full + ancillary + lithology + soil order data for lon/lat coordinates

## ------------------------------------------ ##
#             Housekeeping -----
## ------------------------------------------ ##
# Load necessary libraries
# install.packages("librarian")
librarian::shelf(tidyverse, scicomptools, sf, terra)

# Create necessary sub-folder(s)
dir.create(path = file.path("raw_data"), showWarnings = F)

# Identify raw data files
raw_lithology_ids <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1gxhT6OCOGlIsLZ-DT3Zzvuj6-EnNgXQh")) %>%
  dplyr::filter(name %in% c("glim_wgs84_0point5deg.txt.asc",
                            "Classnames.txt"))

# Identify raw data files
raw_soil_order_ids <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1q9WhGM0ZFQfpJczIc3fym85H--ayPfeb")) %>%
  dplyr::filter(name %in% c("TAXOUSDA_250m_suborder_classes.tif",
                            "TAXOUSDA_250m_suborder_classes_legend.csv"))
# Identify raw data files
# CHANGE INPUT SOURCE AS SOON AS OVERALL WORKFLOW IS FINALIZED
raw_latlon_ids <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1pjgN-wRlec65NDLBvryibifyx6k9Iqy9")) %>%
  dplyr::filter(name %in% c("sparc-soil-p_full-data-incl-ancillary.csv"))

# Combine file IDs
raw_ids <- rbind(raw_lithology_ids, raw_soil_order_ids, raw_latlon_ids)

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
locations <- read_csv(file.path("raw_data","sparc-soil-p_full-data-incl-ancillary.csv")) 

# Convert the dataframe to a terra SpatVector object
locations_spatvector <- terra::vect(locations, geom=c("longitude", "latitude"), crs="+proj=longlat +datum=WGS84", keepgeom=T)

# Another way to set the CRS
# locations_spatvector <- terra::vect(locations, geom=c("x", "y"), crs="EPSG:4326", keepgeom=FALSE)

## ------------------------------------------------------- ##
#                Lithology - Extract ----
## ------------------------------------------------------- ##

# Pull in the raw lithology data
rocks_raw <- terra::rast(x = file.path("raw_data", "glim_wgs84_0point5deg.txt.asc"))

# Check CRS
sf::st_crs(rocks_raw)

# Experimental plotting over North America
frame_rast <- terra::rast(terra::ext(-150, -66, 10, 80))
plot(frame_rast, axes = F, reset = F)
terra::plot(rocks_raw, add = T, axes = F)
terra::plot(locations_spatvector, add = T, axes = F)

# Extract lithology data
# Each code number corresponds to a rock type, see below for a list of what each code number represents
rocks_out <- terra::extract(rocks_raw, locations_spatvector, bind = T) %>%
  as.data.frame() %>%
  dplyr::rename(rock_code = glim_wgs84_0point5deg.txt)

# Check it out
dplyr::glimpse(rocks_out)

## ------------------------------------------------------- ##
#              Lithology - Index Prep ----
## ------------------------------------------------------- ##

# Bring in the index tying rock code integers with rock abbreviations
rock_index_raw <- read.table(file = file.path("raw_data", "Classnames.txt"),
                             header = T, sep = ';')

# Fix this index to make it more usable
rock_index <- rock_index_raw %>%
  # Rename the most important columns
  dplyr::rename(rock_code = OBJECTID,
                rock_abbrev = xx) %>%
  # And get a more descriptive version of each of the rock types
  dplyr::mutate(
    rock_type = dplyr::case_when(
      # Abbreviations found here:
      # https://www.clisap.de/fileadmin/B-Research/IA/IA5/LITHOMAP/
      rock_abbrev == 'su' ~ 'unconsolidated_sediments',
      rock_abbrev == 'ss' ~ 'siliciclastic_sedimentary_rocks',
      rock_abbrev == 'sm' ~ 'mixed_sedimentary_rocks',
      rock_abbrev == 'py' ~ 'pyroclastic',
      rock_abbrev == 'sc' ~ 'carbonate_sedimentary_rocks',
      rock_abbrev == 'ev' ~ 'evaporites',
      rock_abbrev == 'mt' ~ 'metamorphic_rocks',
      rock_abbrev == 'pa' ~ 'acid_plutonic_rocks',
      rock_abbrev == 'pi' ~ 'intermediate_plutonic_rocks',
      rock_abbrev == 'pb' ~ 'basic_plutonic_rocks',
      rock_abbrev == 'va' ~ 'acid_volcanic_rocks',
      rock_abbrev == 'vi' ~ 'intermediate_volcanic_rocks',
      rock_abbrev == 'vb' ~ 'basic_volcanic_rocks',
      rock_abbrev == 'ig' ~ 'ice_and_glaciers',
      rock_abbrev == 'wb' ~ 'water_bodies',
      rock_abbrev == 'nd' ~ 'no_data',
      TRUE ~ as.character(rock_abbrev) ) ) %>%
  # Remove unneeded columns
  dplyr::select(rock_code, rock_type) %>%
  # Create new column for rock group
  dplyr::mutate(
    rock_group = dplyr::case_when(
      rock_type == 'unconsolidated_sediments' ~ 'Check on this',
      rock_type == 'siliciclastic_sedimentary_rocks' ~ 'Acidic',
      rock_type == 'mixed_sedimentary_rocks' ~ 'Check on this',
      rock_type == 'pyroclastic' ~ 'Check on this',
      rock_type == 'carbonate_sedimentary_rocks' ~ 'Carbonate',
      rock_type == 'evaporites' ~ 'Check on this',
      rock_type == 'metamorphic_rocks' ~ 'Check on this',
      rock_type == 'acid_plutonic_rocks' ~ 'Acidic',
      rock_type == 'intermediate_plutonic_rocks' ~ 'Intermediate',
      rock_type == 'basic_plutonic_rocks' ~ 'Basic',
      rock_type == 'acid_volcanic_rocks' ~ 'Acidic',
      rock_type == 'intermediate_volcanic_rocks' ~ 'Intermediate',
      rock_type == 'basic_volcanic_rocks' ~ 'Basic',
      rock_type == 'ice_and_glaciers' ~ 'Check on this',
      rock_type == 'water_bodies' ~ 'Check on this',
      rock_type == 'no_data' ~ 'Check on this',
      TRUE ~ as.character(rock_type)))

# Check that worked
dplyr::glimpse(rock_index)

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
#            Lithology & Soil Order - Export ----
## ------------------------------------------------------- ##

# Get ready to export by joining the extracted data with the index
rocks_export <- rocks_out %>%
  dplyr::left_join(y = rock_index)

# Check it out
dplyr::glimpse(rocks_export)

# Get ready to export by joining the extracted data with the index
soil_export <- soil_out %>%
  dplyr::left_join(y = soil_index)

# Check it out
dplyr::glimpse(soil_export)

# Combine both lithology and soil order
spatial_export <- rocks_export %>%
  dplyr::left_join(soil_export) %>%
  dplyr::relocate(rock_code, .after = core) %>%
  dplyr::relocate(rock_type, .after = rock_code) %>%
  dplyr::relocate(rock_group, .after = rock_type) %>%
  dplyr::relocate(soil_code, .after = rock_group) %>%
  dplyr::relocate(generic_soil, .after = soil_code)

# Check it out
dplyr::glimpse(spatial_export)

# Create folder to export to
dir.create(path = file.path("extracted_data"), showWarnings = F)

# Export the summarized lithology data
write.csv(x = spatial_export, na = '', row.names = F,
          file = file.path("extracted_data", "sparc-soil-p_full-plus-ancil-and-spatial.csv"))

# Upload to GoogleDrive
# CHANGE FINAL EXPORT DESTINATION LATER
googledrive::drive_upload(media = file.path("extracted_data", 
                                            "sparc-soil-p_full-plus-ancil-and-spatial.csv"),
                          overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1pjgN-wRlec65NDLBvryibifyx6k9Iqy9"))

# End ----


