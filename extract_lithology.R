## ------------------------------------------ ##
#     SPARC Soil P -- Extract Lithology
## ------------------------------------------ ##

# Life stage: PRELIMINARY 

# Script author(s): Angel Chen

# Purpose:
## Extract lithology data
## Creates a tidy csv file containing lithology data for lon/lat coordinates

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
raw_ancillary_ids <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1TwN8AwUKc3iLBsTRRzm68owNlUOgkQeI")) %>%
  dplyr::filter(name %in% c("Ancillary_site"))

# Combine file IDs
raw_ids <- rbind(raw_lithology_ids, raw_ancillary_ids)

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
#                Lithology - Extract ----
## ------------------------------------------------------- ##

# Pull in the raw lithology data
rocks_raw <- terra::rast(x = file.path("raw_data", "glim_wgs84_0point5deg.txt.asc"))

# Check CRS
sf::st_crs(rocks_raw)

# Experimental plotting
terra::plot(rocks_raw)

# Create a dataframe of some example coordinates
locations <- data.frame(x = c(-71.889695,
                               -71.889102,
                               -71.889058,
                               -71.889397,
                               -71.889636,
                               -71.890073,
                               -71.889466,
                               -71.890195,
                               -71.889306,
                               -71.890063),
                           y = c(44.036890,
                                44.036854,
                                44.037221,
                                44.037569,
                                44.037210,
                                44.036018,
                                44.035582,
                                44.035510,
                                44.036017,
                                44.034650))

# Convert the dataframe to a terra SpatVector object
locations_spatvector <- terra::vect(locations, geom=c("x", "y"), crs="+proj=longlat +datum=WGS84", keepgeom=FALSE)

# Another way to set the CRS
# locations_spatvector <- terra::vect(locations, geom=c("x", "y"), crs="EPSG:4326", keepgeom=FALSE)

# Extract lithology data
# Each code number corresponds to a rock type, see below for a list of what each code number represents
terra::extract(rocks_raw, locations_spatvector)






## ------------------------------------------------------- ##
#              Lithology - Index Prep ----
## ------------------------------------------------------- ##

# Bring in the index tying rock code integers with rock abbreviations
# NOTE: pulling source data from another working group's folder
# Need to create own folder for SPARC Soil P later and populate it with the source data
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
  dplyr::select(value = rock_code, rock_type)

# Check that worked
dplyr::glimpse(rock_index)


