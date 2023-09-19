## ------------------------------------------ ##
       # SPARC Soil P -- Data Wrangling
## ------------------------------------------ ##
# Script author(s): Nick J Lyon

# Purpose:
## Process 'archival' data for the purposes of the 'Soil P' LTER SPARC group

# Pre-Requisites:
## Assumes that "soil_p_harmonize.R" script has been run relatively recently
## Otherwise risks using a superseded version of the tidy data
### (that could lack updated raw data/data key information)

## ------------------------------------------ ##
              # Housekeeping -----
## ------------------------------------------ ##

# Load necessary libraries
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive, supportR)

# Create necessary sub-folder(s)
dir.create(path = file.path("tidy_data"), showWarnings = F)

# Clear environment
rm(list = ls())

# Identify raw data files
tidy_drive <- googledrive::as_id("https://drive.google.com/drive/u/0/folders/1pjgN-wRlec65NDLBvryibifyx6k9Iqy9")

# Identify the archival data in that folder and download it
googledrive::drive_ls(path = tidy_drive) %>%
  dplyr::filter(name == "sparc-soil-p_archival-data.csv") %>%
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("tidy_data", .$name))

# Read that file in
sparc_v1 <- read.csv(file = file.path("tidy_data", "sparc-soil-p_archival-data.csv"))

# Glimpse it!
dplyr::glimpse(sparc_v1)

## ------------------------------------------ ##
              # Streamline Data ----
## ------------------------------------------ ##

# The archival data is ideal for general purpose
# However, it contains details we neither want nor need for SPARC purposes
sparc_v2 <- sparc_v1 %>%
  # Drop unwanted columns
  dplyr::select(-molarity, -time, -temp) %>%
  # Fill remaining P information columns with placeholders where needed
  dplyr::mutate(
    measurement = ifelse((is.na(measurement) | nchar(measurement) == 0), 
                         yes = "data.type", no = measurement),
    units = ifelse(is.na(units) | nchar(units) == 0, 
                   yes = "units", no = units),
    order = ifelse(is.na(order) | nchar(order) == 0, 
                   yes = "order", no = order),
    reagent = ifelse(is.na(reagent) | nchar(reagent) == 0, 
                     yes = "reagent", no = reagent)
  ) %>%
  # Recombine them into a single column
  dplyr::mutate(P_fractions = paste(p_type, measurement, units, order, reagent, sep = "_")) %>%
  # Drop the separate pieces of information
  dplyr::select(-p_type, -measurement, -units, -order, -reagent) %>%
  # Reclaim wide format!
  tidyr::pivot_wider(names_from = P_fractions, 
                     values_from = value,
                     values_fill = NA)

# Glimpse data structure
dplyr::glimpse(sparc_v2)

## ------------------------------------------ ##
            # Phosphorus Sums ----
## ------------------------------------------ ##

# Glimpse the entire dataset
dplyr::glimpse(sparc_v2)

# Now we'll want to add together our various types of P (conditionally)
p_sums <- sparc_v2 %>%
  # First need to fill NAs with 0s to avoid making NA sums
  ## Pivot longer
  tidyr::pivot_longer(cols = c(dplyr::starts_with("P_"),
                               dplyr::starts_with("Po_"),
                               dplyr::starts_with("Pi_")),
                      names_to = "names", values_to = "values") %>%
  ## Remove NA / missing values
  dplyr::filter(!is.na(values) & nchar(values) != 0) %>%
  ## Pivot back to wide format and fill empty cells with 0
  tidyr::pivot_wider(names_from = names, values_from = values, values_fill = 0) %>%
  # Placeholder slow/total P
  dplyr::mutate(slow.P_conc_mg.kg = 1:nrow(.),
                total.P_conc_mg.kg = 1:nrow(.)) %>%
  # # Calculate slow P conditionally
  # dplyr::mutate(slow.P_conc_mg.kg = dplyr::case_when(
  #   T ~ NA)) %>%
  # # Do the same for total P
  # dplyr::mutate(total.P_conc_mg.kg = dplyr::case_when(
  #   T ~ NA)) %>%
  # And for any other P fraction sum groups
  # dplyr::mutate(___.P_conc_mg.kg = dplyr::case_when(
  #   T ~ NA)) %>%
  # After summing, remove all P fraction columns (because we changed real NAs to convenient 0s)
  dplyr::select(-dplyr::starts_with("P_"), -dplyr::starts_with("Po_"), 
                -dplyr::starts_with("Pi_")) %>%
  # Keep only unique rows
  dplyr::distinct()

# Any datasets missing?
p_sums %>%
  dplyr::filter(is.na(slow.P_conc_mg.kg) | is.na(total.P_conc_mg.kg)) %>%
  dplyr::select(dataset, slow.P_conc_mg.kg, total.P_conc_mg.kg) %>%
  dplyr::distinct()

# Check structure
dplyr::glimpse(p_sums)

# Note we're doing this in a separate object because we coerced NAs into 0s for algebra reasons
## They're not "real" 0s so we want to preserve the real 0s while still easily getting sums

# Now we can attach our sums to the original tidy object
sparc_v3 <- sparc_v2 %>%
  # By not specifying which columns to join by, all shared columns will be used
  dplyr::left_join(y = p_sums) %>%
  # Move our P sums to the left for more easy reference
  dplyr::relocate(dplyr::ends_with(".P_conc_mg.kg"),
                  .after = bulk.density_kg.ha)

# Check structure
dplyr::glimpse(sparc_v3)

## ------------------------------------------ ##
          # Stock P Calculations ----
## ------------------------------------------ ##

# Calculate absolute P totals (rather than portions of each core)
sparc_v4 <- sparc_v3 %>%
  # Multiply P concentration by core length & bulk density to get stocks
  dplyr::mutate(slow.P_temp = slow.P_conc_mg.kg * core.length_cm * bulk.density_g.cm3,
                total.P_temp = total.P_conc_mg.kg * core.length_cm * bulk.density_g.cm3) %>%
  # Do unit conversions to get to g/m2
  dplyr::mutate(slow.P_stock_g.m2 = (slow.P_temp * 10^4) / 10^6,
                .before = slow.P_conc_mg.kg) %>%
  dplyr::mutate(total.P_stock_g.m2 = (total.P_temp * 10^4) / 10^6,
                .before = total.P_conc_mg.kg) %>%
  # Drop intermediary columns
  dplyr::select(-slow.P_temp, -total.P_temp)

# Check contents of those columns
summary(sparc_v4$slow.P_stock_g.m2)
summary(sparc_v4$total.P_stock_g.m2)

# Re-check structure
dplyr::glimpse(sparc_v4[1:35])

## ------------------------------------------ ##
        # Export SPARC-Ready Data ----
## ------------------------------------------ ##

# Create a final data object
full_sparc <- sparc_v4

# Check its structure
dplyr::glimpse(full_sparc)

# Define the tidy file name
tidy_name <- "full-data_tidy-soil-p.csv"

# Save out the final data object
write.csv(x = full_sparc, file = file.path("tidy_data", tidy_name), 
          row.names = F, na = "")

# Upload to GoogleDrive
googledrive::drive_upload(media = file.path("tidy_data", tidy_name), 
                          overwrite = T, path = tidy_drive)

# End ----




