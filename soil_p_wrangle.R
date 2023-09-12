## ------------------------------------------ ##
        # SPARC Soil P -- Data Wrangling
## ------------------------------------------ ##
# Script author(s): Nick J Lyon

# Purpose:
## Process 'archival' data for the purposes of the 'Soil P' LTER SPARC group

# Pre-Requisites:
## Assumes that "soil_p_harmonize.R" script has been run relatively recently
## Otherwise risks using a superseded version of the tidy data
### (that ccould lack raw data/data key information)

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
    measurement = ifelse(is.na(measurement), yes = "data.type", no = measurement),
    units = ifelse(is.na(units), yes = "units", no = units),
    order = ifelse(is.na(order), yes = "order", no = order),
    reagent = ifelse(is.na(reagent), yes = "reagent", no = reagent)
  ) %>%
  # Recombine them into a single column
  dplyr::mutate(P_fractions = paste(p_type, measurement, units, order, reagent,
                                    sep = "_")) %>%
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
dplyr::glimpse(tidy_v10)

# Now we'll want to add together our various types of P (conditionally)
p_sums <- tidy_v10 %>%
  # First need to fill NAs with 0s to avoid making NA sums
  ## Pivot longer
  tidyr::pivot_longer(cols = c(dplyr::starts_with("P_"),
                               dplyr::starts_with("Po_"),
                               dplyr::starts_with("ReBHsin_"),
                               dplyr::starts_with("Pi_")),
                      names_to = "names", values_to = "values") %>%
  ## Remove NA / missing values
  dplyr::filter(!is.na(values) & nchar(values) != 0) %>%
  ## Pivot back to wide format and fill empty cells with 0
  tidyr::pivot_wider(names_from = names, values_from = values, values_fill = 0) %>%
  # Calculate slow P conditionally
  ## NOTE: using placeholder (obviously wrong) while we await complete methods data knowledge / inclusion in data key
  dplyr::mutate(slow.P_conc_mg.kg = 1) %>%
  # dplyr::mutate(slow.P_mg_kg = dplyr::case_when(
  #   dataset == "HJAndrews_1" ~ (P_conc_HCl_mg_kg),
  #   dataset == "Bonanza Creek_1" ~ NA,
  #   dataset == "Bonanza Creek_2" ~ NA,
  #   dataset == "Brazil" ~ NA,
  #   dataset == "Calhoun" ~ (P_conc_HCl_mg_kg),
  #   # (vvv) Data seem to only have total P but searching for Hedley fraction
  #   dataset == "CedarCreek_1" ~ NA, 
  #   dataset == "Coweeta" ~ (P_conc_HCl_mg_kg), 
  #   # (vvv) Only have a neutral salt extraction (available P). May remove dataset entirely
  #   dataset == "Fernow" ~ NA,
#   dataset == "FloridaCoastal" ~ (P_conc_HCl_mg_kg),
#   dataset == "Hubbard Brook" ~ (P_conc_HNO3_cold_mg_kg),
#   dataset == "Jornada" ~ NA,
#   dataset == "Kellog_Biological_Station" ~ (Pi_conc_HCl_mg_kg),
#   # (vvv) Based on 2 different methodologies
#   dataset == "Konza_1" ~ (P_conc_Ca_bound_mg_kg),
#   dataset == "Luquillo_1" ~ NA,
#   # (vvv) Re-check! assuming these are the correct units, and dilute HCl is 1M step from Hedley
#   dataset == "Luquillo_2" ~ (P_conc_dilHCl_mg_kg),
#   dataset == "Niwot_1" ~ (P_conc_HCl_mg_kg),
#   dataset == "Niwot_2" ~ (P_conc_HCl_mg_kg),
#   dataset == "Niwot_3" ~ NA,
#   dataset == "Sevilleta_1" ~ (P_conc_HCl_mg_kg),
#   dataset == "Sevilleta_2" ~ NA,
#   # (vvv) If resulting number is negative it gets set to zero
#   dataset == "Toolik" ~ ifelse((P_conc_HCl_mg_kg - P_conc_citrate_mg_kg) < 0,
#                                yes = 0, 
#                                no = P_conc_HCl_mg_kg - P_conc_citrate_mg_kg), 
#   TRUE ~ NA )) %>%
# Also total P
## NOTE: using slow P as placeholder while we wait for identification of P fractions / site
dplyr::mutate(total.P_conc_mg.kg = slow.P_conc_mg.kg) %>%
  # After summing, remove all columns where we changed NAs to 0s
  dplyr::select(lter:soil.mass_g.m2, slow.P_conc_mg.kg, total.P_conc_mg.kg) %>%
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
tidy_v11 <- tidy_v10 %>%
  # By not specifying which columns to join by, all shared columns will be used
  dplyr::left_join(y = p_sums) %>%
  # Move our P sums to the left for more easy reference
  dplyr::relocate(slow.P_conc_mg.kg, total.P_conc_mg.kg, .after = bulk.density_kg.ha)

# Check structure
dplyr::glimpse(tidy_v11)

## ------------------------------------------ ##
# Stock P Calculations ----
## ------------------------------------------ ##

# Calculate absolute P totals (rather than portions of each core)
tidy_v12 <- tidy_v11 %>%
  # Due to our earlier depth/bulk density prep this is easy!
  dplyr::mutate(slow.P_stock_g.m2 = ((slow.P_conc_mg.kg * core.length_cm * bulk.density_g.cm3) * 10^4) / 10^6,
                .before = slow.P_conc_mg.kg) %>%
  dplyr::mutate(total.P_stock_g.m2 = ((total.P_conc_mg.kg * core.length_cm * bulk.density_g.cm3) * 10^4) / 10^6,
                .before = total.P_conc_mg.kg)

# Check contents of those columns
summary(tidy_v12$slow.P_stock)
summary(tidy_v12$total.P_stock)

# Re-check structure
dplyr::glimpse(tidy_v12[1:35])

## ------------------------------------------ ##
# Export ----
## ------------------------------------------ ##

# Create a final data object
final_tidy <- tidy_v12

# Check its structure
dplyr::glimpse(final_tidy)

# Create a folder to export into
dir.create(path = file.path("tidy_data"), showWarnings = F)

# And identify the tidy data Drive URL
tidy_drive <- googledrive::as_id("https://drive.google.com/drive/u/0/folders/1pjgN-wRlec65NDLBvryibifyx6k9Iqy9")

# Define the tidy file name
tidy_name <- "tidy_soil_p.csv"

# Save out the final data object
write.csv(x = final_tidy, file = file.path("tidy_data", tidy_name), 
          row.names = F, na = "")

# Upload to GoogleDrive
googledrive::drive_upload(media = file.path("tidy_data", tidy_name), 
                          overwrite = T, path = tidy_drive)

# Also define a name for the archival variant of the data
arch_name <- "sparc-soil-p_archival-data.csv"

# Export the version of the data for archiving in a data repository
write.csv(x = archive, row.names = F, na = '',
          file = file.path("tidy_data", arch_name))

# Upload that as well
googledrive::drive_upload(media = file.path("tidy_data", arch_name), 
                          overwrite = T, path = tidy_drive)

# End ----
