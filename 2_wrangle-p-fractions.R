## ------------------------------------------ ##
       # SPARC Soil P -- Data Wrangling
## ------------------------------------------ ##
# Script author(s): Nick J Lyon

# Purpose:
## Flips archival data to wide format and calculates various P fraction sums

## ------------------------------------------ ##
              # Housekeeping -----
## ------------------------------------------ ##

# Load necessary libraries
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive, magrittr)

# Create necessary sub-folder(s)
dir.create(path = file.path("data", "tidy_data"), showWarnings = F)

# Clear environment
rm(list = ls())

# Identify raw data files
tidy_drive <- googledrive::as_id("https://drive.google.com/drive/u/0/folders/1pjgN-wRlec65NDLBvryibifyx6k9Iqy9")

# Identify the archival data in that folder and download it
googledrive::drive_ls(path = tidy_drive) %>%
  dplyr::filter(name == "sparc-soil-p_archival-data.csv") %>%
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "tidy_data", .$name))

# Read that file in
sparc_v1 <- read.csv(file = file.path("data", "tidy_data", 
                                      "sparc-soil-p_archival-data.csv")) %>%
  # Drop row number column
  dplyr::select(-row_num)

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
    units = ifelse((is.na(units) | nchar(units) == 0), 
                   yes = "units", no = units),
    order = ifelse((is.na(order) | nchar(order) == 0), 
                   yes = "order", no = order),
    reagent = ifelse((is.na(reagent) | nchar(reagent) == 0), 
                     yes = "reagent", no = reagent)
  ) %>%
  # Recombine them into a single column
  dplyr::mutate(fractions = dplyr::case_when(
    reagent == "total" ~ paste(p_type, measurement, units, reagent, sep = "_"),
    T ~ paste(p_type, measurement, units, order, reagent, sep = "_"))) %>%
  # Drop the separate pieces of information
  dplyr::select(-p_type, -measurement, -units, -order, -reagent) %>%
  # Reclaim wide format!
  tidyr::pivot_wider(names_from = fractions, 
                     values_from = value,
                     values_fill = NA) %>%
  # And rename Al/Fe columns more simply
  dplyr::rename(Al_conc_mg.g_AC = Al_conc_mg.g_1_molarity_AC_time_temp,
                Al_conc_mg.g_OX = Al_conc_mg.g_1_molarity_OX_time_temp,
                Fe_conc_mg.g_AC = Fe_conc_mg.g_1_molarity_AC_time_temp,
                Fe_conc_mg.g_OX = Fe_conc_mg.g_1_molarity_OX_time_temp,
                Fe2_conc_mg.g_HCl = Fe2_conc_mg.g_order_molarity_HCl_time_temp,
                Fe3_conc_mg.g_HCl = Fe3_conc_mg.g_order_molarity_HCl_time_temp)

# Glimpse data structure
dplyr::glimpse(sparc_v2)

## ------------------------------------------ ##
          # Phosphorus Sums Prep ----
## ------------------------------------------ ##

# Glimpse the entire dataset
dplyr::glimpse(sparc_v2)

# Need to replace all NAs in P fraction columns with 0s to do addition
p_sums_v1 <- sparc_v2 %>%
  # First need to fill NAs with 0s to avoid making NA sums
  ## Pivot longer
  tidyr::pivot_longer(cols = c(dplyr::starts_with("P_"),
                               dplyr::starts_with("Po_"),
                               dplyr::starts_with("Pi_")),
                      names_to = "names", values_to = "values") %>%
  ## Remove NA / missing values
  dplyr::filter(!is.na(values) & nchar(values) != 0) %>%
  ## Pivot back to wide format and fill empty cells with 0
  tidyr::pivot_wider(names_from = names, values_from = values) %>%
  # Also drop P stocks (mg/m2)
  dplyr::select(-dplyr::contains("_stock_"))

# Check that out
dplyr::glimpse(p_sums_v1)

## ------------------------------------------ ##
             # P Sums - Slow ----
## ------------------------------------------ ##

# For the below to work we need to easily reference which P fractions exist for each dataset
for(data_obj in sort(unique(p_sums_v1$dataset))){
  
  # Want to know which P fractions are actually in the data
  sub <- p_sums_v1 %>%
    # Filter to this dataset
    dplyr::filter(dataset == data_obj) %>%
    # Drop completely NA/empty columns
    dplyr::select(dplyr::where(fn = ~ !(all(is.na(.) | all(nchar(.) == 0)) ) ) ) %>%
    # Keep only P concentration columns
    dplyr::select(dplyr::contains("_conc_mg.kg")) %>%
    # What is left?
    names()
  
  # Message that out for later use
  message("Following fractions found for dataset '", data_obj, "': ")
  print(paste(sub, collapse = "; ")) }

# Now we'll want to add together our various types of P (conditionally)
p_sums_v2 <- p_sums_v1 %>%
  # Calculate slow P conditionally
  dplyr::mutate(slow.P_conc_mg.kg = dplyr::case_when(
    dataset == "Bonanza Creek_1" ~ NA,
    dataset == "Bonanza Creek_2" ~ NA,
    dataset == "Bonanza Creek_3" ~ NA,
    dataset == "Brazil" ~ NA,
    dataset == "Calhoun" ~ (P_conc_mg.kg_4_HCl),
    dataset == "CedarCreek_1" ~ NA,
    dataset == "Coweeta" ~ (P_conc_mg.kg_4_HCl),
    dataset == "HJAndrews_1" ~ (P_conc_mg.kg_4_HCl),
    dataset == "Hubbard Brook" ~ (P_conc_mg.kg_3_HNO3),
    dataset == "FloridaCoastal" ~ (P_conc_mg.kg_4_HCl),
    dataset == "Jornada_1" ~ NA,
    dataset == "Jornada_2" ~ (P_conc_mg.kg_3_HCl),
    dataset == "Kellogg_Bio_Station" ~ (Pi_conc_mg.kg_6_HCl),
    dataset == "Konza_1" ~ (P_conc_mg.kg_3_Ca.bound),
    dataset == "Luquillo_1" ~ NA,
    dataset == "Luquillo_2" ~ (P_conc_mg.kg_4_HCl),
    dataset == "Luquillo_3" ~ (P_conc_mg.kg_3_residual),
    dataset == "Niwot_1" ~ (P_conc_mg.kg_4_HCl),
    dataset == "Niwot_2" ~ (P_conc_mg.kg_4_HCl),
    dataset == "Niwot_3" ~ NA,
    dataset == "Niwot_4" ~ NA,
    dataset == "Sevilleta_1" ~ (P_conc_mg.kg_5_HCl),
    dataset == "Sevilleta_2" ~ NA,
    # (vvv) If resulting number is negative it gets set to zero
    dataset == "Toolik_1" ~ ifelse((P_conc_mg.kg_1_HCl - P_conc_mg.kg_1_citrate) < 0,
                                   yes = 0,
                                   no = P_conc_mg.kg_1_HCl - P_conc_mg.kg_1_citrate),
    dataset == "Toolik_2" ~ NA,
    T ~ NA))

## ------------------------------------------ ##
            # P Sums - Total ----
## ------------------------------------------ ##

# Recall P fractions for calculating total P
for(data_obj in sort(unique(p_sums_v1$dataset))){
  
  # Want to know which P fractions are actually in the data
  sub <- p_sums_v1 %>%
    # Filter to this dataset
    dplyr::filter(dataset == data_obj) %>%
    # Drop completely NA/empty columns
    dplyr::select(dplyr::where(fn = ~ !(all(is.na(.) | all(nchar(.) == 0)) ) ) ) %>%
    # Keep only P concentration columns
    dplyr::select(dplyr::contains("_conc_mg.kg")) %>%
    # What is left?
    names()
  
  # Message that out for later use
  message("Following fractions found for dataset '", data_obj, "': ")
  print(paste(sub, collapse = "; ")) }

# Calculate total P next
p_sums_v3 <- p_sums_v2 %>%
  # Do the same for total P
  dplyr::mutate(total.P_conc_mg.kg = dplyr::case_when(
    dataset == "Bonanza Creek_1" ~ (P_conc_mg.kg_total),
    dataset == "Bonanza Creek_2" ~ (P_conc_mg.kg_total),
    dataset == "Bonanza Creek_3" ~ (P_conc_mg.kg_total),
    dataset == "Brazil" ~ (P_conc_mg.kg_total),
    dataset == "Calhoun" ~ (P_conc_mg.kg_total),
    dataset == "CedarCreek_1" ~ NA,
    dataset == "Coweeta" ~ (P_conc_mg.kg_1_NH4Cl + P_conc_mg.kg_2_HCO3 + P_conc_mg.kg_3_NaOH +
                              P_conc_mg.kg_4_HCl + P_conc_mg.kg_5_residual),
    dataset == "HJAndrews_1" ~ (P_conc_mg.kg_total),
    # (vvv) Both HNO3s should be used (3 is cold, 4 is hot)
    dataset == "Hubbard Brook" ~ (P_conc_mg.kg_1_NH4Cl + P_conc_mg.kg_2_H2O2 +
                                    P_conc_mg.kg_3_HNO3 + P_conc_mg.kg_4_HNO3),
    dataset == "FloridaCoastal" ~ (P_conc_mg.kg_1_resin + P_conc_mg.kg_2_HCO3 +
                                     P_conc_mg.kg_3_NaOH + P_conc_mg.kg_4_HCl +
                                     P_conc_mg.kg_5_residual),
    dataset == "Jornada_1" ~ (P_conc_mg.kg_total),
    dataset == "Jornada_2" ~ (P_conc_mg.kg_1_MgCl2 + P_conc_mg.kg_2_NaOH + P_conc_mg.kg_3_HCl +
                                P_conc_mg.kg_4_residual),
    dataset == "Kellogg_Bio_Station" ~ (Pi_conc_mg.kg_1_resin + 
                                                Pi_conc_mg.kg_2_NaHCO3 + 
                                                Po_conc_mg.kg_2_NaHCO3 +
                                                Pi_conc_mg.kg_3_microbial + 
                                                Po_conc_mg.kg_3_microbial +
                                                Pi_conc_mg.kg_4_NaOH + Po_conc_mg.kg_4_NaOH +
                                                Pi_conc_mg.kg_5_sonic.NaOH + 
                                                Po_conc_mg.kg_5_sonic.NaOH + 
                                                Pi_conc_mg.kg_6_HCl + P_conc_mg.kg_7_residual),
    dataset == "Konza_1" ~ (P_conc_mg.kg_1_Al.Fe + P_conc_mg.kg_2_occluded + 
                              P_conc_mg.kg_3_Ca.bound),
    dataset == "Luquillo_1" ~ NA,
    ## (vvv) May exchange for pre-existing 'total P' column in raw data
    dataset == "Luquillo_2" ~ (P_conc_mg.kg_1_resin + P_conc_mg.kg_2_NaHCO3 + 
                                 P_conc_mg.kg_3_NaOH + P_conc_mg.kg_4_HCl + 
                                 P_conc_mg.kg_5_residual),
    dataset == "Luquillo_3" ~ (P_conc_mg.kg_total),
    dataset == "Niwot_1" ~ (P_conc_mg.kg_1_resin + 
                              Pi_conc_mg.kg_2_HCO3 + Po_conc_mg.kg_2_HCO3 + 
                              Pi_conc_mg.kg_3_NaOH + Po_conc_mg.kg_3_NaOH +
                              P_conc_mg.kg_4_HCl +
                              Pi_conc_mg.kg_5_sonic.HCl + Po_conc_mg.kg_5_sonic.HCl +
                              P_conc_mg.kg_6_residual),
    dataset == "Niwot_2" ~ (P_conc_mg.kg_1_resin +
                              Pi_conc_mg.kg_2_HCO3 + Po_conc_mg.kg_2_HCO3 +
                              Pi_conc_mg.kg_3_NaOH + Po_conc_mg.kg_3_NaOH +
                              P_conc_mg.kg_4_HCl + P_conc_mg.kg_5_residual),
    dataset == "Niwot_3" ~ (P_conc_mg.kg_total),
    dataset == "Niwot_4" ~ (P_conc_mg.kg_total),
    dataset == "Sevilleta_1" ~ (P_conc_mg.kg_total),
    dataset == "Sevilleta_2" ~ (P_conc_mg.kg_total),
    dataset == "Toolik_1" ~ NA,
    dataset == "Toolik_2" ~ (P_conc_mg.kg_total),
    T ~ NA))

## ------------------------------------------ ##
            # P Sums - Available ----
## ------------------------------------------ ##

# Recall extant P fractions
for(data_obj in sort(unique(p_sums_v1$dataset))){
  
  # Want to know which P fractions are actually in the data
  sub <- p_sums_v1 %>%
    # Filter to this dataset
    dplyr::filter(dataset == data_obj) %>%
    # Drop completely NA/empty columns
    dplyr::select(dplyr::where(fn = ~ !(all(is.na(.) | all(nchar(.) == 0)) ) ) ) %>%
    # Keep only P concentration columns
    dplyr::select(dplyr::contains("_conc_mg.kg")) %>%
    # What is left?
    names()
  
  # Message that out for later use
  message("Following fractions found for dataset '", data_obj, "': ")
  print(paste(sub, collapse = "; ")) }

# Calculate available P
p_sums_v4 <- p_sums_v3 %>%
  dplyr::mutate(available.P_conc_mg.kg = dplyr::case_when(
    dataset == "Bonanza Creek_1" ~ NA,
    dataset == "Bonanza Creek_2" ~ NA,
    dataset == "Bonanza Creek_3" ~ NA,
    dataset == "Brazil" ~ NA,
    dataset == "Calhoun" ~ (P_conc_mg.kg_1_resin + Po_conc_mg.kg_2_HCO3 + Pi_conc_mg.kg_2_HCO3),
    dataset == "CedarCreek_1" ~ NA,
    dataset == "CedarCreek_2" ~ NA,
    dataset == "Coweeta" ~ (P_conc_mg.kg_1_NH4Cl),
    dataset == "FloridaCoastal" ~ (P_conc_mg.kg_1_resin + P_conc_mg.kg_2_HCO3),
    dataset == "HJAndrews_1" ~ NA,
    dataset == "Hubbard Brook" ~ NA,
    dataset == "Jornada_1" ~ NA,
    dataset == "Jornada_2" ~ NA,
    dataset == "Kellogg_Bio_Station" ~ (Pi_conc_mg.kg_1_resin + 
                                          Po_conc_mg.kg_2_NaHCO3 + Pi_conc_mg.kg_2_NaHCO3),
    dataset == "Konza_1" ~ NA,
    dataset == "Luquillo_1" ~ (P_conc_mg.kg_1_resin + Po_conc_mg.kg_1_HCO3 + Pi_conc_mg.kg_1_HCO3),
    dataset == "Luquillo_2" ~ (P_conc_mg.kg_1_resin + P_conc_mg.kg_2_NaHCO3),
    dataset == "Luquillo_3" ~ NA,
    dataset == "Niwot_1" ~ (P_conc_mg.kg_1_resin + Po_conc_mg.kg_2_HCO3 + Pi_conc_mg.kg_2_HCO3),
    dataset == "Niwot_2" ~ (P_conc_mg.kg_1_resin + Po_conc_mg.kg_2_HCO3 + Pi_conc_mg.kg_2_HCO3),
    dataset == "Niwot_3" ~ NA,
    dataset == "Niwot_4" ~ NA,
    dataset == "Sevilleta_1" ~ (P_conc_mg.kg_2_HCO3),
    dataset == "Sevilleta_2" ~ NA,
    dataset == "Toolik_1" ~ NA,
    dataset == "Toolik_2" ~ NA,
    T ~ NA))

## ------------------------------------------ ##
              # P Sums - Bicarb ----
## ------------------------------------------ ##

# Recall extant P fractions
for(data_obj in sort(unique(p_sums_v1$dataset))){
  
  # Want to know which P fractions are actually in the data
  sub <- p_sums_v1 %>%
    # Filter to this dataset
    dplyr::filter(dataset == data_obj) %>%
    # Drop completely NA/empty columns
    dplyr::select(dplyr::where(fn = ~ !(all(is.na(.) | all(nchar(.) == 0)) ) ) ) %>%
    # Keep only P concentration columns
    dplyr::select(dplyr::contains("_conc_mg.kg")) %>%
    # What is left?
    names()
  
  # Message that out for later use
  message("Following fractions found for dataset '", data_obj, "': ")
  print(paste(sub, collapse = "; ")) }

# Calculate bicarb P
p_sums_v5 <- p_sums_v4 %>%
  dplyr::mutate(bicarb.P_conc_mg.kg = dplyr::case_when(
    dataset == "Bonanza Creek_1" ~ NA,
    dataset == "Bonanza Creek_2" ~ NA,
    dataset == "Bonanza Creek_3" ~ NA,
    dataset == "Brazil" ~ NA,
    dataset == "Calhoun" ~ (Po_conc_mg.kg_2_HCO3 + Pi_conc_mg.kg_2_HCO3),
    dataset == "CedarCreek_1" ~ NA,
    dataset == "CedarCreek_2" ~ NA,
    dataset == "Coweeta" ~ NA,
    dataset == "FloridaCoastal" ~ P_conc_mg.kg_2_HCO3,
    dataset == "HJAndrews_1" ~ (P_conc_mg.kg_2_HCO3),
    dataset == "Hubbard Brook" ~ NA,
    dataset == "Jornada_1" ~ NA,
    dataset == "Jornada_2" ~ NA,
    dataset == "Kellogg_Bio_Station" ~ (Po_conc_mg.kg_2_NaHCO3 + Pi_conc_mg.kg_2_NaHCO3),
    dataset == "Konza_1" ~ NA,
    dataset == "Luquillo_1" ~ (Po_conc_mg.kg_1_HCO3 + Pi_conc_mg.kg_1_HCO3),
    dataset == "Luquillo_2" ~ (P_conc_mg.kg_2_NaHCO3),
    dataset == "Luquillo_3" ~ NA,
    dataset == "Niwot_1" ~ (Po_conc_mg.kg_2_HCO3 + Pi_conc_mg.kg_2_HCO3),
    dataset == "Niwot_2" ~ (Po_conc_mg.kg_2_HCO3 + Pi_conc_mg.kg_2_HCO3),
    dataset == "Niwot_3" ~ NA,
    dataset == "Niwot_4" ~ NA,
    dataset == "Sevilleta_1" ~ ( P_conc_mg.kg_2_HCO3),
    dataset == "Sevilleta_2" ~ NA,
    dataset == "Toolik_1" ~ NA,
    dataset == "Toolik_2" ~ NA,
    T ~ NA))

## ------------------------------------------ ##
            # P Sums - Biological ----
## ------------------------------------------ ##

# Recall extant P fractions
for(data_obj in sort(unique(p_sums_v1$dataset))){
  
  # Want to know which P fractions are actually in the data
  sub <- p_sums_v1 %>%
    # Filter to this dataset
    dplyr::filter(dataset == data_obj) %>%
    # Drop completely NA/empty columns
    dplyr::select(dplyr::where(fn = ~ !(all(is.na(.) | all(nchar(.) == 0)) ) ) ) %>%
    # Keep only P concentration columns
    dplyr::select(dplyr::contains("_conc_mg.kg")) %>%
    # What is left?
    names()
  
  # Message that out for later use
  message("Following fractions found for dataset '", data_obj, "': ")
  print(paste(sub, collapse = "; ")) }

# Calculate biological P
p_sums_v6 <- p_sums_v5 %>%
  dplyr::mutate(biological.P_conc_mg.kg = dplyr::case_when(
    dataset == "Bonanza Creek_1" ~ NA,
    dataset == "Bonanza Creek_2" ~ NA,
    dataset == "Bonanza Creek_3" ~ NA,
    dataset == "Brazil" ~ NA,
    dataset == "Calhoun" ~ (P_conc_mg.kg_1_resin + Po_conc_mg.kg_2_HCO3 + Po_conc_mg.kg_3_NaOH),
    dataset == "CedarCreek_1" ~ NA,
    dataset == "CedarCreek_2" ~ NA,
    dataset == "Coweeta" ~ NA,
    dataset == "FloridaCoastal" ~ NA,
    dataset == "HJAndrews_1" ~ NA,
    dataset == "Hubbard Brook" ~ NA,
    dataset == "Jornada_1" ~ NA,
    dataset == "Jornada_2" ~ NA,
    dataset == "Kellogg_Bio_Station" ~ (Pi_conc_mg.kg_1_resin + Po_conc_mg.kg_2_NaHCO3 + 
                                          Po_conc_mg.kg_4_NaOH),
    dataset == "Konza_1" ~ NA,
    dataset == "Luquillo_1" ~ NA,
    dataset == "Luquillo_2" ~ NA,
    dataset == "Luquillo_3" ~ NA,
    dataset == "Niwot_1" ~ (P_conc_mg.kg_1_resin + Po_conc_mg.kg_2_HCO3 + Po_conc_mg.kg_3_NaOH),
    dataset == "Niwot_2" ~ (P_conc_mg.kg_1_resin + Po_conc_mg.kg_2_HCO3 + Po_conc_mg.kg_3_NaOH),
    dataset == "Niwot_3" ~ NA,
    dataset == "Niwot_4" ~ NA,
    dataset == "Sevilleta_1" ~ NA,
    dataset == "Sevilleta_2" ~ NA,
    dataset == "Toolik_1" ~ NA,
    dataset == "Toolik_2" ~ NA,
    T ~ NA))

## ------------------------------------------ ##
        # P Sums - Intermediate ----
## ------------------------------------------ ##

# Recall extant P fractions
for(data_obj in sort(unique(p_sums_v1$dataset))){
  
  # Want to know which P fractions are actually in the data
  sub <- p_sums_v1 %>%
    # Filter to this dataset
    dplyr::filter(dataset == data_obj) %>%
    # Drop completely NA/empty columns
    dplyr::select(dplyr::where(fn = ~ !(all(is.na(.) | all(nchar(.) == 0)) ) ) ) %>%
    # Keep only P concentration columns
    dplyr::select(dplyr::contains("_conc_mg.kg")) %>%
    # What is left?
    names()
  
  # Message that out for later use
  message("Following fractions found for dataset '", data_obj, "': ")
  print(paste(sub, collapse = "; ")) }

# Calculate intermediate P
p_sums_v7 <- p_sums_v6 %>%
  dplyr::mutate(intermediate.P_conc_mg.kg = dplyr::case_when(
    dataset == "Bonanza Creek_1" ~ NA,
    dataset == "Bonanza Creek_2" ~ NA,
    dataset == "Bonanza Creek_3" ~ NA,
    dataset == "Brazil" ~ NA,
    dataset == "Calhoun" ~ (P_conc_mg.kg_1_resin +
                              Po_conc_mg.kg_2_HCO3 + Pi_conc_mg.kg_2_HCO3 +
                              Po_conc_mg.kg_3_NaOH + Pi_conc_mg.kg_3_NaOH),
    dataset == "CedarCreek_1" ~ NA,
    dataset == "CedarCreek_2" ~ NA,
    dataset == "Coweeta" ~ (P_conc_mg.kg_1_NH4Cl + P_conc_mg.kg_3_NaOH),
    dataset == "FloridaCoastal" ~ (P_conc_mg.kg_2_HCO3 + 
                                     Po_conc_mg.kg_3_NaOH + Pi_conc_mg.kg_3_NaOH),
    dataset == "HJAndrews_1" ~ (P_conc_mg.kg_1_resin + P_conc_mg.kg_2_HCO3 + P_conc_mg.kg_3_NaOH),
    dataset == "Hubbard Brook" ~ NA,
    dataset == "Jornada_1" ~ NA,
    dataset == "Jornada_2" ~ (P_conc_mg.kg_1_MgCl2 + P_conc_mg.kg_2_NaOH),
    dataset == "Kellogg_Bio_Station" ~ (Pi_conc_mg.kg_1_resin +
                                          Po_conc_mg.kg_2_NaHCO3 + Pi_conc_mg.kg_2_NaHCO3 +
                                          Po_conc_mg.kg_4_NaOH + Pi_conc_mg.kg_4_NaOH),
    dataset == "Konza_1" ~ NA,
    dataset == "Luquillo_1" ~ (Po_conc_mg.kg_1_HCO3 + Pi_conc_mg.kg_1_HCO3 +
                                 Po_conc_mg.kg_2_NaOH + Pi_conc_mg.kg_2_NaOH),
    dataset == "Luquillo_2" ~ (P_conc_mg.kg_2_NaHCO3 + P_conc_mg.kg_3_NaOH),
    dataset == "Luquillo_3" ~ (Po_conc_mg.kg_1_NaOH + Pi_conc_mg.kg_1_NaOH),
    dataset == "Niwot_1" ~ (P_conc_mg.kg_1_resin +
                              Po_conc_mg.kg_2_HCO3 + Pi_conc_mg.kg_2_HCO3 +
                              Po_conc_mg.kg_3_NaOH + Pi_conc_mg.kg_3_NaOH),
    dataset == "Niwot_2" ~ (P_conc_mg.kg_1_resin +
                              Po_conc_mg.kg_2_HCO3 + Pi_conc_mg.kg_2_HCO3 +
                              Po_conc_mg.kg_3_NaOH + Pi_conc_mg.kg_3_NaOH),
    dataset == "Niwot_3" ~ NA,
    dataset == "Niwot_4" ~ NA,
    dataset == "Sevilleta_1" ~ (P_conc_mg.kg_2_HCO3 + P_conc_mg.kg_3_NaOH),
    dataset == "Sevilleta_2" ~ NA,
    dataset == "Toolik_1" ~ NA,
    dataset == "Toolik_2" ~ NA,
    T ~ NA))

## ------------------------------------------ ##
              # P Sums - NaOH P ----
## ------------------------------------------ ##

# Recall extant P fractions
for(data_obj in sort(unique(p_sums_v1$dataset))){
  
  # Want to know which P fractions are actually in the data
  sub <- p_sums_v1 %>%
    # Filter to this dataset
    dplyr::filter(dataset == data_obj) %>%
    # Drop completely NA/empty columns
    dplyr::select(dplyr::where(fn = ~ !(all(is.na(.) | all(nchar(.) == 0)) ) ) ) %>%
    # Keep only P concentration columns
    dplyr::select(dplyr::contains("_conc_mg.kg")) %>%
    # What is left?
    names()
  
  # Message that out for later use
  message("Following fractions found for dataset '", data_obj, "': ")
  print(paste(sub, collapse = "; ")) }

# Calculate NaOH P
p_sums_v8 <- p_sums_v7 %>%
  dplyr::mutate(NaOH.P_conc_mg.kg = dplyr::case_when(
    dataset == "Bonanza Creek_1" ~ NA,
    dataset == "Bonanza Creek_2" ~ NA,
    dataset == "Bonanza Creek_3" ~ NA,
    dataset == "Brazil" ~ NA,
    dataset == "Calhoun" ~ (Po_conc_mg.kg_3_NaOH + Pi_conc_mg.kg_3_NaOH),
    dataset == "CedarCreek_1" ~ NA,
    dataset == "CedarCreek_2" ~ NA,
    dataset == "Coweeta" ~ (P_conc_mg.kg_3_NaOH),
    dataset == "FloridaCoastal" ~ (P_conc_mg.kg_3_NaOH),
    dataset == "HJAndrews_1" ~ (P_conc_mg.kg_3_NaOH),
    dataset == "Hubbard Brook" ~ NA,
    dataset == "Jornada_1" ~ NA,
    dataset == "Jornada_2" ~ NA,
    dataset == "Kellogg_Bio_Station" ~ (Po_conc_mg.kg_4_NaOH + Pi_conc_mg.kg_4_NaOH),
    dataset == "Konza_1" ~ NA,
    dataset == "Luquillo_1" ~ (Po_conc_mg.kg_2_NaOH + Pi_conc_mg.kg_2_NaOH),
    dataset == "Luquillo_2" ~ (P_conc_mg.kg_3_NaOH),
    dataset == "Luquillo_3" ~ (P_conc_mg.kg_1_NaOH),
    dataset == "Niwot_1" ~ (Po_conc_mg.kg_3_NaOH + Pi_conc_mg.kg_3_NaOH),
    dataset == "Niwot_2" ~ (Po_conc_mg.kg_3_NaOH + Pi_conc_mg.kg_3_NaOH),
    dataset == "Niwot_3" ~ NA,
    dataset == "Niwot_4" ~ NA,
    dataset == "Sevilleta_1" ~ (P_conc_mg.kg_3_NaOH),
    dataset == "Sevilleta_2" ~ NA,
    dataset == "Toolik_1" ~ NA,
    dataset == "Toolik_2" ~ NA,
    T ~ NA))

## ------------------------------------------ ##
              # Finish P Sums ----
## ------------------------------------------ ##

# Make a final version of the p_sums object that is as simple as possible
p_sums <- p_sums_v8 %>%
  # Remove all P fraction columns (because we changed real NAs to convenient 0s)
  dplyr::select(-dplyr::starts_with("P_"), -dplyr::starts_with("Po_"), 
                -dplyr::starts_with("Pi_")) %>%
  # Keep only unique rows
  dplyr::distinct()

# Any datasets missing?
p_sums %>%
  dplyr::filter(is.na(slow.P_conc_mg.kg) | is.na(total.P_conc_mg.kg) |
                  is.na(available.P_conc_mg.kg) | is.na(bicarb.P_conc_mg.kg) |
                  is.na(biological.P_conc_mg.kg) | is.na(intermediate.P_conc_mg.kg) |
                  is.na(NaOH.P_conc_mg.kg) ) %>%
  dplyr::group_by(dataset) %>%
  dplyr::summarize(slow_mean = mean(slow.P_conc_mg.kg, na.rm = T),
                   total_mean = mean(total.P_conc_mg.kg, na.rm = T),
                   avail_mean = mean(available.P_conc_mg.kg, na.rm = T),
                   bicarb_mean = mean(bicarb.P_conc_mg.kg, na.rm = T),
                   bio_mean = mean(biological.P_conc_mg.kg, na.rm = T),
                   inter_mean = mean(intermediate.P_conc_mg.kg, na.rm = T),
                   naoh_mean = mean(NaOH.P_conc_mg.kg, na.rm = T) ) %>%
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
            # Export P Sums Data ----
## ------------------------------------------ ##

# Make a final data object
final_sparc <- sparc_v4

# Glimpse it
dplyr::glimpse(final_sparc)

# Define file name
sparc_name <- "sparc-soil-p_full-data-p-sums.csv"

# Export locally
write.csv(x = final_sparc, row.names = F, na = '',
          file = file.path("data", "tidy_data", sparc_name))

# Export to that folder in the Drive
googledrive::drive_upload(media = file.path("data", "tidy_data", sparc_name), 
                          overwrite = T, path = tidy_drive)

# End ----
