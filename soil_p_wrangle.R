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
sparc_v1 <- read.csv(file = file.path("tidy_data", "sparc-soil-p_archival-data.csv")) %>%
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
  dplyr::mutate(P_fractions = ifelse(!reagent %in% c("total"),
                                     yes = paste(p_type, measurement, units, order, 
                                                 reagent, sep = "_"),
                                     no = paste(p_type, measurement, units,
                                                reagent, sep = "_"))) %>%
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
    dataset == "Kellog_Biological_Station" ~ (Pi_conc_mg.kg_6_HCl),
    dataset == "Konza_1" ~ (P_conc_mg.kg_3_Ca.bound),
    dataset == "Luquillo_1" ~ NA,
    dataset == "Luquillo_2" ~ (P_conc_mg.kg_4_HCl),
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
    dataset == "Kellog_Biological_Station" ~ (Pi_conc_mg.kg_1_resin + 
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
        # Standardize Spatial Info ----
## ------------------------------------------ ##

# Not all datasets are collected at the same level of spatial granularity
## Those that don't have a given level (e.g., data only at plot level not specific cores)...
## ...have NA in the levels of information that they are missing
# We'll fill those with a standard character here so that we can use it for stats/graphing purposes

# Check structure of the relevant columns
sparc_v4 %>%
  dplyr::select(lter, dataset, site, block, plot, core) %>%
  dplyr::glimpse()

# Fill missing entries in a dataset-specific way
sparc_v5 <- sparc_v4 %>%
  # If site is missing, fill with dataset name
  dplyr::mutate(site = ifelse(test = (is.na(site) | nchar(site) == 0),
                              yes = dataset, no = site)) %>%
  # If block is missing, fill with site
  dplyr::mutate(block = ifelse(test = (is.na(block) | nchar(block) == 0),
                               yes = site, no = block)) %>%
  # If plot is missing, fill with block
  dplyr::mutate(plot = ifelse(test = (is.na(plot) | nchar(plot) == 0),
                               yes = block, no = plot)) %>%
  # If core is missing, fill with plot
  dplyr::mutate(core = ifelse(test = (is.na(core) | nchar(core) == 0),
                              yes = plot, no = core))

# Re-check structure
sparc_v5 %>%
  dplyr::select(lter, dataset, site, block, plot, core) %>%
  dplyr::glimpse()

# Collapse spatial organization to get a quick sense of how many granularity is available
sparc_v5 %>%
  dplyr::group_by(lter, dataset) %>%
  dplyr::summarize(site_ct = length(unique(site)),
                   sites = paste(unique(site), collapse = "; "),
                   block_ct = length(unique(block)),
                   blocks = paste(unique(block), collapse = "; "),
                   plot_ct = length(unique(plot)),
                   plots = paste(unique(plot), collapse = "; "),
                   core_ct = length(unique(core)),
                   cores = paste(unique(core), collapse = "; "))

## ------------------------------------------ ##
        # Export Full SPARC Data ----
## ------------------------------------------ ##

# Create a final data object
sparc_tidy <- sparc_v5 %>%
  # Simplify dataset names to make plot labels neater
  dplyr::mutate(dataset_simp = gsub(pattern = "Bonanza Creek", replacement = "BNZ", 
                                    x = dataset), .before = dataset) %>%
  dplyr::mutate(dataset_simp = gsub(pattern = "CedarCreek", replacement = "CDR", 
                                    x = dataset_simp)) %>%
  dplyr::mutate(dataset_simp = gsub(pattern = "Coweeta", replacement = "CWT", 
                                    x = dataset_simp)) %>%
  dplyr::mutate(dataset_simp = gsub(pattern = "FloridaCoastal", replacement = "FCE", 
                                    x = dataset_simp)) %>%
  dplyr::mutate(dataset_simp = gsub(pattern = "HJAndrews", replacement = "AND", 
                                  x = dataset_simp)) %>%
  dplyr::mutate(dataset_simp = gsub(pattern = "Hubbard Brook", replacement = "HBR", 
                                    x = dataset_simp)) %>%
  dplyr::mutate(dataset_simp = gsub(pattern = "Jornada", replacement = "JRN", 
                                    x = dataset_simp)) %>%
  dplyr::mutate(dataset_simp = gsub(pattern = "Kellogg_Bio_Station", replacement = "KBS", 
                                    x = dataset_simp)) %>%
  dplyr::mutate(dataset_simp = gsub(pattern = "Konza", replacement = "KNZ", 
                                    x = dataset_simp)) %>%
  dplyr::mutate(dataset_simp = gsub(pattern = "Luquillo", replacement = "LUQ", 
                                    x = dataset_simp)) %>%
  dplyr::mutate(dataset_simp = gsub(pattern = "Niwot", replacement = "NWT", 
                                    x = dataset_simp)) %>%
  dplyr::mutate(dataset_simp = gsub(pattern = "Sevilleta", replacement = "SEV", 
                                    x = dataset_simp)) %>%
  dplyr::mutate(dataset_simp = gsub(pattern = "Toolik", replacement = "ARC", 
                                    x = dataset_simp))

# Check simplified dataset names
sort(unique(sparc_tidy$dataset_simp))

# Check its structure
dplyr::glimpse(sparc_tidy)

# Define the tidy file name
tidy_name <- "full-data_tidy-soil-p.csv"

# Save out the final data object
write.csv(x = sparc_tidy, file = file.path("tidy_data", tidy_name), 
          row.names = F, na = "")

## ------------------------------------------ ##
    # Statistics / Visualization Prep ----
## ------------------------------------------ ##

# We definitely want the data we just exported BUT
## we also want a really simplified version for stats/visualization
## this will make it much easier to navigate the really fundamental parts of the data
## while still having easy access to the most granular version of the data (exported above)

# Megadata includes *a lot* of information and we only really need a subset of it for stats
stats_v1 <- sparc_tidy %>%
  # Pare down to only columns of interest
  ## Unspecified columns are implicitly removed
  dplyr::select(lter, dataset_simp, dataset, site, block, plot, core,
                dplyr::starts_with("horizon"), dplyr::starts_with("depth."),
                core.length_cm, bulk.density_g.cm3,
                dplyr::ends_with(".P_conc_mg.kg"),
                C_conc_percent, N_conc_percent) %>%
  # Drop non-unique rows
  dplyr::distinct()

# How do the dataframe dimensions change?
dim(sparc_tidy); dim(stats_v1)
## Lose many columns but no rows? Good!

# Check structure
dplyr::glimpse(stats_v1)

# Need to subset to only certain horizons and where N/C data are present
stats_v2 <- stats_v1 %>%
  # Keep only mineral layer (and mixed mineral/organic) data 
  dplyr::filter(horizon_binary %in% c("mineral", "mixed") |
                  # Also keep unspecified horizon information (assumes mineral)
                  nchar(horizon_binary) == 0) %>%
  # Coerce empty N/C percents into true NAs
  dplyr::mutate(C_conc_percent = ifelse(nchar(C_conc_percent) == 0,
                                        yes = NA, no = C_conc_percent),
                N_conc_percent = ifelse(nchar(N_conc_percent) == 0,
                                        yes = NA, no = N_conc_percent)) %>%
  # Also we need **either** N or C information in addition to P data for statistics
  dplyr::filter(!is.na(C_conc_percent) | !is.na(N_conc_percent))
  
# How do the dataframe dimensions change?
dim(stats_v1); dim(stats_v2)
## Lose many rows but no columns? Good!

# Check structure
dplyr::glimpse(stats_v2)

# Finally, we want to subset to only particular depths within those horizons
stats_v3 <- stats_v2 %>%
  # Keep only cores beginning at the top of the horizon
  dplyr::filter(depth.start_cm == 0 | 
                  # Again, keep missing depths on assumption they start at 0
                  nchar(depth.start_cm) == 0 | is.na(depth.start_cm))

# How do the dataframe dimensions change?
dim(stats_v2); dim(stats_v3)
## Lose some rows but no columns? Good!

# Check structure
dplyr::glimpse(stats_v3)
## tibble::view(stats_v3)

## ------------------------------------------ ##
          # Export Statistics Data ----
## ------------------------------------------ ##

# Create a final data object
sparc_stats <- stats_v3

# Check its structure
dplyr::glimpse(sparc_stats)

# Define the tidy file name
stats_name <- "stats-ready_tidy-soil-p.csv"

# Save out the final data object
write.csv(x = sparc_stats, file = file.path("tidy_data", stats_name), 
          row.names = F, na = "")

## ------------------------------------------ ##
            # Spatial Aggregation ----
## ------------------------------------------ ##

# We also want to average data within progressively coarser units of spatial organization
## First, average across cores within plots
## Second, average across plots within blocks
## Third, average across blocks within sites

# Prepare to take averages by:
avgs_prep <- sparc_stats %>%
  # Dropping core-specific depth/horizon columns
  dplyr::select(-core, -dplyr::starts_with("horizon"),
                -dplyr::starts_with("depth."), -core.length_cm,
                -bulk.density_g.cm3) %>%
  # Flipping to long format
  tidyr::pivot_longer(cols = slow.P_conc_mg.kg:N_conc_percent,
                      names_to = "variables", values_to = "vals")

# Check structure
dplyr::glimpse(avgs_prep)

# Begin with averaging across cores within plots
plots_v1 <- avgs_prep %>%
  # Group by everything and average the response variables
  dplyr::group_by(lter, dataset_simp, dataset, site, block, plot, variables) %>%
  dplyr::summarize(mean = mean(vals, na.rm = T),
                   std.dev = sd(vals, na.rm = T),
                   sample.size = dplyr::n(),
                   std.error = std.dev / sqrt(sample.size)) %>%
  dplyr::ungroup()

# Check structure
dplyr::glimpse(plots_v1)

# Check dimension change from that step
dim(avgs_prep); dim(plots_v1)

# Next, average across plots within blocks
blocks_v1 <- plots_v1 %>%
  # Rename summary metrics columns
  dplyr::rename(prev_std.dev = std.dev,
                prev_sample.size = sample.size,
                prev_std.error = std.error,
                vals = mean) %>%
  # Group by everything *except* plot
  dplyr::group_by(lter, dataset_simp, dataset, site, block, variables) %>%
  # And get averages (and variation metrics) again
  dplyr::summarize(sample.size = dplyr::n(),
                   mean = ifelse(test = all(sample.size) == 1,
                                 yes = unique(vals),
                                 no = mean(vals, na.rm = T)),
                   std.dev = ifelse(test = all(sample.size) == 1,
                                    yes = unique(prev_std.dev),
                                    no = sd(vals, na.rm = T)),
                   std.error = ifelse(test = all(sample.size) == 1,
                                    yes = unique(prev_std.error),
                                    no = std.dev / sqrt(sample.size)) ) %>%
  # And ungroup
  dplyr::ungroup()

# Check structure
dplyr::glimpse(blocks_v1)

# Check dimension change from that step
dim(plots_v1); dim(blocks_v1)

# Finally, average across blocks within sites
sites_v1 <- blocks_v1 %>%
  # Rename summary metrics columns
  dplyr::rename(prev_std.dev = std.dev, prev_sample.size = sample.size,
                prev_std.error = std.error, vals = mean) %>%
  # Group by everything *except* block
  dplyr::group_by(lter, dataset_simp, dataset, site, variables) %>%
  # And get averages (and variation metrics) again
  dplyr::summarize(sample.size = dplyr::n(),
                   mean = ifelse(test = all(sample.size) == 1,
                                 yes = unique(vals),
                                 no = mean(vals, na.rm = T)),
                   std.dev = ifelse(test = all(sample.size) == 1,
                                    yes = unique(prev_std.dev),
                                    no = sd(vals, na.rm = T)),
                   std.error = ifelse(test = all(sample.size) == 1,
                                      yes = unique(prev_std.error),
                                      no = std.dev / sqrt(sample.size)) ) %>%
  # And ungroup
  dplyr::ungroup()

# Check structure of *that*
dplyr::glimpse(sites_v1)

# Check dimension change from that step
dim(blocks_v1); dim(sites_v1)

# Tweak 'shape' of plot and site averages for viz/stats use
plots_v2 <- plots_v1 %>%
  # Drop sample size column
  dplyr::select(-sample.size) %>%
  # Pivot remaining columns into long format
  tidyr::pivot_longer(cols = mean:std.error,
                      names_to = "stat", values_to = "value") %>%
  # Combine statistic with variable
  dplyr::mutate(name_actual = paste0(stat, "_", variables)) %>%
  # Drop now-superseded columns
  dplyr::select(-stat, -variables) %>%
  # Flip back to wide format
  tidyr::pivot_wider(names_from = name_actual,
                     values_from = value)

# Check structure
glimpse(plots_v2)

# Do the same for the site-level averages
sites_v2 <- sites_v1 %>%
  # Drop sample size column
  dplyr::select(-sample.size) %>%
  # Pivot remaining columns into long format
  tidyr::pivot_longer(cols = mean:std.error,
                      names_to = "stat", values_to = "value") %>%
  # Combine statistic with variable
  dplyr::mutate(name_actual = paste0(stat, "_", variables)) %>%
  # Drop now-superseded columns
  dplyr::select(-stat, -variables) %>%
  # Flip back to wide format
  tidyr::pivot_wider(names_from = name_actual,
                     values_from = value)

# Re-check structure
dplyr::glimpse(sites_v2)

## ------------------------------------------ ##
        # Export Spatial Aggergations ----
## ------------------------------------------ ##

# Create a final data object for both
sparc_site_avgs <- sites_v2
sparc_plot_avgs <- plots_v2

# Check its structure
dplyr::glimpse(sparc_site_avgs)
dplyr::glimpse(sparc_plot_avgs)

# Define the tidy file names
sites_name <- "site-avgs_tidy-soil-p.csv"
plots_name <- "plot-avgs_tidy-soil-p.csv"

# Save out the final data objects
write.csv(x = sparc_site_avgs, file = file.path("tidy_data", sites_name), row.names = F, na = "")
write.csv(x = sparc_plot_avgs, file = file.path("tidy_data", plots_name), row.names = F, na = "")

## ------------------------------------------ ##
          # Google Drive Uploads ----
## ------------------------------------------ ##

# Identify tidy files (other than archival data made by 'harmonize' script)
( ready_files <- setdiff(x = dir(path = file.path("tidy_data")), 
                         y = "sparc-soil-p_archival-data.csv") )

# Loop across these files...
for(file in ready_files){
  
  #...and upload to the Drive
  googledrive::drive_upload(media = file.path("tidy_data", file), 
                            overwrite = T, path = tidy_drive) }

## ------------------------------------------ ##
    # Bonus - Nuanced Depth Subsetting ----
## ------------------------------------------ ##

# We may eventually want a more nuanced depth subset operation
## I figured this out when it seemed like what we wanted first
## and am preserving it here if/when we need it later

# Check structure of pre-depth subset version of the stats object
dplyr::glimpse(stats_v2)

# How many *centimeters* from the first measured depth (of the mineral/A horizon) are allowed?
depth_cutoff <- 15

# Start with the 
stats_bonus <- stats_v2 %>%
  # Group by site-information columns
  dplyr::group_by(dplyr::across(lter:block)) %>%
  # Calculate minimum depth within those columns
  dplyr::mutate(min_depth = ifelse(!all(is.na(depth.start_cm)),
                                   yes = min(depth.start_cm, na.rm = T),
                                   no = NA)) %>%
  # Also calculate the maximum 'allowed' depth using the cutoff defined above
  dplyr::mutate(max_allowed_depth = (min_depth + depth_cutoff)) %>%
  # Ungroup
  dplyr::ungroup() %>%
  # Now filter to only samples between those bookends
  dplyr::filter(depth.start_cm >= min_depth & depth.end_cm <= max_allowed_depth) %>%
  # Drop columns needed for that filter but otherwise not needed
  dplyr::select(-min_depth, -max_allowed_depth)

# How many rows does that drop?
nrow(stats_v2); nrow(stats_bonus)

# More or less data in this subset than the simpler 'depth starting at 0' subset?
nrow(stats_v3); nrow(stats_bonus)
## *Much* less data with this approach

# Re-check structure
dplyr::glimpse(stats_bonus)

# End ----
