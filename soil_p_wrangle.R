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
    order = ifelse((is.na(order) | nchar(order) == 0), 
                   yes = "order", no = order),
    reagent = ifelse(is.na(reagent) | nchar(reagent) == 0, 
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
  message("Following fractions found for dataset '", data_obj, 
          "': ", paste(sub, collapse = "; ")) }

# Now we'll want to add together our various types of P (conditionally)
p_sums_v2 <- p_sums_v1 %>%
  # Calculate slow P conditionally
  dplyr::mutate(slow.P_conc_mg.kg = dplyr::case_when(
    dataset == "Bonanza Creek_1" ~ NA,
    dataset == "Bonanza Creek_2" ~ NA,
    dataset == "Brazil" ~ NA,
    dataset == "Calhoun" ~ (P_conc_mg.kg_4_HCl),
    dataset == "CedarCreek_1" ~ NA,
    dataset == "Coweeta" ~ (P_conc_mg.kg_4_HCl),
    dataset == "HJAndrews_1" ~ (P_conc_mg.kg_4_HCl),
    dataset == "Hubbard Brook" ~ (P_conc_mg.kg_3_HNO3),
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
    dataset == "Toolik_1" ~ ifelse((P_conc_mg.kg_order_HCl - P_conc_mg.kg_order_citrate) < 0,
                                   yes = 0,
                                   no = P_conc_mg.kg_order_HCl - P_conc_mg.kg_order_citrate),
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
  message("Following fractions found for dataset '", data_obj, 
          "': ", paste(sub, collapse = "; ")) }

# Calculate total P next
p_sums_v3 <- p_sums_v2 %>%
  # Do the same for total P
  dplyr::mutate(total.P_conc_mg.kg = dplyr::case_when(
    dataset == "Bonanza Creek_1" ~ (P_conc_mg.kg_total),
    dataset == "Bonanza Creek_2" ~ (P_conc_mg.kg_total),
    dataset == "Brazil" ~ (P_conc_mg.kg_total),
    dataset == "Calhoun" ~ (P_conc_mg.kg_total),
    dataset == "CedarCreek_1" ~ NA,
    dataset == "Coweeta" ~ (P_conc_mg.kg_1_NH4Cl + P_conc_mg.kg_2_HCO3 + P_conc_mg.kg_3_NaOH +
                              P_conc_mg.kg_4_HCl + P_conc_mg.kg_5_residual),
    dataset == "HJAndrews_1" ~ (P_conc_mg.kg_total),
    # (vvv) Both HNO3s should be used (3 is cold, 4 is hot)
    dataset == "Hubbard Brook" ~ (P_conc_mg.kg_1_NH4Cl + P_conc_mg.kg_2_H2O2 +
                                    P_conc_mg.kg_3_HNO3 + P_conc_mg.kg_4_HNO3),
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

# Make a final version of the p_sums object that is as simple as possible
p_sums <- p_sums_v3 %>%
  # Remove all P fraction columns (because we changed real NAs to convenient 0s)
  dplyr::select(-dplyr::starts_with("P_"), -dplyr::starts_with("Po_"), 
                -dplyr::starts_with("Pi_")) %>%
  # Keep only unique rows
  dplyr::distinct()

# Any datasets missing?
p_sums %>%
  dplyr::filter(is.na(slow.P_conc_mg.kg) | is.na(total.P_conc_mg.kg)) %>%
  dplyr::group_by(dataset) %>%
  dplyr::summarize(slow_mean = mean(slow.P_conc_mg.kg, na.rm = T),
                   total_mean = mean(total.P_conc_mg.kg, na.rm = T)) %>%
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
        # Export Full SPARC Data ----
## ------------------------------------------ ##

# Create a final data object
sparc_tidy <- sparc_v4 %>%
  # Simplify dataset names to make plot labels neater
  dplyr::mutate(dataset_simp = gsub(pattern = "Bonanza Creek", replacement = "BNZ", 
                                    x = dataset), .before = dataset) %>%
  dplyr::mutate(dataset_simp = gsub(pattern = "CedarCreek", replacement = "CDR", 
                                    x = dataset_simp)) %>%
  dplyr::mutate(dataset_simp = gsub(pattern = "Coweeta", replacement = "CWT", 
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
  dplyr::select(lter, dataset_simp, dataset, site, plot, block, core,
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
     # Calculate Across Site Averages ----
## ------------------------------------------ ##

# The version of the data with granular (i.e., within dataset) is useful BUT
## there are also hypotheses requiring among-site averages.
## We might as well do that here!

# Prepare for calculation of summary statistics
avgs_v1 <- sparc_stats %>%
  # Drop all information we're not interested in
  dplyr::select(lter, dataset_simp, dataset, dplyr::ends_with(".P_conc_mg.kg"),
                C_conc_percent, N_conc_percent) %>%
  # Pivot to long format
  tidyr::pivot_longer(cols = -lter:-dataset,
                      names_to = "variable",
                      values_to = "values")

# Check the structure of that
dplyr::glimpse(avgs_v1)

# Compute mean / SD / SE
avgs_v2 <- supportR::summary_table(data = avgs_v1, 
                                   groups = c("lter", "dataset_simp", "dataset", "variable"),
                                   response = "values", drop_na = T, round_digits = 6)

# Check that out
dplyr::glimpse(avgs_v2)

# Process that into a better format for graphing / statistics
avgs_v3 <- avgs_v2 %>%
  # Pivot remaining columns into long format
  tidyr::pivot_longer(cols = mean:std_error,
                      names_to = "stat", 
                      values_to = "value") %>%
  # Replace underscores with periods in statistic ID columns
  dplyr::mutate(stat = gsub(pattern = "_", replacement = ".", x = stat)) %>%
  # Combine statistic with variable
  dplyr::mutate(name_actual = paste0(stat, "_", variable)) %>%
  # Drop now-superseded columns
  dplyr::select(-stat, -variable) %>%
  # Flip back to wide format
  tidyr::pivot_wider(names_from = name_actual,
                    values_from = value) %>%
  # Drop any instances where the sample size is 1
  dplyr::filter(dplyr::if_any(.cols = dplyr::starts_with("sample.size_"),
                              .fns = ~ .x != 1)) %>%
  # Now ditch all of the sample size columns
  dplyr::select(-dplyr::starts_with("sample.size_"))

# Re-check structure
dplyr::glimpse(avgs_v3)
## tibble::view(avgs_v3)

## ------------------------------------------ ##
        # Export Across Site Averages ----
## ------------------------------------------ ##

# Create a final data object
sparc_avgs <- avgs_v3

# Check its structure
dplyr::glimpse(sparc_avgs)

# Define the tidy file name
avgs_name <- "site-avgs_tidy-soil-p.csv"

# Save out the final data object
write.csv(x = sparc_avgs, file = file.path("tidy_data", avgs_name), 
          row.names = F, na = "")

## ------------------------------------------ ##
          # Google Drive Uploads ----
## ------------------------------------------ ##

# Centralizing Drive upload steps to make it easier to re-run processing bits iteratively

# Upload tidy wide-format SPARC data
googledrive::drive_upload(media = file.path("tidy_data", tidy_name), 
                          overwrite = T, path = tidy_drive)

# Upload simplifid, stats-ready data
googledrive::drive_upload(media = file.path("tidy_data", stats_name), 
                          overwrite = T, path = tidy_drive)

# Upload site averages
googledrive::drive_upload(media = file.path("tidy_data", avgs_name), 
                          overwrite = T, path = tidy_drive)

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
