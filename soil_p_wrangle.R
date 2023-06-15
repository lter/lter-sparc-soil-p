## ------------------------------------------ ##
      # SPARC Soil P -- Data Wrangling
## ------------------------------------------ ##
# Script author(s): Nick J Lyon

# Purpose:
## Harmonize / wrangle soil phosphorus & nitrogen concentration data
## Ultimately creates a "megadata" file that can be used by downstream analysis/plotting

## ------------------------------------------ ##
              # Housekeeping -----
## ------------------------------------------ ##

# Load necessary libraries
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive, supportR, psych)

# Create necessary sub-folder(s)
dir.create(path = file.path("raw_data"), showWarnings = F)

# Identify raw data files
raw_ids <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/10igyNjNSEJDrz5mUtYyxgbUPDUO7bsuW"), type = "csv")

# Download each data file into the new 'raw_data' folder
purrr::walk2(.x = raw_ids$id, .y = raw_ids$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("raw_data", .y)))

# Clear environment
rm(list = ls())

## ------------------------------------------ ##
            # Data Harmonizing ----
## ------------------------------------------ ##

# Download data key (connects raw column names with synonymized equivalents)
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1WIAo08Jnmp7BdvN8xxobZ_txcFCWZ35w"), pattern = "LTER_P_DataKey") %>%
  googledrive::drive_download(file = ., type = "csv", overwrite = T)

# Retrieve the data key
key_v0 <- read.csv(file = file.path("LTER_P_DataKey.csv"))

# Identify the downloaded raw files
raw_files <- dir(path = file.path("raw_data"))

# Compare the two to see if all file names in the key were in the Drive
supportR::diff_check(old = raw_files, new = unique(key_v0$Raw_Filename))

# Wrangle key object
key <- key_v0 %>%
  # Subset to only files we downloaded
  dplyr::filter(Raw_Filename %in% raw_files) %>%
  # Tweak what the key expects raw file column names to be
  ## Identify what the first character is (some fixes depend on this)
  dplyr::mutate(first_char = stringr::str_sub(Raw_Column_Name, start = 1, end = 1)) %>%
  ## Conditional fixes
  dplyr::mutate(Raw_Column_Name = dplyr::case_when(
    # Leading % becomes X. when reading in CSV
    first_char == "%" ~ paste0("X.", gsub(pattern = "%", replacement = "",
                                          x = Raw_Column_Name)),
    # Leading number becomes X 
    !is.na(suppressWarnings(as.numeric(first_char))) ~ paste0("X", Raw_Column_Name),
    # Some LUQ columns with specific issues
    Dataset == "Luquillo_1" & Raw_Column_Name == "C (%)" ~ "C....",
    Dataset == "Luquillo_1" & Raw_Column_Name == "N (%)" ~ "N....",
    # If conditions not specified, return column unmodified
    TRUE ~ Raw_Column_Name)) %>%
  ## Unconditional fixes
  ## Spaces & parentheses & slashes & hyphens in a CSV column name will be coerced to periods
  dplyr::mutate(Raw_Column_Name = gsub(pattern = " |\\(|\\)|\\/|\\-", replacement = ".", 
                                       x = Raw_Column_Name)) %>%
  ## Percent symbols become Xs
  dplyr::mutate(Raw_Column_Name = gsub(pattern = "\\%", replacement = "X", 
                                       x = Raw_Column_Name)) %>%
  # Drop unwanted column(s)
  dplyr::select(-first_char, -Extraction_Method, -Notes, -X)

# Check structure of key
dplyr::glimpse(key)

# Make an empty list (to store raw data in shortly)
df_list <- list()

# For each raw file...
for(j in 1:length(raw_files)){
  
  # Grab its name
  focal_raw <- raw_files[j]
  
  # Subset the key object a bit
  key_sub <- key %>%
    # Only this file's section
    dplyr::filter(Raw_Filename == focal_raw) %>%
    # And only columns that have a synonymized equivalent
    dplyr::filter(!is.na(Combined_Column_Name) & nchar(Combined_Column_Name) != 0)
  
  # Load in that file
  raw_df_v1 <- read.csv(file = file.path("raw_data", focal_raw))
  
  # Process it to ready for integration with other raw files
  raw_df_v2 <- raw_df_v1 %>%
    # Create a row number column and a column for the original file
    dplyr::mutate(row_num = 1:nrow(.),
                  Raw_Filename = focal_raw,
                  .before = dplyr::everything()) %>%
    # Make all columns into character columns
    dplyr::mutate(dplyr::across(.cols = dplyr::everything(), .fns = as.character)) %>%
    # Now pivot everything into ultimate long format
    ## Note: if column class differs this step can't be done
    ## That is why we convert everything into characters in the previous step
    tidyr::pivot_longer(cols = -row_num:-Raw_Filename,
                        names_to = "Raw_Column_Name",
                        values_to = "values")
  
  # Identify any columns that are in the data key but (apparently) not in the data
  missing_cols <- setdiff(x = key_sub$Raw_Column_Name, y = unique(raw_df_v2$Raw_Column_Name))
  
  # If any are found, print a warning for whoever is running this
  if(length(missing_cols) != 0){
    message("Not all expected columns in '", focal_raw, "' are in data key!")
    message("Check (and fix if needed) raw columns: ", 
            paste0("'", missing_cols, "'", collapse = " & ")) }
  
  # Drop this object (if it exists) to avoid false warning with the next run of the loop
  if(exists("missing_cols") == T){ rm(list = "missing_cols") }
  
  # Integrate synonymized column names from key
  raw_df <- raw_df_v2 %>%
    # Attach revised column names
    dplyr::left_join(key_sub, by = c("Raw_Filename", "Raw_Column_Name")) %>%
    # Drop any columns that don't have a synonymized equivalent
    dplyr::filter(!is.na(Combined_Column_Name)) %>%
    # Pick a standard 'not provided' entry for concentration units
    dplyr::mutate(Concentration_Units = ifelse(nchar(Concentration_Units) == 0,
                                               yes = NA, no = Concentration_Units)) %>%
    # Handle concentration units characters that can't be in column names
    dplyr::mutate(conc_actual = gsub(pattern = "\\/", replacement = "_", 
                                     x = Concentration_Units)) %>%
    # Combine concentration units with column name (where conc units are provided)
    dplyr::mutate(names_fix = ifelse(!is.na(conc_actual),
                                     yes = paste0(Combined_Column_Name, "_", conc_actual),
                                     no = Combined_Column_Name)) %>%
    # If units were already in the column name and the above step duplicate them, handle that
    dplyr::mutate(names_actual = gsub(pattern = "_mg_kg_mg_kg", replacement = "_mg_kg", 
                                      x = names_fix)) %>%
    # Pare down to only needed columns (implicitly removes unspecified columns)
    dplyr::select(row_num, Dataset, Raw_Filename, names_actual, values) %>%
    # Pivot back to wide format with revised column names
    tidyr::pivot_wider(names_from = names_actual, values_from = values, values_fill = NA) %>%
    # Drop row number column
    dplyr::select(-row_num) %>%
    # Drop non-unique rows (there shouldn't be any but better safe than sorry)
    dplyr::distinct()
  
  # Add to list
  df_list[[focal_raw]] <- raw_df
  
  # Success message
  message("Wrangling complete for '", focal_raw, "' (", length(raw_files) - j, " files remaining)") 
  
} # Close loop

# Unlist the list we just generated
tidy_v0 <- df_list %>%
  purrr::list_rbind()

# Process that object a little
tidy_v1 <- tidy_v0 %>%
  # Reorder columns somewhat
  dplyr::select(Dataset, Raw_Filename, site, lat, lon, plot, block,
                core, treatment, depth_cm, dplyr::ends_with("_mg_kg"),
                dplyr::ends_with("_percent")) %>%
  # Group C/N columns together
  dplyr::relocate(C_conc_percent, C_conc_mg_kg, .after = depth_cm) %>%
  dplyr::relocate(N_conc_percent, N_conc_mg_kg, .after = depth_cm)

# Make sure no columns were dropped / added
supportR::diff_check(old = names(tidy_v0), new = names(tidy_v1))

# Glimpse it
dplyr::glimpse(tidy_v1)

# Clean up environment
rm(list = setdiff(ls(), "tidy_v1"))

## ------------------------------------------ ##
    # Data Wrangling - Site Info Checks ----
## ------------------------------------------ ##

# Re-check data structure
dplyr::glimpse(tidy_v1)

# Check for typos in the site/sample information columns
sort(unique(tidy_v1$Dataset))
sort(unique(tidy_v1$Raw_Filename))
sort(unique(tidy_v1$site))
sort(unique(tidy_v1$lat))
sort(unique(tidy_v1$lon))
sort(unique(tidy_v1$plot))
sort(unique(tidy_v1$block))
sort(unique(tidy_v1$core))
sort(unique(tidy_v1$treatment))

# Fix any typos identified above
tidy_v2 <- tidy_v1 %>%
  # Rename the dataset & raw file columns to match naming convention of other columns
  dplyr::rename(dataset = Dataset,
                raw_filename = Raw_Filename)

# Check structure
dplyr::glimpse(tidy_v2)

## ------------------------------------------ ##
        # Data Wrangling - Depth ----
## ------------------------------------------ ##

# Next, we need to handle the depth column
sort(unique(tidy_v2$depth_cm))

# Do wrangling
tidy_v3 <- tidy_v2 %>%
  # Standardize range formatting
  dplyr::mutate(depth_range_cm = gsub(pattern = "_", replacement = "-", x = depth_cm)) %>%
  # Remove any spaces in these values
  dplyr::mutate(depth_range_cm = gsub(pattern = " ", replacement = "", x = depth_range_cm)) %>%
  # Fix some non-ranges
  dplyr::mutate(depth_range_cm = dplyr::case_when(
    dataset == "Coweeta" & depth_range_cm == "10" ~ "10-30", # All other begin at 10 are 10-30
    dataset == "Coweeta" & depth_range_cm == "30+" ~ "30-60", # End of range is a guess
    # dataset == "" &  depth_range_cm == "" ~ "",
    TRUE ~ depth_range_cm)) %>%
  # Now that everything is a range, we can split based on the hyphen
  tidyr::separate_wider_delim(cols = depth_range_cm, delim = "-", cols_remove = F,
                              names = c("depth_start_cm", "depth_end_cm"),
                              too_few = "error", too_many = "error") %>%
  # Make the resulting columns numeric
  dplyr::mutate(depth_start_cm = as.numeric(depth_start_cm),
                depth_end_cm = as.numeric(depth_end_cm)) %>%
  # Calculate the difference in depth (i.e., sampling length regardless of depth)
  dplyr::mutate(core_length_cm = depth_end_cm - depth_start_cm) %>%
  # Relocate the depth columns to the same place
  dplyr::relocate(depth_range_cm, depth_start_cm, depth_end_cm, core_length_cm,
                  .after = treatment) %>%
  # Throw away the original (un-tidied) depth column
  dplyr::select(-depth_cm)

# Take a quick glance at each of the depth columns we just generated
sort(unique(tidy_v3$depth_range_cm))
psych::multi.hist(x = tidy_v3$depth_start_cm)
psych::multi.hist(x = tidy_v3$depth_end_cm)
psych::multi.hist(x = tidy_v3$core_length_cm)

# Re-check structure
dplyr::glimpse(tidy_v3)

## ------------------------------------------ ##
      # Data Wrangling - Bulk Density ----
## ------------------------------------------ ##

# We need soil bulk density to convert 'per sample' values to absolute totals of P/C/N
tidy_v4 <- tidy_v3 %>%
  # We're hard coding bulk density in here rather than typing manually
  ## Citations/justifications are included next to each bulk density value
  dplyr::mutate(bulk_density = dplyr::case_when(
    dataset == "Calhoun" ~ 0.9,
    dataset == "Coweeta" ~ 0.9,
    dataset == "Niwot_Liptzen2006" ~ 0.9,
    dataset == "Sevilletta_Cross1994" ~ 0.9,
    # dataset == "" ~ ,
    # If no bulk density is supplied by above conditions, fill with NA
    TRUE ~ NA), .after = core_length_cm)

# Check whether we're missing any bulk density values
## If so, need to add another conditional to the above `case_when`
tidy_v4 %>%
  dplyr::filter(is.na(bulk_density)) %>%
  dplyr::select(dataset, site, plot, block) %>%
  dplyr::distinct()

# Check structure
dplyr::glimpse(tidy_v4)

## ------------------------------------------ ##
        # Data Wrangling - P Sums ----
## ------------------------------------------ ##

# Now we'll want to add together our various types of P (conditionally)
p_sums <- tidy_v4 %>%
  # First need to fill NAs with 0s to avoid making NA sums
  ## Pivot longer
  tidyr::pivot_longer(cols = -dataset:-C_conc_mg_kg,
                      names_to = "names", values_to = "values") %>%
  ## Fill NA with 0
  dplyr::mutate(values = ifelse(test = is.na(values), yes = 0, no = values)) %>%
  ## Pivot back to wide format
  tidyr::pivot_wider(names_from = names, values_from = values, values_fill = 0) %>%
  # Calculate slow P conditionally
  dplyr::mutate(slow_P_mg_kg = dplyr::case_when(
    dataset == "Calhoun" ~ (HCl_P_mg_kg + ConHCl_Po_mg_kg + ConHCl_Pi_mg_kg),
    dataset == "Coweeta" ~ (HCl_P_mg_kg + ConHCl_Po_mg_kg + ConHCl_Pi_mg_kg),
    dataset == "Niwot_Liptzen2006" ~ (HCl_P_mg_kg + ConHCl_Po_mg_kg + ConHCl_Pi_mg_kg),
    dataset == "Sevilletta_Cross1994" ~ (HCl_P_mg_kg + ConHCl_Po_mg_kg + ConHCl_Pi_mg_kg),
    # dataset == "" ~ (),
    TRUE ~ (HCl_P_mg_kg) )) %>%
  # Also total P
  dplyr::mutate(total_P_mg_kg = dplyr::case_when(
    dataset == "Calhoun" ~ (Resin_P_mg_kg + HCO3_Po_mg_kg + HCO3_Pi_mg_kg +
                              NaOH_Po_mg_kg + NaOH_Pi_mg_kg + HCl_P_mg_kg +
                              ConHCl_Po_mg_kg + ConHCl_Pi_mg_kg + MIII_P_mg_kg +
                              HCO3_P_mg_kg + NaOH_P_mg_kg + Sonic_P_mg_kg + Residual_P_mg_kg +
                              Sonic_Pi_mg_kg + Sonic_Po_mg_kg),
    dataset == "Coweeta" ~ (Resin_P_mg_kg + HCO3_Po_mg_kg + HCO3_Pi_mg_kg +
                              NaOH_Po_mg_kg + NaOH_Pi_mg_kg + HCl_P_mg_kg +
                              ConHCl_Po_mg_kg + ConHCl_Pi_mg_kg + MIII_P_mg_kg +
                              HCO3_P_mg_kg + NaOH_P_mg_kg + Sonic_P_mg_kg + Residual_P_mg_kg +
                              Sonic_Pi_mg_kg + Sonic_Po_mg_kg),
    dataset == "Niwot_Liptzen2006" ~ (Resin_P_mg_kg + HCO3_Po_mg_kg + HCO3_Pi_mg_kg +
                                        NaOH_Po_mg_kg + NaOH_Pi_mg_kg + HCl_P_mg_kg +
                                        ConHCl_Po_mg_kg + ConHCl_Pi_mg_kg + MIII_P_mg_kg +
                                        HCO3_P_mg_kg + NaOH_P_mg_kg + Sonic_P_mg_kg +
                                        Residual_P_mg_kg + Sonic_Pi_mg_kg + Sonic_Po_mg_kg),
    dataset == "Sevilletta_Cross1994" ~ (Resin_P_mg_kg + HCO3_Po_mg_kg + HCO3_Pi_mg_kg +
                                           NaOH_Po_mg_kg + NaOH_Pi_mg_kg + HCl_P_mg_kg +
                                           ConHCl_Po_mg_kg + ConHCl_Pi_mg_kg + MIII_P_mg_kg +
                                           HCO3_P_mg_kg + NaOH_P_mg_kg + Sonic_P_mg_kg +
                                           Residual_P_mg_kg + Sonic_Pi_mg_kg + Sonic_Po_mg_kg),
    # dataset == "" ~ (),
    TRUE ~ (HCl_P_mg_kg) )) %>%
  # After summing, remove all columns where we changed NAs to 0s
  dplyr::select(dataset:C_conc_mg_kg, slow_P_mg_kg, total_P_mg_kg) %>%
  # Keep only unique rows
  dplyr::distinct()

# Check structure
dplyr::glimpse(p_sums)

# Note we're doing this in a separate object because we coerced NAs into 0s for algebra reasons
## They're not "real" 0s so we want to preserve the relevant information

# Now we can attach our sums to the original tidy object
tidy_v5 <- tidy_v4 %>%
  dplyr::left_join(y = p_sums, by = dplyr::join_by(dataset, raw_filename, site, lat, lon, plot,
                                                   block, core, treatment, depth_range_cm,
                                                   depth_start_cm, depth_end_cm, 
                                                   core_length_cm, bulk_density, 
                                                   N_conc_percent, N_conc_mg_kg, 
                                                   C_conc_percent, C_conc_mg_kg)) %>%
  # Move our P sums to the left for more easy reference
  dplyr::relocate(slow_P_mg_kg, total_P_mg_kg, .after = bulk_density)

# Check structure
dplyr::glimpse(tidy_v5)

## ------------------------------------------ ##
      # Data Wrangling - Absolute P ----
## ------------------------------------------ ##

# Calculate absolute P totals (rather than portions of each core)
tidy_v6 <- tidy_v5 %>%
  # Due to our earlier depth/bulk density prep this is easy!
  dplyr::mutate(slow_P_absolute = slow_P_mg_kg * core_length_cm * bulk_density,
                .before = slow_P_mg_kg) %>%
  dplyr::mutate(total_P_absolute = total_P_mg_kg * core_length_cm * bulk_density,
                .before = total_P_mg_kg)
## Units of absolute sums are ____?
  
# Re-check structure
dplyr::glimpse(tidy_v6)

## ------------------------------------------ ##
        # Data Wrangling - N & C ----
## ------------------------------------------ ##

# Convert N & C into percents
tidy_v7 <- tidy_v6 %>%
  # Do conditionally so we don't overwrite anything
  ## Nitrogen first
  dplyr::mutate(N_conc_percent = ifelse(is.na(N_conc_percent) & !is.na(N_conc_mg_kg),
                                        yes = N_conc_mg_kg * 0.0001,
                                        no = N_conc_percent),
                ## Then Carbon
                C_conc_percent = ifelse(is.na(C_conc_percent) & !is.na(C_conc_mg_kg),
                                        yes = C_conc_mg_kg * 0.0001,
                                        no = C_conc_percent)) %>%
  # Drop the mg/kg versions of both chemicals
  dplyr::select(-N_conc_mg_kg, -C_conc_mg_kg)

# Check the number of NAs before/after (for Nitrogen)
summary(tidy_v6$N_conc_percent)
summary(tidy_v7$N_conc_percent)

# Do the same check for Carbon
summary(tidy_v6$C_conc_percent)
summary(tidy_v7$C_conc_percent)

# Check data structure again
dplyr::glimpse(tidy_v7)

## ------------------------------------------ ##
# Export ----
## ------------------------------------------ ##

# Create a final data object
final_tidy <- tidy_v7

# Check its structure
dplyr::glimpse(final_tidy)

# Save out the final data object
write.csv(x = final_tidy, file = "tidy_soil_p.csv", row.names = F, na = "")

# Upload to GoogleDrive
googledrive::drive_upload(media = "tidy_soil_p.csv", overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1pjgN-wRlec65NDLBvryibifyx6k9Iqy9"))

# End ----
