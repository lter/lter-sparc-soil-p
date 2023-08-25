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
downloaded_files <- dir(path = file.path("raw_data"))

# Compare the two to see if all file names in the key were in the Drive
supportR::diff_check(old = downloaded_files, new = unique(key_v0$Raw_Filename))

# Wrangle key object
key <- key_v0 %>%
  # Subset to only files we downloaded
  dplyr::filter(Raw_Filename %in% downloaded_files) %>%
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
  dplyr::mutate(Raw_Column_Name = gsub(pattern = " |\\(|\\)|\\/|\\-|\\+|\\:|,", replacement = ".", 
                                       x = Raw_Column_Name)) %>%
  ## Percent symbols become Xs
  dplyr::mutate(Raw_Column_Name = gsub(pattern = "\\%", replacement = "X", 
                                       x = Raw_Column_Name)) %>%
  # Make sure all synonymized column names avoid characters that are bad in column names
  dplyr::mutate(Combined_Column_Name = gsub(pattern = " |\\(|\\)|\\/|\\-|\\+|\\:|,", 
                                            replacement = "_", x = Combined_Column_Name)) %>%
  # Drop unwanted column(s)
  dplyr::select(-first_char, -Extraction_Method, -Notes, -X)

# Check structure of key
dplyr::glimpse(key)

# Check whether any raw column names are duplicate within any data file
key %>% 
  dplyr::group_by(Raw_Filename, Raw_Column_Name) %>%
  dplyr::summarize(ct = dplyr::n()) %>%
  dplyr::filter(ct > 1)

# Subset the downloaded files to only those in the data key
raw_files <- intersect(downloaded_files, key$Raw_Filename)

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
    dplyr::filter(!is.na(Combined_Column_Name) & nchar(Combined_Column_Name) != 0) %>%
    dplyr::filter(Combined_Column_Name != "NA") 
  
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
  raw_df_v3 <- raw_df_v2 %>%
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
    dplyr::mutate(names_actual = gsub(pattern = "_g_m2_g_m2", replacement = "_g_m2", 
                                      x = names_actual)) %>%
    dplyr::mutate(names_actual = gsub(pattern = "_ppm_ppm", replacement = "_ppm", 
                                      x = names_actual)) %>%
    dplyr::mutate(names_actual = gsub(pattern = "_cm_cm", replacement = "_cm", 
                                      x = names_actual)) %>%
    # Pare down to only needed columns (implicitly removes unspecified columns)
    dplyr::select(row_num, Dataset, Raw_Filename, names_actual, values)
  
  # As a separate object (for ease of maintenance we want the preceding work in its own object)
  raw_df <- raw_df_v3 %>%
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
tidy_v1 <- purrr::list_rbind(df_list)

# Check that out
dplyr::glimpse(tidy_v1)

# Clean up environment
rm(list = setdiff(ls(), "tidy_v1"))

## ------------------------------------------ ##
             # Site Info Checks ----
## ------------------------------------------ ##

# Re-check data structure
dplyr::glimpse(tidy_v1)

# Check for typos in the site/sample information columns
sort(unique(tidy_v1$Dataset))
sort(unique(tidy_v1$Raw_Filename))
sort(unique(tidy_v1$LTER))
sort(unique(tidy_v1$site))
sort(unique(tidy_v1$lat))
sort(unique(tidy_v1$lon))
sort(unique(tidy_v1$plot))
sort(unique(tidy_v1$block))
sort(unique(tidy_v1$core))
sort(unique(tidy_v1$treatment))
sort(unique(tidy_v1$treatment_years))
sort(unique(tidy_v1$distance))
sort(unique(tidy_v1$topography))

# Fix any typos identified above
tidy_v2 <- tidy_v1 %>%
  # Fix some of the spatial/site columns
  dplyr::mutate(lat = as.numeric(lat),
                lon = as.numeric(lon),
                distance = as.numeric(distance),
                treatment_years = as.numeric(treatment_years),
                topography = tolower(topography)) %>%
  # Rename columns so that everything is in snake case except element abbreviations
  ## snake case = "lower_lower_lower"
  dplyr::rename(lter = LTER,
                dataset = Dataset,
                raw_filename = Raw_Filename,
                available_P_ppm = Avail_P_ppm,
                coarse_vol_percent = Coarse_Vol_percent,
                tot_P_kg_ha_0_10 = `0_10_tot_P_kg_ha`) %>%
  # Relocate all spatial/site columns to the left of the dataframe
  dplyr::relocate(lter, dataset, raw_filename, site, lat, lon, plot, block, core,
                  sample_replicate, treatment, treatment_years, distance, topography, 
                  horizon, depth_cm, org_depth_cm, pH,
                  .before = dplyr::everything()) %>%
  # Create a better version of the LTER column
  dplyr::mutate(lter = dplyr::coalesce(lter, dataset)) %>%
  dplyr::mutate(lter = dplyr::case_when(
    raw_filename == "HJAndrews_Spears.et.al_2000.csv" ~ "AND",
    lter %in% c("Bonanza Creek_1", "Bonanza Creek_2") ~ "BNZ",
    lter %in% c("Cedar Creek ") ~ "CDR",
    lter %in% c("Chichaqua Bottoms ") ~ "Chichaqua Bottoms",
    lter %in% c("Coweeta") ~ "CWT",
    lter %in% c("FloridaCoastal") ~ "FCE",
    lter %in% c("Hubbard Brook") ~ "HBR",
    lter %in% c("Jornada") ~ "JRN",
    lter %in% c("Konza_1", "Konza_2") ~ "KNZ",
    lter %in% c("Luquillo_1", "Luquillo_2") ~ "LUQ",
    lter %in% c("Niwot_1", "Niwot_2", "Niwot_3", "Niwot_4") ~ "NWT",
    lter %in% c("Sevilleta_1", "Sevilleta_2") ~ "SEV",
    lter %in% c("Toolik") ~ "ARC",
    # Non-LTER sites
    lter %in% c("Brazil") ~ "Brazil",
    lter %in% c("Calhoun") ~ "Calhoun",
    lter %in% c("Fernow") ~ "Fernow",
    # Otherwise retain whatever was in that column originally
    T ~ lter))

# Check out new LTER column
sort(unique(tidy_v2$lter))

# Check structure
dplyr::glimpse(tidy_v2)

## ------------------------------------------ ##
          # Depth & Horizon Fixes ----
## ------------------------------------------ ##

# Next we need to wrangle depth and horizon information
## Need to do concurrently because each column can help inform the other

# Check horizon column and depth column
sort(unique(tidy_v2$horizon))
sort(unique(tidy_v2$depth_cm))

# Do easy unconditional wrangling steps
tidy_v2b <- tidy_v2 %>%
  # Tidy and rename the starting horizon column
  dplyr::mutate(horizon_raw = dplyr::case_when(
    tolower(horizon) %in% c("upper organic") ~ "upper organic",
    tolower(horizon) %in% c("lower organic") ~ "lower organic",
    tolower(horizon) %in% c("mineral") ~ "mineral",
    T ~ horizon), .after = horizon) %>%
  # Standardize range formatting
  dplyr::mutate(depth_raw = gsub(pattern = "_", replacement = "-", x = depth_cm)) %>%
  # Remove any spaces in these values
  dplyr::mutate(depth_raw = gsub(pattern = " ", replacement = "", x = depth_raw)) %>%
  # Create a version of the depth column that only includes any embedded horizon info
  dplyr::mutate(depth_horizon = gsub(pattern = "0|1|2|3|4|5|6|7|8|9|_|-|\\+| |\\.|Apr|Aug|Dec|hurricane_sediment|Jul|Jun|Mar|May|Nov|Oct|Sep",
                                     replacement = "", x = depth_cm)) %>%
  # Drop original columns
  dplyr::select(-horizon, -depth_cm)
  
# Check structure of this
tidy_v2b %>%
  dplyr::select(lter:core, dplyr::contains("horizon"), dplyr::contains("depth")) %>%
  dplyr::select(-dplyr::ends_with("_by_depth")) %>%
  dplyr::glimpse()

# Check for depth values that *aren't* ranges (i.e., no hyphens)
tidy_v2b %>%
  dplyr::filter(stringr::str_detect(string = depth_raw, pattern = "-") != T) %>%
  dplyr::select(dataset, raw_filename, depth_raw) %>%
  dplyr::distinct()

# Also need to identify values that have more than one hyphen
tidy_v2b %>%
  dplyr::filter(stringr::str_count(string = depth_raw, pattern = "-") > 1) %>%
  dplyr::select(dataset, raw_filename, depth_raw) %>%
  dplyr::distinct()

# Wrangle depth into actual numbers
tidy_v2c <- tidy_v2b %>%
  dplyr::mutate(depth_range_raw = dplyr::case_when(
    ## Bonanza (1)
    dataset == "Bonanza Creek_1" & depth_raw == "24" ~ "24-40",
    dataset == "Bonanza Creek_1" & depth_raw == "36" ~ "36-50",
    dataset == "Bonanza Creek_1" ~ gsub(pattern = "\\+", replacement = "", x = depth_raw),
    ## Bonanza (2)
    ### Starting depth listed in separate column
    dataset == "Bonanza Creek_2" ~ paste0(org_depth_cm, "-", depth_raw),
    ## Brazil
    dataset == "Brazil" & depth_raw == "0--10" ~ "0-10",
    dataset == "Brazil" & depth_raw == "10--30" ~ "10-30",
    ## Coweeta
    dataset == "Coweeta" & depth_raw == "10" ~ "10-30", # All other begin at 10 are 10-30
    dataset == "Coweeta" & depth_raw == "30+" ~ "30-60", # End of range is a guess
    ## Florida
    dataset == "FloridaCoastal" & depth_raw == "hurricane-sediment" ~ "",
    ## Hubbard Brook
    dataset == "Hubbard Brook" & depth_raw == "30+" ~ "30-40",
    dataset == "Hubbard Brook" & depth_raw == "C25+" ~ "25-35",
    dataset == "Hubbard Brook" & depth_raw == "C50+" ~ "50-75",
    dataset == "Hubbard Brook" & depth_raw == "C+" ~ "", # guess needed
    dataset == "Hubbard Brook" & depth_raw == "Oa" ~ "", # guess needed
    dataset == "Hubbard Brook" & depth_raw == "50-C" ~ "", # guess needed
    dataset == "Hubbard Brook" & depth_raw == "C0-25" ~ "", # guess needed
    dataset == "Hubbard Brook" & depth_raw == "C25+" ~ "", # guess needed
    dataset == "Hubbard Brook" & depth_raw == "C25-50" ~ "", # guess needed
    dataset == "Hubbard Brook" & depth_raw == "C50+" ~ "", # guess needed
    ## Jornada
    dataset == "Jornada" & depth_raw == "5" ~ "0-5",
    dataset == "Jornada" & depth_raw == "15" ~ "5-15",
    ## Konza
    dataset == "Konza_1" & depth_raw == "81+" ~ "81-91", # guessing all KNZ are 10 cm cores
    dataset == "Konza_1" & depth_raw == "192+" ~ "192-202", 
    dataset == "Konza_1" & depth_raw == "198+" ~ "198-208", 
    dataset == "Konza_1" & depth_raw == "221+" ~ "221-231",
    ## Luquillo (1)
    ### No ranges so we'll just add a constant to every depth value to get the end of the range
    dataset == "Luquillo_1" & stringr::str_detect(string = depth_raw, pattern = "-") != T ~ paste0(depth_raw, "-", suppressWarnings(as.numeric(depth_raw)) + 10),
    ## Luquillo (2)
    dataset == "Luquillo_2" & depth_raw == "1" ~ "", # guess needed
    dataset == "Luquillo_2" & depth_raw == "2" ~ "", # guess needed
    ## Niwot (3)
    dataset == "Niwot_3" & depth_raw == "10" ~ "0-10", # guess needed
    dataset == "Niwot_3" & depth_raw == "20" ~ "10-20", # guess needed
    ## Seviletta (2)
    dataset == "Sevilleta_2" & depth_raw == "10" ~ "0-10", # guess needed
    dataset == "Sevilleta_2" & depth_raw == "20" ~ "10-20", # guess needed
    dataset == "Sevilleta_2" & depth_raw == "30" ~ "20-30", # guess needed
    ## Otherwise raw depth assumed to be a functioning range
    TRUE ~ depth_raw), .after = depth_raw) %>%
  # Drop now-unneeded BNZ depth column
  dplyr::select(-org_depth_cm) %>%
  # If depth included horizon information we probably can assume that it was *relative* depth
  dplyr::mutate(depth_type = dplyr::case_when(
    # Relative depth within horizon layer
    dataset == "Hubbard Brook" & depth_raw %in% c("30+", "C+", "C25+", "C50+", "Oa",
                                                  "50-C", "C0-25", "C25-50") ~ "relative",
    # No depth info means no depth type
    is.na(depth_raw) | nchar(depth_raw) == 0 ~ NA,
    # Otherwise depth info is assumed to be objective depth
    T ~ "objective"), .after = depth_range_raw)

# Re-check for malformed depth ranges
tidy_v2c %>%
  dplyr::filter(stringr::str_detect(string = depth_range_raw, pattern = "-") != T) %>%
  dplyr::select(dataset, raw_filename, depth_raw, depth_range_raw, horizon_raw, depth_type) %>%
  dplyr::distinct()

# And too many hyphens
tidy_v2c %>%
  dplyr::filter(stringr::str_count(string = depth_range_raw, pattern = "-") > 1) %>%
  dplyr::select(dataset, raw_filename, depth_raw, depth_range_raw, horizon_raw, depth_type) %>%
  dplyr::distinct()

# Check structure again
tidy_v2c %>%
  dplyr::select(dataset:core, dplyr::contains("horizon"), dplyr::contains("depth")) %>%
  dplyr::select(-dplyr::ends_with("_by_depth")) %>%
  dplyr::glimpse()

# Separate the semi-tidied depth range into a start and end
tidy_v2d <- tidy_v2c %>%
  # Now that everything is a range, we can split based on the hyphen
  tidyr::separate_wider_delim(cols = depth_range_raw, delim = "-", cols_remove = F,
                              names = c("depth_1", "depth_2"),
                              too_few = "align_start", too_many = "error") %>%
  # Some ranges are converted by Excel into dates automatically upon entry so we need to fix that
  dplyr::mutate(
    depth_1 = dplyr::case_when(
      depth_1 == "Jan" ~ "1", depth_1 == "Feb" ~ "2", depth_1 == "Mar" ~ "3",
      depth_1 == "Apr" ~ "4", depth_1 == "May" ~ "5", depth_1 == "Jun" ~ "6",
      depth_1 == "Jul" ~ "7", depth_1 == "Aug" ~ "8", depth_1 == "Sep" ~ "9",
      depth_1 == "Oct" ~ "10", depth_1 == "Nov" ~ "11", depth_1 == "Dec" ~ "12",
      TRUE ~ depth_1),
    depth_2 = dplyr::case_when(
      depth_2 == "Jan" ~ "1", depth_2 == "Feb" ~ "2", depth_2 == "Mar" ~ "3",
      depth_2 == "Apr" ~ "4", depth_2 == "May" ~ "5", depth_2 == "Jun" ~ "6",
      depth_2 == "Jul" ~ "7", depth_2 == "Aug" ~ "8", depth_2 == "Sep" ~ "9",
      depth_2 == "Oct" ~ "10", depth_2 == "Nov" ~ "11", depth_2 == "Dec" ~ "12",
      TRUE ~ depth_2))

# Check for non-numbers
supportR::multi_num_check(data = tidy_v2d, col_vec = c("depth_1", "depth_2"))

# Now can do numeric wrangling of depth columns
tidy_v2e <- tidy_v2d %>%
  # Do any needed fixes of non-numbers
  ## None needed currently
  # Make the depth columns numeric
  dplyr::mutate(depth_1 = as.numeric(depth_1),
                depth_2 = as.numeric(depth_2)) %>%
  # Now that all depths are numbers we can figure out start and end depths
  dplyr::mutate(depth_start_cm = ifelse(depth_1 < depth_2, yes = depth_1, no = depth_2),
                depth_end_cm = ifelse(depth_2 > depth_1, yes = depth_2, no = depth_1),
                .after = depth_type) %>%
  # Drop intermediary columns and old raw depth columns
  dplyr::select(-depth_1, -depth_2, -depth_raw, -depth_range_raw) %>%
  # Calculate length of core as well
  dplyr::mutate(core_length_cm = depth_end_cm - depth_start_cm,
                .after = depth_end_cm) %>%
  # Move these columns to the left
  dplyr::relocate(depth_type, depth_start_cm, depth_end_cm, core_length_cm, .after = horizon_raw)

# Check distribution of the new depth columns we just extracted
psych::multi.hist(x = tidy_v2e$depth_start_cm)
psych::multi.hist(x = tidy_v2e$depth_end_cm)
psych::multi.hist(x = tidy_v2e$core_length_cm)

# Check structure yet again
tidy_v2e %>%
  dplyr::select(dataset:core, dplyr::contains("horizon"), dplyr::contains("depth")) %>%
  dplyr::select(-dplyr::ends_with("_by_depth")) %>%
  dplyr::glimpse()

# Wrangle horizon information to get other desired facets of that variable
tidy_v3 <- tidy_v2e %>%
  # Make a column that simplifies horizon information
  dplyr::mutate(horizon_simp = dplyr::case_when(
    !horizon_raw %in% c("upper organic", "lower organic",
                        "mineral", "T") ~ stringr::str_sub(string = horizon_raw, 
                                                           start = 1, end = 1),
    horizon_raw %in% c("upper organic", "lower organic") ~ "O",
    dataset == "Toolik" & horizon_raw == "mineral" ~ "A", # Note judgement call
    T ~ horizon_raw)) %>%
  # Create a column that uses entered horizon, depth horizon info, and expert knowledge to increase coverage
  dplyr::mutate(horizon_actual = dplyr::case_when(
    # If horizon is in data, use that
    !is.na(horizon_raw) & nchar(horizon_raw) != 0 ~ horizon_simp,
    !is.na(depth_horizon) & nchar(depth_horizon) != 0 ~ depth_horizon,
    # If not in data, use expert knowledge to fill conditionally
    # dataset == "Bonanza Creek_1" ~ "",
    # dataset == "Bonanza Creek_2" ~ "",
    # dataset == "Brazil" ~ "",
    # dataset == "Calhoun" ~ "",
    # dataset == "Coweeta" ~ "",
    # dataset == "Jornada" ~ "",
    # dataset == "Kellog_Biological_Station" ~ "",
    # dataset == "Luquillo_1" ~ "",
    # dataset == "Luquillo_2" ~ "",
    # dataset == "Niwot_1" ~ "",
    # dataset == "Niwot_2" ~ "",
    # dataset == "Niwot_3" ~ "",
    # dataset == "Sevilleta_1" ~ "",
    # dataset == "Sevilleta_2" ~ "",
    # If not in data and not known, fill with NA
    T ~ NA), .after = horizon_raw) %>%
  # Identify the source of this information
  dplyr::mutate(horizon_source = dplyr::case_when(
    # If in data say that
    !is.na(horizon_raw) & nchar(horizon_raw) != 0 ~ "in data",
    !is.na(depth_horizon) & nchar(depth_horizon) != 0 ~ "in data",
    # If filled conditionally, enter that 
    # dataset == "Bonanza Creek_1" ~ "expert knowledge",
    # dataset == "Bonanza Creek_2" ~ "expert knowledge",
    # dataset == "Brazil" ~ "expert knowledge",
    # dataset == "Calhoun" ~ "expert knowledge",
    # dataset == "Coweeta" ~ "expert knowledge",
    # dataset == "Jornada" ~ "expert knowledge",
    # dataset == "Kellog_Biological_Station" ~ "expert knowledge",
    # dataset == "Luquillo_1" ~ "expert knowledge",
    # dataset == "Luquillo_2" ~ "expert knowledge",
    # dataset == "Niwot_1" ~ "expert knowledge",
    # dataset == "Niwot_2" ~ "expert knowledge",
    # dataset == "Niwot_3" ~ "expert knowledge",
    # dataset == "Sevilleta_1" ~ "expert knowledge",
    # dataset == "Sevilleta_2" ~ "expert knowledge",
    # If no horizon information in this column, the source is NA
    is.na(horizon_actual) ~ NA,
    # Otherwise fill with NA
    T ~ NA), .after = horizon_actual) %>%
  # Create a 'mineral vs. organic' horizon column
  dplyr::mutate(horizon_binary = dplyr::case_when(
    horizon_actual %in% c("organic", "O", "Oi", "Oe", "Oa") ~ "organic",
    horizon_actual %in% c("mineral", "A", "B", "C", "AEB") ~ "mineral",
    horizon_actual == "T" ~ "hurricane",
    T ~ NA), .after = horizon_source) %>%
  # Drop depth horizon column and original (un-tidied) horizon column
  dplyr::select(-depth_horizon, -horizon_simp) %>%
  # Rename tidied horizon column
  dplyr::rename(horizon = horizon_actual)

# For which datasets is horizon info *absent* (that could be filled by expert knowledge)?
tidy_v3 %>% 
  dplyr::filter(is.na(horizon) | nchar(horizon) == 0) %>%
  dplyr::select(dataset, raw_filename, horizon) %>%
  dplyr::distinct()

# Check contents of the specific horizon columns
sort(unique(tidy_v3$horizon_raw))
sort(unique(tidy_v3$horizon))
sort(unique(tidy_v3$horizon_source))
sort(unique(tidy_v3$horizon_binary))

# Ad nauseam at this point but check structure
tidy_v3 %>%
  dplyr::select(dataset:core, dplyr::contains("horizon"), dplyr::contains("depth"), core_length_cm) %>%
  dplyr::select(-dplyr::ends_with("_by_depth")) %>%
  dplyr::glimpse()

# Check again for column order
dplyr::glimpse(tidy_v3[1:21])

## ------------------------------------------ ##
          # Numeric Column Checks ----
## ------------------------------------------ ##

# Before we can continue, we need to make all columns that should be numeric actually be numeric
## Can also handle the 'sample replicate' rows once our response values are numbers

# Reshape into long format to make a single column to check for non-numbers
tidy_v3b <- tidy_v3 %>%
  tidyr::pivot_longer(cols = -lter:-core_length_cm,
                      names_to = "variable",
                      values_to = "value_raw") %>%
  # Ditch empty rows too
  dplyr::filter(!is.na(value_raw))

# Check for non-numbers in the response value column
supportR::num_check(data = tidy_v3b, col = "value_raw")

# Resolve all non-numbers
tidy_v3c <- tidy_v3b %>%
  # Remove % symbols
  dplyr::mutate(value_clean = gsub(pattern = "\\%", replacement = "", x = value_raw)) %>%
  # Conditionally handle remaining issues
  dplyr::mutate(value_actual = dplyr::case_when(
    ## Missing values should be NA
    value_clean %in% c("M", ".", "NaN", "NA000") ~ NA,
    nchar(value_clean) == 0 ~ NA,
    ## Handle 'less than' indications (note judgement call)
    value_clean == "< 0.5" ~ "0.25",
    ## Otherwise keep the value as it is
    T ~ value_clean))

# Make sure all numbers are 'good' numbers
supportR::num_check(data = tidy_v3c, col = "value_actual")

# Identify names of all columns except for sample replicate / old values columns
(keeps <- setdiff(x = names(tidy_v3c), y = c("sample_replicate", "value_raw", 
                                            "value_clean", "value_actual")))

# Finish wrangling this object!
tidy_v3d <- tidy_v3c %>%
  # Make the value column numeric
  dplyr::mutate(values = as.numeric(value_actual)) %>%
  # Drop intermediary value columns
  dplyr::select(-value_raw, -value_clean, -value_actual) %>%
  # Group by all keep columns and average across values (removes sample replicate column)
  dplyr::group_by(dplyr::across(dplyr::all_of(keeps))) %>%
  dplyr::summarize(value = mean(values, na.rm = T)) %>%
  dplyr::ungroup()

# How many rows were lost (i.e., how much replication at finer scale than of interest for us)?
nrow(tidy_v3c) - nrow(tidy_v3d)

# Glimpse structure
dplyr::glimpse(tidy_v3d)

# Final processing of this object
tidy_v4 <- tidy_v3d %>%
  # Drop any empty rows (created by cleaning up non-numbers)
  dplyr::filter(nchar(value) != 0 & !is.na(value)) %>%
  # Pivot back to wide format
  tidyr::pivot_wider(names_from = variable, values_from = value) %>%
  # Relocate pH column
  dplyr::relocate(pH, .after = core_length_cm) %>%
  # Group C/N columns together
  dplyr::relocate(dplyr::starts_with("C_conc"), .after = pH) %>%
  dplyr::relocate(dplyr::starts_with("N_conc"), .after = pH)

# Re-check structure
dplyr::glimpse(tidy_v4)

# Check to see if any columns were lost/gained (should only be 'sample_replicate' lost)
supportR::diff_check(old = names(tidy_v3), new = names(tidy_v4))

## ------------------------------------------ ##
        # Bulk Density / Soil Fixes ----
## ------------------------------------------ ##

# Wrangling for soil information
tidy_v5 <- tidy_v4 %>%
  # Relocate soil columns to the left
  dplyr::relocate(dplyr::contains("bulk_density"), coarse_vol_percent, dplyr::contains("soil"),
                  .after = pH) %>%
  # Average across the two bulk density columns in the same units
  dplyr::mutate(bulk_dens_avg_kg_ha = dplyr::case_when(
    !is.na(bulk_density_kg_ha) & !is.na(bulk_density_value2_kg_ha) ~ (bulk_density_kg_ha + bulk_density_value2_kg_ha) / 2,
    !is.na(bulk_density_kg_ha) & is.na(bulk_density_value2_kg_ha) ~ bulk_density_kg_ha,
    is.na(bulk_density_kg_ha) & !is.na(bulk_density_value2_kg_ha) ~ bulk_density_value2_kg_ha,
    T ~ NA), .after = bulk_density_g_cm3) %>%
  # Drop those now-superseded columns
  dplyr::select(-bulk_density_kg_ha, -bulk_density_value2_kg_ha) %>%
  # Rename the bulk density columns
  dplyr::rename(bulk_density_kg_ha = bulk_dens_avg_kg_ha,
                bulk_density_g_cm3_raw = bulk_density_g_cm3) %>%
  #  We're hard coding bulk density in here rather than typing manually
  ## Citations/justifications are included next to each bulk density value
  dplyr::mutate(bulk_density = dplyr::case_when(
  # If bulk density was provided, use that instead of doing conditionals
    !is.na(bulk_density_g_cm3_raw) & 
      nchar(bulk_density_g_cm3_raw) != 0 ~ as.numeric(bulk_density_g_cm3_raw),
    dataset == "HJAndrews_1" ~ 0.9,
    dataset == "Bonanza Creek_1" ~ 0.9,
    dataset == "Bonanza Creek_2" ~ 0.9,
    dataset == "Brazil" ~ 0.9,
    dataset == "Calhoun" ~ 0.9,
    dataset == "CedarCreek_1" ~ 0.9,
    dataset == "Coweeta" ~ 0.9,
    dataset == "Fernow" ~ 0.9,
    dataset == "FloridaCoastal" ~ 0.9,
    dataset == "Hubbard Brook" ~ 0.9,
    dataset == "Jornada" ~ 0.9,
    dataset == "Kellog_Biological_Station" ~ 0.9,
    dataset == "Konza_1" ~ 0.9,
    dataset == "Luquillo_1" ~ 0.9,
    dataset == "Luquillo_2" ~ 0.9,
    dataset == "Niwot_1" ~ 0.9,
    dataset == "Niwot_2" ~ 0.9,
    dataset == "Niwot_3" ~ 0.9,
    dataset == "Niwot_Liptzen2006" ~ 0.9,
    dataset == "Sevilleta_1" ~ 0.9,
    dataset == "Sevilleta_2" ~ 0.9,
    dataset == "Toolik" ~ 0.9,
    # If no bulk density is supplied by above conditions, fill with NA
    TRUE ~ NA), .after = bulk_density_g_cm3_raw) %>%
  # Drop old column and rename remaining one to avoid confusion
  dplyr::select(-bulk_density_g_cm3_raw) %>%
  dplyr::rename(bulk_density_g_cm3 = bulk_density)

# Check whether we're missing any bulk density values
## If so, need to add another conditional to the above `case_when`
tidy_v5 %>%
  dplyr::filter(is.na(bulk_density_g_cm3)) %>%
  # dplyr::select(dataset, site, plot, block) %>%
  dplyr::select(dataset) %>%
  dplyr::distinct()

# Check structure
dplyr::glimpse(tidy_v5[1:30])

## ------------------------------------------ ##
            # Nitrogen & Carbon ----
## ------------------------------------------ ##

# Look at the most relevant bit for N/C tidying
tidy_v5 %>%
  dplyr::select(dataset, dplyr::starts_with("N_"), dplyr::starts_with("C_")) %>%
  dplyr::glimpse()

# Convert N & C into percents
tidy_v5b <- tidy_v5 %>%
  # Standardize Nitrogen concentration units (conditions are from most to least preferred unit)
  dplyr::mutate(N_conc_actual = dplyr::case_when(
    !is.na(N_conc_percent) ~ N_conc_percent,
    is.na(N_conc_percent) & !is.na(N_conc_mg_kg) ~ N_conc_mg_kg * 0.0001,
    is.na(N_conc_percent) & is.na(N_conc_mg_kg) &
      !is.na(N_conc_mg_g) ~ (N_conc_mg_g * 10^3) * 0.0001,
    is.na(N_conc_percent) & is.na(N_conc_mg_kg) &
      is.na(N_conc_mg_g) & !is.na(N_conc_ug_g) ~ (N_conc_ug_g * 10^6) * 0.0001,
    is.na(N_conc_percent) & is.na(N_conc_mg_kg) &
      is.na(N_conc_mg_g) & is.na(N_conc_ug_g) &
      !is.na(N_conc_g_kg) ~ (N_conc_g_kg / 10^3) * 0.0001,
    is.na(N_conc_percent) & is.na(N_conc_mg_kg) &
      is.na(N_conc_mg_g) & is.na(N_conc_ug_g) &
      is.na(N_conc_g_kg) & !is.na(mean_N_percent) ~ mean_N_percent,
    # If nothing exists, fill with NA
    TRUE ~ NA), .before = N_conc_percent) %>%
  # Do the same for Carbon
  dplyr::mutate(C_conc_actual = dplyr::case_when(
    !is.na(C_conc_percent) ~ C_conc_percent,
    is.na(C_conc_percent) & !is.na(C_conc_mg_kg) ~ C_conc_mg_kg * 0.0001,
    is.na(C_conc_percent) & is.na(C_conc_mg_kg) &
      !is.na(C_conc_mg_g) ~ (C_conc_mg_g * 10^3) * 0.0001,
    is.na(C_conc_percent) & is.na(C_conc_mg_kg) &
      is.na(C_conc_mg_g) & !is.na(C_conc_ug_g) ~ (C_conc_ug_g * 10^6) * 0.0001,
    is.na(C_conc_percent) & is.na(C_conc_mg_kg) &
      is.na(C_conc_mg_g) & is.na(C_conc_ug_g) &
      !is.na(C_conc_g_kg) ~ (C_conc_g_kg / 10^3) * 0.0001,
    is.na(C_conc_percent) & is.na(C_conc_mg_kg) &
      is.na(C_conc_mg_g) & is.na(C_conc_ug_g) &
      is.na(C_conc_g_kg) & !is.na(mean_C_percent) ~ mean_C_percent,
    TRUE ~ NA), .before = C_conc_percent) %>%
  # Drop now-superseded columns
  dplyr::select(-N_conc_percent, -N_conc_mg_kg, -N_conc_mg_g, 
                -N_conc_ug_g, -N_conc_g_kg, -mean_N_percent,
                -C_conc_percent, -C_conc_mg_kg, -C_conc_mg_g, 
                -C_conc_ug_g, -C_conc_g_kg, -mean_C_percent) %>%
  # Rename combined columns for clarity and to maintain snake_case
  dplyr::rename(N_conc_percent = N_conc_actual,
                C_conc_percent = C_conc_actual,
                C_conc_inorg_percent = C_conc_Inorg_percent,
                C_conc_org_percent = C_conc_Org_percent)

# How many NAs did we fill for Nitrogran?
summary(tidy_v5$N_conc_percent); summary(tidy_v5b$N_conc_percent)

# Check Carbon in the same way
summary(tidy_v5$C_conc_percent); summary(tidy_v5b$C_conc_percent)

# Check remaining columns
tidy_v5b %>%
  dplyr::select(dataset, dplyr::starts_with("N_"), dplyr::starts_with("C_")) %>%
  dplyr::glimpse()

# Now let's handle different units for stock
tidy_v6 <- tidy_v5b %>%
  # Convert Nitrogen stock into one unit (mg/m2)
  dplyr::mutate(N_stock_actual = dplyr::case_when(
    !is.na(N_stock_mg_m2) ~ N_stock_mg_m2,
    is.na(N_stock_mg_m2) & !is.na(N_stock_g_m2) ~ (N_stock_g_m2 / 10^3),
    is.na(N_stock_mg_m2) & is.na(N_stock_g_m2) &
      !is.na(N_kg_ha) ~ (N_kg_ha * 100),
    T ~ NA), .after = N_conc_percent) %>%
  # Convert Carbon stocks too
  dplyr::mutate(C_stock_actual = dplyr::case_when(
    !is.na(C_stock_mg_m2) ~ C_stock_mg_m2,
    is.na(C_stock_mg_m2) & !is.na(C_stock_g_m2) ~ (C_stock_g_m2 / 10^3),
    is.na(C_stock_mg_m2) & is.na(C_stock_g_m2) &
      !is.na(C_kg_ha) ~ (C_kg_ha * 100),
    T ~ NA), .after = C_conc_percent) %>%
  # Drop now-superseded columns
  dplyr::select(-N_stock_mg_m2, -N_stock_g_m2, -N_kg_ha,
                -C_stock_mg_m2, -C_stock_g_m2, -C_kg_ha) %>%
  # Rename remaining columns for clarity
  dplyr::rename(N_stock_mg_m2 = N_stock_actual,
                C_stock_mg_m2 = C_stock_actual) %>%
  # Relocate N/C 'by depth' columns to be near these others
  dplyr::relocate(N_by_depth, .after = N_stock_mg_m2) %>%
  dplyr::relocate(C_by_depth, .after = C_stock_mg_m2)

# How many NAs did we fill for Nitrogran?
summary(tidy_v5b$N_stock_mg_m2); summary(tidy_v6$N_stock_mg_m2)

# Check Carbon in the same way
summary(tidy_v5b$C_stock_mg_m2); summary(tidy_v6$C_stock_mg_m2)

# Check remaining columns' structure again
tidy_v6 %>%
  dplyr::select(dataset, dplyr::starts_with("N_"), dplyr::starts_with("C_")) %>%
  dplyr::glimpse()

# Check structure of more columns
dplyr::glimpse(tidy_v6[c(1:10, 25:35)])

## ------------------------------------------ ##
            # Phosphorus Sums ----
## ------------------------------------------ ##

# Glimpse the entire dataset
dplyr::glimpse(tidy_v6)

# Now we'll want to add together our various types of P (conditionally)
p_sums <- tidy_v6 %>%
  # First need to fill NAs with 0s to avoid making NA sums
  ## Pivot longer
  tidyr::pivot_longer(cols = -lter:-C_conc_org_percent,
                      names_to = "names", values_to = "values") %>%
  ## Remove NA / missing values
  dplyr::filter(!is.na(values) & nchar(values) != 0) %>%
  ## Pivot back to wide format and fill empty cells with 0
  tidyr::pivot_wider(names_from = names, values_from = values, values_fill = 0) %>%
  # Calculate slow P conditionally
  dplyr::mutate(slow_P_mg_kg = dplyr::case_when(
    dataset == "HJAndrews_1" ~ (HCl_P_mg_kg + ConHCl_Po_mg_kg + ConHCl_Pi_mg_kg),
    dataset == "Bonanza Creek_1" ~ (HCl_P_mg_kg + ConHCl_Po_mg_kg + ConHCl_Pi_mg_kg),
    dataset == "Bonanza Creek_2" ~ (HCl_P_mg_kg + ConHCl_Po_mg_kg + ConHCl_Pi_mg_kg),
    dataset == "Brazil" ~ (HCl_P_mg_kg + ConHCl_Po_mg_kg + ConHCl_Pi_mg_kg),
    dataset == "Calhoun" ~ (HCl_P_mg_kg + ConHCl_Po_mg_kg + ConHCl_Pi_mg_kg),
    dataset == "CedarCreek_1" ~ (HCl_P_mg_kg + ConHCl_Po_mg_kg + ConHCl_Pi_mg_kg),
    dataset == "Coweeta" ~ (HCl_P_mg_kg + ConHCl_Po_mg_kg + ConHCl_Pi_mg_kg),
    dataset == "Fernow" ~ (HCl_P_mg_kg + ConHCl_Po_mg_kg + ConHCl_Pi_mg_kg),
    dataset == "FloridaCoastal" ~ (HCl_P_mg_kg + ConHCl_Po_mg_kg + ConHCl_Pi_mg_kg),
    dataset == "Hubbard Brook" ~ (HCl_P_mg_kg + ConHCl_Po_mg_kg + ConHCl_Pi_mg_kg),
    dataset == "Jornada" ~ (HCl_P_mg_kg + ConHCl_Po_mg_kg + ConHCl_Pi_mg_kg),
    dataset == "Kellog_Biological_Station" ~ (HCl_P_mg_kg + ConHCl_Po_mg_kg + ConHCl_Pi_mg_kg),
    dataset == "Konza_1" ~ (HCl_P_mg_kg + ConHCl_Po_mg_kg + ConHCl_Pi_mg_kg),
    dataset == "Luquillo_1" ~ (HCl_P_mg_kg + ConHCl_Po_mg_kg + ConHCl_Pi_mg_kg),
    dataset == "Luquillo_2" ~ (HCl_P_mg_kg + ConHCl_Po_mg_kg + ConHCl_Pi_mg_kg),
    dataset == "Niwot_1" ~ (HCl_P_mg_kg + ConHCl_Po_mg_kg + ConHCl_Pi_mg_kg),
    dataset == "Niwot_2" ~ (HCl_P_mg_kg + ConHCl_Po_mg_kg + ConHCl_Pi_mg_kg),
    dataset == "Niwot_3" ~ (HCl_P_mg_kg + ConHCl_Po_mg_kg + ConHCl_Pi_mg_kg),
    dataset == "Sevilleta_1" ~ (HCl_P_mg_kg + ConHCl_Po_mg_kg + ConHCl_Pi_mg_kg),
    dataset == "Sevilleta_2" ~ (HCl_P_mg_kg + ConHCl_Po_mg_kg + ConHCl_Pi_mg_kg),
    dataset == "Toolik" ~ (HCl_P_mg_kg + ConHCl_Po_mg_kg + ConHCl_Pi_mg_kg),
    # dataset == "" ~ (),
    TRUE ~ NA )) %>%
  # Also total P
  dplyr::mutate(total_P_mg_kg = dplyr::case_when(
    dataset == "HJAndrews_1" ~ (Resin_P_mg_kg + HCO3_Po_mg_kg + HCO3_Pi_mg_kg +
                                      NaOH_Po_mg_kg + NaOH_Pi_mg_kg + HCl_P_mg_kg +
                                      ConHCl_Po_mg_kg + ConHCl_Pi_mg_kg + MIII_P_mg_kg +
                                      HCO3_P_mg_kg + NaOH_P_mg_kg + Sonic_P_mg_kg + Residual_P_mg_kg +
                                      Sonic_Pi_mg_kg + Sonic_Po_mg_kg),
    dataset == "Bonanza Creek_1" ~ (Resin_P_mg_kg + HCO3_Po_mg_kg + HCO3_Pi_mg_kg +
                                      NaOH_Po_mg_kg + NaOH_Pi_mg_kg + HCl_P_mg_kg +
                                      ConHCl_Po_mg_kg + ConHCl_Pi_mg_kg + MIII_P_mg_kg +
                                      HCO3_P_mg_kg + NaOH_P_mg_kg + Sonic_P_mg_kg + Residual_P_mg_kg +
                                      Sonic_Pi_mg_kg + Sonic_Po_mg_kg),
    dataset == "Bonanza Creek_2" ~ (Resin_P_mg_kg + HCO3_Po_mg_kg + HCO3_Pi_mg_kg +
                                      NaOH_Po_mg_kg + NaOH_Pi_mg_kg + HCl_P_mg_kg +
                                      ConHCl_Po_mg_kg + ConHCl_Pi_mg_kg + MIII_P_mg_kg +
                                      HCO3_P_mg_kg + NaOH_P_mg_kg + Sonic_P_mg_kg + Residual_P_mg_kg +
                                      Sonic_Pi_mg_kg + Sonic_Po_mg_kg),
    dataset == "Brazil" ~ (Resin_P_mg_kg + HCO3_Po_mg_kg + HCO3_Pi_mg_kg +
                             NaOH_Po_mg_kg + NaOH_Pi_mg_kg + HCl_P_mg_kg +
                             ConHCl_Po_mg_kg + ConHCl_Pi_mg_kg + MIII_P_mg_kg +
                             HCO3_P_mg_kg + NaOH_P_mg_kg + Sonic_P_mg_kg + Residual_P_mg_kg +
                             Sonic_Pi_mg_kg + Sonic_Po_mg_kg),
    dataset == "Calhoun" ~ (Resin_P_mg_kg + HCO3_Po_mg_kg + HCO3_Pi_mg_kg +
                              NaOH_Po_mg_kg + NaOH_Pi_mg_kg + HCl_P_mg_kg +
                              ConHCl_Po_mg_kg + ConHCl_Pi_mg_kg + MIII_P_mg_kg +
                              HCO3_P_mg_kg + NaOH_P_mg_kg + Sonic_P_mg_kg + Residual_P_mg_kg +
                              Sonic_Pi_mg_kg + Sonic_Po_mg_kg),
    dataset == "CedarCreek_1" ~ (Resin_P_mg_kg + HCO3_Po_mg_kg + HCO3_Pi_mg_kg +
                              NaOH_Po_mg_kg + NaOH_Pi_mg_kg + HCl_P_mg_kg +
                              ConHCl_Po_mg_kg + ConHCl_Pi_mg_kg + MIII_P_mg_kg +
                              HCO3_P_mg_kg + NaOH_P_mg_kg + Sonic_P_mg_kg + Residual_P_mg_kg +
                              Sonic_Pi_mg_kg + Sonic_Po_mg_kg),
    dataset == "Coweeta" ~ (Resin_P_mg_kg + HCO3_Po_mg_kg + HCO3_Pi_mg_kg +
                              NaOH_Po_mg_kg + NaOH_Pi_mg_kg + HCl_P_mg_kg +
                              ConHCl_Po_mg_kg + ConHCl_Pi_mg_kg + MIII_P_mg_kg +
                              HCO3_P_mg_kg + NaOH_P_mg_kg + Sonic_P_mg_kg + Residual_P_mg_kg +
                              Sonic_Pi_mg_kg + Sonic_Po_mg_kg),
    dataset == "Fernow" ~ (Resin_P_mg_kg + HCO3_Po_mg_kg + HCO3_Pi_mg_kg +
                             NaOH_Po_mg_kg + NaOH_Pi_mg_kg + HCl_P_mg_kg +
                             ConHCl_Po_mg_kg + ConHCl_Pi_mg_kg + MIII_P_mg_kg +
                             HCO3_P_mg_kg + NaOH_P_mg_kg + Sonic_P_mg_kg + Residual_P_mg_kg +
                             Sonic_Pi_mg_kg + Sonic_Po_mg_kg),
    dataset == "FloridaCoastal" ~ (Resin_P_mg_kg + HCO3_Po_mg_kg + HCO3_Pi_mg_kg +
                                     NaOH_Po_mg_kg + NaOH_Pi_mg_kg + HCl_P_mg_kg +
                                     ConHCl_Po_mg_kg + ConHCl_Pi_mg_kg + MIII_P_mg_kg +
                                     HCO3_P_mg_kg + NaOH_P_mg_kg + Sonic_P_mg_kg + Residual_P_mg_kg +
                                     Sonic_Pi_mg_kg + Sonic_Po_mg_kg),
    dataset == "Hubbard Brook" ~ (Resin_P_mg_kg + HCO3_Po_mg_kg + HCO3_Pi_mg_kg +
                                    NaOH_Po_mg_kg + NaOH_Pi_mg_kg + HCl_P_mg_kg +
                                    ConHCl_Po_mg_kg + ConHCl_Pi_mg_kg + MIII_P_mg_kg +
                                    HCO3_P_mg_kg + NaOH_P_mg_kg + Sonic_P_mg_kg + Residual_P_mg_kg +
                                    Sonic_Pi_mg_kg + Sonic_Po_mg_kg),
    dataset == "Jornada" ~ (Resin_P_mg_kg + HCO3_Po_mg_kg + HCO3_Pi_mg_kg +
                              NaOH_Po_mg_kg + NaOH_Pi_mg_kg + HCl_P_mg_kg +
                              ConHCl_Po_mg_kg + ConHCl_Pi_mg_kg + MIII_P_mg_kg +
                              HCO3_P_mg_kg + NaOH_P_mg_kg + Sonic_P_mg_kg + Residual_P_mg_kg +
                              Sonic_Pi_mg_kg + Sonic_Po_mg_kg),
    dataset == "Kellog_Biological_Station" ~ (Resin_P_mg_kg + HCO3_Po_mg_kg + HCO3_Pi_mg_kg +
                              NaOH_Po_mg_kg + NaOH_Pi_mg_kg + HCl_P_mg_kg +
                              ConHCl_Po_mg_kg + ConHCl_Pi_mg_kg + MIII_P_mg_kg +
                              HCO3_P_mg_kg + NaOH_P_mg_kg + Sonic_P_mg_kg + Residual_P_mg_kg +
                              Sonic_Pi_mg_kg + Sonic_Po_mg_kg),
    dataset == "Konza_1" ~ (Resin_P_mg_kg + HCO3_Po_mg_kg + HCO3_Pi_mg_kg +
                              NaOH_Po_mg_kg + NaOH_Pi_mg_kg + HCl_P_mg_kg +
                              ConHCl_Po_mg_kg + ConHCl_Pi_mg_kg + MIII_P_mg_kg +
                              HCO3_P_mg_kg + NaOH_P_mg_kg + Sonic_P_mg_kg + Residual_P_mg_kg +
                              Sonic_Pi_mg_kg + Sonic_Po_mg_kg),
    dataset == "Luquillo_1" ~ (Resin_P_mg_kg + HCO3_Po_mg_kg + HCO3_Pi_mg_kg +
                                 NaOH_Po_mg_kg + NaOH_Pi_mg_kg + HCl_P_mg_kg +
                                 ConHCl_Po_mg_kg + ConHCl_Pi_mg_kg + MIII_P_mg_kg +
                                 HCO3_P_mg_kg + NaOH_P_mg_kg + Sonic_P_mg_kg + Residual_P_mg_kg +
                                 Sonic_Pi_mg_kg + Sonic_Po_mg_kg),
    dataset == "Luquillo_2" ~ (Resin_P_mg_kg + HCO3_Po_mg_kg + HCO3_Pi_mg_kg +
                                 NaOH_Po_mg_kg + NaOH_Pi_mg_kg + HCl_P_mg_kg +
                                 ConHCl_Po_mg_kg + ConHCl_Pi_mg_kg + MIII_P_mg_kg +
                                 HCO3_P_mg_kg + NaOH_P_mg_kg + Sonic_P_mg_kg + Residual_P_mg_kg +
                                 Sonic_Pi_mg_kg + Sonic_Po_mg_kg),
    dataset == "Niwot_1" ~ (Resin_P_mg_kg + HCO3_Po_mg_kg + HCO3_Pi_mg_kg +
                              NaOH_Po_mg_kg + NaOH_Pi_mg_kg + HCl_P_mg_kg +
                              ConHCl_Po_mg_kg + ConHCl_Pi_mg_kg + MIII_P_mg_kg +
                              HCO3_P_mg_kg + NaOH_P_mg_kg + Sonic_P_mg_kg + Residual_P_mg_kg +
                              Sonic_Pi_mg_kg + Sonic_Po_mg_kg),
    dataset == "Niwot_2" ~ (Resin_P_mg_kg + HCO3_Po_mg_kg + HCO3_Pi_mg_kg +
                              NaOH_Po_mg_kg + NaOH_Pi_mg_kg + HCl_P_mg_kg +
                              ConHCl_Po_mg_kg + ConHCl_Pi_mg_kg + MIII_P_mg_kg +
                              HCO3_P_mg_kg + NaOH_P_mg_kg + Sonic_P_mg_kg + Residual_P_mg_kg +
                              Sonic_Pi_mg_kg + Sonic_Po_mg_kg),
    dataset == "Niwot_3" ~ (Resin_P_mg_kg + HCO3_Po_mg_kg + HCO3_Pi_mg_kg +
                              NaOH_Po_mg_kg + NaOH_Pi_mg_kg + HCl_P_mg_kg +
                              ConHCl_Po_mg_kg + ConHCl_Pi_mg_kg + MIII_P_mg_kg +
                              HCO3_P_mg_kg + NaOH_P_mg_kg + Sonic_P_mg_kg + Residual_P_mg_kg +
                              Sonic_Pi_mg_kg + Sonic_Po_mg_kg),
    dataset == "Sevilleta_1" ~ (Resin_P_mg_kg + HCO3_Po_mg_kg + HCO3_Pi_mg_kg +
                                  NaOH_Po_mg_kg + NaOH_Pi_mg_kg + HCl_P_mg_kg +
                                  ConHCl_Po_mg_kg + ConHCl_Pi_mg_kg + MIII_P_mg_kg +
                                  HCO3_P_mg_kg + NaOH_P_mg_kg + Sonic_P_mg_kg + Residual_P_mg_kg +
                                  Sonic_Pi_mg_kg + Sonic_Po_mg_kg),
    dataset == "Sevilleta_2" ~ (Resin_P_mg_kg + HCO3_Po_mg_kg + HCO3_Pi_mg_kg +
                                  NaOH_Po_mg_kg + NaOH_Pi_mg_kg + HCl_P_mg_kg +
                                  ConHCl_Po_mg_kg + ConHCl_Pi_mg_kg + MIII_P_mg_kg +
                                  HCO3_P_mg_kg + NaOH_P_mg_kg + Sonic_P_mg_kg + Residual_P_mg_kg +
                                  Sonic_Pi_mg_kg + Sonic_Po_mg_kg),
    dataset == "Toolik" ~ (Resin_P_mg_kg + HCO3_Po_mg_kg + HCO3_Pi_mg_kg +
                             NaOH_Po_mg_kg + NaOH_Pi_mg_kg + HCl_P_mg_kg +
                             ConHCl_Po_mg_kg + ConHCl_Pi_mg_kg + MIII_P_mg_kg +
                             HCO3_P_mg_kg + NaOH_P_mg_kg + Sonic_P_mg_kg + Residual_P_mg_kg +
                             Sonic_Pi_mg_kg + Sonic_Po_mg_kg),
    # dataset == "" ~ (),
    TRUE ~ NA )) %>%
  # After summing, remove all columns where we changed NAs to 0s
  dplyr::select(lter:C_conc_org_percent, slow_P_mg_kg, total_P_mg_kg) %>%
  # Keep only unique rows
  dplyr::distinct()

# Any datasets missing?
p_sums %>%
  dplyr::filter(is.na(slow_P_mg_kg) | is.na(total_P_mg_kg)) %>%
  dplyr::pull(dataset) %>%
  unique()

# Check structure
dplyr::glimpse(p_sums)

# Note we're doing this in a separate object because we coerced NAs into 0s for algebra reasons
## They're not "real" 0s so we want to preserve the real 0s while still easily getting sums

# Now we can attach our sums to the original tidy object
tidy_v7 <- tidy_v6 %>%
  # By not specifying which columns to join by, all shared columns will be used
  dplyr::left_join(y = p_sums) %>%
  # Move our P sums to the left for more easy reference
  dplyr::relocate(slow_P_mg_kg, total_P_mg_kg, .after = bulk_density_kg_ha)

# Check structure
dplyr::glimpse(tidy_v7)

## ------------------------------------------ ##
              # Absolute P ----
## ------------------------------------------ ##

# Calculate absolute P totals (rather than portions of each core)
tidy_v8 <- tidy_v7 %>%
  # Due to our earlier depth/bulk density prep this is easy!
  dplyr::mutate(slow_P_absolute = slow_P_mg_kg * core_length_cm * bulk_density_g_cm3,
                .before = slow_P_mg_kg) %>%
  dplyr::mutate(total_P_absolute = total_P_mg_kg * core_length_cm * bulk_density_g_cm3,
                .before = total_P_mg_kg)
## Units of absolute sums are ____?
  
# Re-check structure
dplyr::glimpse(tidy_v8)

## ------------------------------------------ ##
        # Relative Depth Wrangling ----
## ------------------------------------------ ##

# Some depths are relative to their soil horizon
## Need to calculate their objective depths

# Split off only relative data
rel_v1 <- dplyr::filter(tidy_v8, depth_type == "relative") %>%
  # Keep only columns with at least one value
  dplyr::select(dplyr::where(~ !(all(is.na(.)) | all(. == "")))) %>%
  # Add in a row number column
  dplyr::mutate(row_num = 1:nrow(.))

# Split off non-relative data (to avoid accidentally tweaking it)
tidy_v8_nonrelative <- dplyr::filter(tidy_v8, depth_type == "objective" | is.na(depth_type))

# Check to make sure no rows were lost
nrow(rel_v1) + nrow(tidy_v8_nonrelative) == nrow(tidy_v8)

# What horizon layers are in this subset of the data?
rel_v1 %>%
  dplyr::select(dataset, horizon) %>%
  dplyr::distinct()

# Check structure
dplyr::glimpse(rel_v1)

# Wrangle relative depth data object
rel_v2 <- rel_v1 %>%
  # Pare down to only the necessary columns for depth / re-attaching with core data later
  dplyr::select(row_num, lter:treatment_years, horizon, depth_start_cm, depth_end_cm) %>%
  # Pivot longer to have depth start/end as a column
  tidyr::pivot_longer(cols = depth_start_cm:depth_end_cm,
                      names_to = "start_end", values_to = "depth_cm") %>%
  # Clean up start/end column
  dplyr::mutate(start_end = gsub(pattern = "depth_|_cm", replacement = "", x = start_end)) %>%
  # Paste together with horizon
  dplyr::mutate(temp_horizon = paste0(paste0("temp_", horizon), "_", start_end)) %>%
  # Dump unwanted columns (plus all horizon columns except new 'temporary horizon')
  dplyr::select(-start_end, -horizon) %>%
  # Pivot wider with new 'temp horizon' (horizon + start/end) as columns
  tidyr::pivot_wider(names_from = temp_horizon, values_from = depth_cm) %>%
  # Reorder based on horizon order
  dplyr::relocate(dplyr::starts_with("temp_O"), 
                  dplyr::starts_with("temp_A"), 
                  dplyr::starts_with("temp_C"), 
                  .after = dplyr::everything())

# Check structure
dplyr::glimpse(rel_v2)

# Can begin fixing the relative depths now
## Doing all of these as `case_when`s to allow for future differences in horizon inclusion
rel_v3 <- rel_v2 %>%
  # If O start is empty, assume it's 0
  dplyr::mutate(temp_O_start = dplyr::case_when(
    dataset == "Hubbard Brook" & is.na(temp_O_start) ~ 0,
    T ~ temp_O_start)) %>%
  # If O end is empty but next horizon isn't empty, replace with that
  ## Doing as `case_when` to allow for future below O horizons (currently only "A")
  dplyr::mutate(temp_O_end = dplyr::case_when(
    dataset == "Hubbard Brook" & is.na(temp_O_end) & !is.na(temp_A_start) ~ temp_A_start,
    T ~ temp_O_end)) %>%
  # If start of C horizon is empty, add in end of preceding horizon
  dplyr::mutate(temp_C_start = dplyr::case_when(
    dataset == "Hubbard Brook" & is.na(temp_C_start) & !is.na(temp_A_end) ~ temp_A_end,
    T ~ temp_C_start))

# Check structure
dplyr::glimpse(rel_v3)
## view(rel_v3)

# Need to get back into original format
rel_v4 <- rel_v3 %>%
  # Flip to long format
  tidyr::pivot_longer(cols = dplyr::starts_with("temp_")) %>%
  # Break column names back into their component parts
  tidyr::separate_wider_delim(cols = name, delim = "_",
                              names = c("junk", "horizon", "start_end")) %>%
  # Drop junk column
  dplyr::select(-junk) %>%
  # Reformat start/end column
  dplyr::mutate(start_end = paste0("depth_", start_end, "_cm")) %>%
  # Get separate depth columns
  tidyr::pivot_wider(names_from = start_end, values_from = value)

# Check structure
dplyr::glimpse(rel_v4)

# Identify columns shared with first version of relative depth data (except depth cols)
(rel_shares <- setdiff(x = intersect(x = names(rel_v1), y = names(rel_v4)),
                      y = c("depth_start_cm", "depth_end_cm")) )

# Re-attach deduced depths to original dataset
rel_v5 <- rel_v1 %>%
  dplyr::left_join(y = rel_v4, by = rel_shares) %>%
  # Combine old and new depth columns keeping non-NAs where they occur
  dplyr::mutate(depth_start_cm = dplyr::coalesce(depth_start_cm.x, depth_start_cm.y),
                depth_end_cm = dplyr::coalesce(depth_end_cm.x, depth_end_cm.y),
                .after = depth_start_cm.x) %>%
  # Drop .x and .y depth columns left over from that join
  dplyr::select(-dplyr::ends_with(".x"), -dplyr::ends_with(".y")) %>%
  # Drop row number column
  dplyr::select(-row_num)

# Check structure
dplyr::glimpse(rel_v5)

# How many NAs does this whole operation solve?
summary(rel_v1$depth_start_cm); summary(rel_v5$depth_start_cm)
summary(rel_v1$depth_end_cm); summary(rel_v5$depth_end_cm)
## Fixes more starts than ends (likely because of 'start of O is 0' assumption)

# Finish up the depth/horizon wrangling
tidy_v9 <- tidy_v8_nonrelative %>%
  # Bind the rows of the wrangled relative data object back onto the dataframe
  dplyr::bind_rows(rel_v5) %>% # Comfortable with assumptions? Use this
  # dplyr::bind_rows(rel_v1) %>% # Don't want to make assumptions? Use this
  # Drop depth type column
  dplyr::select(-depth_type) %>%
  # Assemble a new depth range column that uses the tidied, objective depths
  dplyr::mutate(depth_range_cm = ifelse(!is.na(depth_start_cm) & !is.na(depth_end_cm),
                                        yes = paste0(depth_start_cm, "-", depth_end_cm),
                                        no = NA),
                .after = depth_end_cm)

# Re-check structure
dplyr::glimpse(tidy_v9)

# Retained all rows?
nrow(tidy_v9) == nrow(tidy_v8)

## ------------------------------------------ ##
                  # Export ----
## ------------------------------------------ ##

# Create a final data object
final_tidy <- tidy_v9

# Check its structure
dplyr::glimpse(final_tidy)

# Save out the final data object
write.csv(x = final_tidy, file = "tidy_soil_p.csv", row.names = F, na = "")

# Upload to GoogleDrive
googledrive::drive_upload(media = "tidy_soil_p.csv", overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1pjgN-wRlec65NDLBvryibifyx6k9Iqy9"))

# End ----
