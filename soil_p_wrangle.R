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
librarian::shelf(tidyverse, googledrive, supportR)

# Create necessary sub-folder(s)
dir.create(path = file.path("raw_data"), showWarnings = F)

# Identify raw data files
raw_ids <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/10igyNjNSEJDrz5mUtYyxgbUPDUO7bsuW"), type = "csv")

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
supportR::diff_check(old = raw_files, unique(key_v0$Raw_Filename))

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
  dplyr::select(-first_char, -Extraction_Method, -Notes)

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
    message("Not all expected columns are in data key!")
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
    dplyr::distinct() %>%
    # Make numeric columns actually be numeric (had to coerce to character earlier)
    dplyr::mutate(dplyr::across(.cols = c(dplyr::starts_with("lat"), 
                                          dplyr::starts_with("lon"),
                                          dplyr::ends_with("_mg_kg"),
                                          dplyr::ends_with("_percent")),
                                .fns = as.numeric))
  
  # Add to list
  df_list[[focal_raw]] <- raw_df
  
  # Success message
  message("Wrangling complete for '", focal_raw, "' (", length(raw_files) - j, " files remaining)") 
  
} # Close loop

# Unlist the list we just generated
tidy_v1 <- df_list %>%
  purrr::list_rbind()

# Glimpse it
dplyr::glimpse(tidy_v1)


# End ----
