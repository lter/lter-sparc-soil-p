## ------------------------------------------ ##
     # SPARC Soil P -- Data Harmonization
## ------------------------------------------ ##
# Script author(s): Nick J Lyon

# Purpose:
## Absorb all raw files and (using data key) combine them
## Finishes with an 'archive ready' long format data file

## ------------------------------------------ ##
              # Housekeeping -----
## ------------------------------------------ ##

# Load necessary libraries
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive, supportR, psych)

# Create necessary sub-folder(s)
dir.create(path = file.path("data"), showWarnings = F)
dir.create(path = file.path("data", "raw_data"), showWarnings = F)
dir.create(path = file.path("data", "tidy_data"), showWarnings = F)
dir.create(path = file.path("data", "key_files"), showWarnings = F)

# Identify raw data files
raw_ids <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/10igyNjNSEJDrz5mUtYyxgbUPDUO7bsuW"), type = "csv")

# Download each data file into the new 'raw_data' folder
purrr::walk2(.x = raw_ids$id, .y = raw_ids$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("data", "raw_data", .y)))

# Clear environment
rm(list = ls())

## ------------------------------------------ ##
              # Data Key Prep ----
## ------------------------------------------ ##

# Download data key (connects raw column names with synonymized equivalents)
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1WIAo08Jnmp7BdvN8xxobZ_txcFCWZ35w"), pattern = "LTER_P_DataKey") %>%
  googledrive::drive_download(file = .$id, type = "csv", overwrite = T,
                              path = file.path("data", "key_files", .$name))

# Retrieve the data key
key_v1 <- read.csv(file = file.path("data", "key_files", "LTER_P_DataKey.csv")) %>%
  # Remove any rows that lack an entry in the "Variable" column
  dplyr::filter(!is.na(Variable) & nchar(Variable) != 0 & Variable != "NA") %>%
  # Drop unwanted columns
  dplyr::select(-Total_P_Method, -Notes)

# Check structure of this object
dplyr::glimpse(key_v1)

# There are many characters we'll want to coerce to "." so make that an object
spec_char <- " |\\(|\\)|\\/|\\-|\\+|\\:|,|_|\""

# First, we need to modify the 'raw column name' column
## Reading in certain characters mangles them...
## ...so we need to do that same change in the key
key_v2 <- key_v1 %>%
  # Identify first character of the raw column name
  dplyr::mutate(first_char = stringr::str_sub(Raw_Column_Name, 
                                              start = 1, end = 1)) %>%
  # Make conditional fixes to "fix" the raw column name in the data key
  dplyr::mutate(Raw_Column_Name = dplyr::case_when(
    # Leading "%" becomes "X." when reading in CSV
    first_char == "%" ~ paste0("X.", gsub(pattern = "%", replacement = "",
                                          x = Raw_Column_Name)),
    # Leading number becomes X 
    !is.na(suppressWarnings(as.numeric(first_char))) ~ paste0("X", Raw_Column_Name),
    # Some Niwot 5 issues
    Dataset == "Niwot_5" & Raw_Column_Name == "TotalP(%)" ~ "TotalP...",
    Dataset == "Niwot_5" & Raw_Column_Name == "TotalC(%)" ~ "TotalC...",
    Dataset == "Niwot_5" & Raw_Column_Name == "TotalN(%)" ~ "TotalN...",
    # Konza_2 problems
    Dataset == "Konza_2" & Raw_Column_Name == "Total N (%)" ~ "Total.N....",
    Dataset == "Konza_2" & Raw_Column_Name == "Total C (%)" ~ "Total.C....",
    # Some LUQ columns with specific issues
    Dataset == "Luquillo_1" & Raw_Column_Name == "C (%)" ~ "C....",
    Dataset == "Luquillo_1" & Raw_Column_Name == "N (%)" ~ "N....",
    # If conditions not specified, return column unmodified
    TRUE ~ Raw_Column_Name)) %>%
  # Certain special characters become periods when in column names
  dplyr::mutate(Raw_Column_Name = gsub(pattern = spec_char,
                                       replacement = ".", 
                                       x = Raw_Column_Name)) %>%
  # Percent symbols become Xs
  dplyr::mutate(Raw_Column_Name = gsub(pattern = "\\%", replacement = "X", 
                                       x = Raw_Column_Name)) %>%
  # Drop the first character column
  dplyr::select(-first_char)
  
# Re-check structure
dplyr::glimpse(key_v2)

# We need to identify what (if anything) is *missing* from the data key for each variable (in each dataset)
key_v3 <- key_v2 %>%
  # Identify which variables _should not_ have units
  ## Note this is done **manually** and will need to be updated that way too
  dplyr::mutate(need_units = dplyr::case_when(
    Variable == "LTER" ~ "no",
    Variable == "site" ~ "no",
    Variable == "block" ~ "no",
    Variable == "plot" ~ "no",
    Variable == "core" ~ "no",
    Variable == "sample replicate" ~ "no",
    Variable == "sample replicate 2" ~ "no",
    Variable == "treatment" ~ "no",
    Variable == "treatment years" ~ "no",
    Variable == "lat" ~ "no",
    Variable == "lon" ~ "no",
    Variable == "horizon" ~ "no",
    Variable == "topography" ~ "no",
    Variable == "distance" ~ "no",
    Variable == "pH" ~ "no",
    Variable == "pH max" ~ "no",
    Variable == "pH min" ~ "no",
    # If not identified above, variables is _assumed to need units_
    T ~ "yes"), .before = Units) %>%
  # Identify which columns _do not_ need a data type
  dplyr::mutate(need_type = dplyr::case_when(
    need_units == "no" ~ "no",
    Variable == "precipitation" ~ "no",
    Variable == "depth" ~ "no",
    Variable == "org depth" ~ "no",
    Variable == "treatment" ~ "no", # EV added this 02/13/24
    stringr::str_detect(string = Variable, pattern = "bulk") ~ "no",
    stringr::str_detect(string = Variable, pattern = "soil") ~ "no",
    stringr::str_detect(string = Variable, pattern = "coarse") ~ "no",
    T ~ "yes"), .before = Data_Type) %>%
  # Identify which columns _do not_ need Phosphorus methods
  dplyr::mutate(need_method = dplyr::case_when(
    need_type == "no" ~ "no",
    stringr::str_detect(string = Variable, pattern = "N") ~ "no",
    stringr::str_detect(string = Variable, pattern = "C") ~ "no",
    T ~ "yes"), .before = Leach_Order) %>%
  # Simplify missing information to all true NAs
  dplyr::mutate(Units = ifelse(Units == "NA" | nchar(Units) == 0,
                               yes = NA, no = Units),
                Data_Type = ifelse(Data_Type == "NA" | nchar(Data_Type) == 0,
                                   yes = NA, no = Data_Type),
                Reagent = ifelse(Reagent == "NA" | nchar(Reagent) == 0,
                                   yes = NA, no = Reagent),
                Molarity = ifelse(Molarity == "NA" | nchar(Molarity) == 0,
                                   yes = NA, no = Molarity),
                Time = ifelse(Time == "NA" | nchar(Time) == 0,
                                   yes = NA, no = Time),
                Temperature = ifelse(Temperature == "NA" | nchar(Temperature) == 0,
                                   yes = NA, no = Temperature)) %>%
  # If a piece of information is needed but is absent, fill it in!
  dplyr::mutate(
    ## Units
    Units = ifelse(need_units == "yes" & is.na(Units),
                   yes = "units", no = Units),
    ## Data Type
    Data_Type = ifelse(need_type == "yes" & is.na(Data_Type),
                       yes = "data.type", no = Data_Type),
    ## Methods information
    Leach_Order = ifelse(need_method == "yes" & is.na(Leach_Order),
                         yes = "order", no = Leach_Order),
    Reagent = ifelse(need_method == "yes" & is.na(Reagent),
                     yes = "reagent", no = Reagent),
    Molarity = ifelse(need_method == "yes" & is.na(Molarity),
                      yes = "molarity", no = Molarity),
    Time = ifelse(need_method == "yes" & is.na(Time),
                  yes = "time", no = Time),
    Temperature = ifelse(need_method == "yes" & is.na(Temperature),
                         yes = "temp", no = Temperature))

# Check which columns _DO_ need units
## Add them above if any of these don't belong
unique(dplyr::filter(key_v3, need_units == "yes")$Variable)

# Do the same for data type
unique(dplyr::filter(key_v3, need_type == "yes")$Variable)

# And for P methods
unique(dplyr::filter(key_v3, need_method == "yes")$Variable)

# Check structure again
dplyr::glimpse(key_v3)

# Do final sub-column tidying to assemble the 'combined column name'
key_v4 <- key_v3 %>%
  # Fix special characters in all user-entered columns in the key
  dplyr::mutate(dplyr::across(.cols = c(Variable:Temperature), 
                              .fns = ~ gsub(pattern = spec_char, 
                                            replacement = ".", x = .x))) %>%
  # Combine them into a combined column name based on what they need!
  dplyr::mutate(Combined_Column_Name = dplyr::case_when(
    ## If needs all info then aggregate all key columns!
    need_units == "yes" & need_type == "yes" & 
      need_method == "yes" ~ paste(Variable, Data_Type, Units, Leach_Order, Molarity, Reagent, Time, Temperature, sep = "_"),
    ## Otherwise only use columns that are needed
    need_units == "yes" & need_type == "yes" &
      need_method == "no" ~ paste(Variable, Data_Type, Units, sep = "_"),
    need_units == "yes" & need_type == "no" &
      need_method == "no" ~ paste(Variable, Units, sep = "_"),
    # If doesn't need anything just use the variable as-is
    need_units == "no" & need_type == "no" &
      need_method == "no" ~ Variable))

# Do one more structure check
dplyr::glimpse(key_v4)

# Pare down to only needed columns
key_v5 <- key_v4 %>%
  dplyr::select(Dataset, Raw_Filename, Raw_Column_Name, Combined_Column_Name) %>%
  dplyr::distinct()

# Check structure of key
dplyr::glimpse(key_v5)
## view(key_v5)

# Check whether any raw column names are duplicate within any data file
key_v5 %>% 
  dplyr::group_by(Raw_Filename, Raw_Column_Name) %>%
  dplyr::summarize(ct = dplyr::n()) %>%
  dplyr::filter(ct > 1)

## ------------------------------------------ ##
            # Data Harmonizing ----
## ------------------------------------------ ##

# Identify the downloaded raw files
downloaded_files <- dir(path = file.path("data", "raw_data"))

# Compare the two to see if all file names in the key were in the Drive
supportR::diff_check(old = downloaded_files, new = unique(key_v5$Raw_Filename))

# Subset the data key to only raw files that we downloaded
key <- dplyr::filter(key_v5, Raw_Filename %in% downloaded_files)

# Subset the downloaded files to only those in the data key
raw_files <- intersect(downloaded_files, key$Raw_Filename)

# Make an empty list (to store raw data in shortly)
df_list <- list()

# For each raw file...
for(j in 1:length(raw_files)){
  
  # Grab its name
  focal_raw <- raw_files[j]
  
  # Subset the key object to only this file's section
  key_sub <- dplyr::filter(key, Raw_Filename == focal_raw)
  
  # Load in that file
  raw_df_v1 <- read.csv(file = file.path("data", "raw_data", focal_raw))
  
  # Process it to ready for integration with other raw files
  raw_df_v2 <- raw_df_v1 %>%
    # Create a row number column and a column for the original file
    dplyr::mutate(row_num = 1:nrow(.),
                  Raw_Filename = focal_raw,
                  .before = dplyr::everything()) %>%
    # Make all columns into character columns
    dplyr::mutate(dplyr::across(.cols = dplyr::everything(), 
                                .fns = as.character)) %>%
    # Now pivot everything into ultimate long format
    ## Note: if column class differs this step can't be done
    ## That is why we convert everything into characters in the previous step
    tidyr::pivot_longer(cols = -row_num:-Raw_Filename,
                        names_to = "Raw_Column_Name",
                        values_to = "values") %>%
    # Process raw column names as needed
    dplyr::mutate(Raw_Column_Name = gsub(pattern = spec_char, replacement = ".",
                                         x = Raw_Column_Name))
  
  # Identify any columns that are in the data key but (apparently) not in the data
  missing_cols <- setdiff(x = key_sub$Raw_Column_Name, y = unique(raw_df_v2$Raw_Column_Name))
  
  # If any are found, print a warning for whoever is running this
  if(length(missing_cols) != 0){
    message("Not all expected columns in '", focal_raw, "' are in data key!")
    message("Check (and fix if needed) raw columns: ")
    print(paste0("'", missing_cols, "'", collapse = " & ")) }
  
  # Drop this object (if it exists) to avoid false warning with the next run of the loop
  if(exists("missing_cols") == T){ rm(list = "missing_cols") }
  
  # Integrate synonymized column names from key
  raw_df_v3 <- raw_df_v2 %>%
    # Attach revised column names
    dplyr::left_join(key_sub, by = c("Raw_Filename", "Raw_Column_Name")) %>%
    # Drop any columns that don't have a synonymized equivalent
    dplyr::filter(!is.na(Combined_Column_Name)) %>%
    # Pare down to only needed columns (implicitly removes unspecified columns)
    dplyr::select(row_num, Dataset, Raw_Filename, Combined_Column_Name, values)
  
  # As a separate object (for ease of maintenance we want the preceding work in its own object)
  raw_df <- raw_df_v3 %>%
    # Pivot back to wide format with revised column names
    tidyr::pivot_wider(names_from = Combined_Column_Name, 
                       values_from = values, 
                       values_fill = NA) %>%
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

# Fix any typos identified above
tidy_v1b <- tidy_v1 %>%
  # Fix some of the spatial/site columns
  dplyr::mutate(lat = as.numeric(lat),
                lon = as.numeric(lon)) %>%
  # Rename columns so that everything is in snake case except element abbreviations
  ## snake case = "lower_lower_lower"
  dplyr::rename(lter = LTER,
                dataset = Dataset,
                raw_filename = Raw_Filename) %>%
  # Relocate all spatial/site columns to the left of the dataframe
  dplyr::relocate(lter, dataset, raw_filename, lat, lon, site, plot, block, core,
                  sample.replicate, treatment, 
                  horizon, depth_cm, org.depth_cm, pH,
                  .before = dplyr::everything()) %>%
  # Create a better version of the LTER column
  dplyr::mutate(lter = dplyr::coalesce(lter, dataset)) %>%
  dplyr::mutate(lter = dplyr::case_when(
    raw_filename == "HJAndrews_Spears.et.al_2000.csv" ~ "AND",
    lter %in% c("Bonanza Creek_1", "Bonanza Creek_2", "Bonanza Creek_3") ~ "BNZ",
    lter %in% c("Cedar Creek ") ~ "CDR",
    lter %in% c("Chichaqua Bottoms ") ~ "Chichaqua",
    lter %in% c("Coweeta") ~ "CWT",
    lter %in% c("FloridaCoastal") ~ "FCE",
    lter %in% c("Hubbard Brook") ~ "HBR",
    lter %in% c("Jornada_1", "Jornada_2") ~ "JRN",
    lter %in% c("Kellog_Biological_Station") ~ "KBS",
    lter %in% c("Konza_1", "Konza_2") ~ "KNZ",
    lter %in% c("Luquillo_1", "Luquillo_2", "Luquillo_3") ~ "LUQ",
    lter %in% c("Niwot_1", "Niwot_2", "Niwot_3", "Niwot_4", "Niwot_5") ~ "NWT",
    lter %in% c("Sevilleta_1", "Sevilleta_2") ~ "SEV",
    lter %in% c("Toolik_1", "Toolik_2") ~ "ARC",
    # Non-LTER sites
    lter %in% c("Brazil_SouthernAmazon","Brazil_AtlanticForest","Tapajos") ~ "Brazil",
    lter %in% c("Calhoun") ~ "Calhoun",
    lter %in% c("Fernow") ~ "Fernow",
    # Otherwise retain whatever was in that column originally
    T ~ lter)) %>%
  # Clarify Chichaqua Bottoms dataset
  dplyr::mutate(dataset = ifelse(lter == "Chichaqua",
                                 yes = "CedarCreek_2",
                                 no = dataset)) %>%
  # And simplify LTER for that dataset
  dplyr::mutate(lter = ifelse(lter == "Chichaqua",
                                 yes = "CDR", no = lter))  %>%
  # Also make a simplified dataset name for use down the line
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
                                    x = dataset_simp)) %>% 
  dplyr::mutate(dataset_simp = gsub(pattern = "SouthernAmazon", replacement = "SA", 
                                    x = dataset_simp)) %>% 
  dplyr::mutate(dataset_simp = gsub(pattern = "AtlanticForest", replacement = "AF", 
                                    x = dataset_simp))

# Check out new LTER column
sort(unique(tidy_v1b$lter))
sort(unique(tidy_v1b$dataset_simp))

# Not all datasets are collected at the same level of spatial granularity
## Those that don't have a given level (e.g., data only at plot level not specific cores)...
## ...have NA in the levels of information that they are missing

# If any piece of information is missing, fill with the next coarser piece
tidy_v2 <- tidy_v1b %>%
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
tidy_v2 %>%
  dplyr::select(lter, dataset, site, block, plot, core) %>%
  dplyr::glimpse()

# Collapse spatial organization to get a quick sense of how many granularity is available
tidy_v2 %>%
  dplyr::group_by(lter, dataset) %>%
  dplyr::summarize(site_ct = length(unique(site)),
                   sites = paste(unique(site), collapse = "; "),
                   block_ct = length(unique(block)),
                   blocks = paste(unique(block), collapse = "; "),
                   plot_ct = length(unique(plot)),
                   plots = paste(unique(plot), collapse = "; "),
                   core_ct = length(unique(core)),
                   cores = paste(unique(core), collapse = "; "))

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
  dplyr::mutate(depth_raw = gsub(pattern = "_|--", replacement = "-", x = depth_cm)) %>%
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
  dplyr::select(-dplyr::contains(".by.depth")) %>%
  dplyr::glimpse()

# Check for depth values that *aren't* ranges (i.e., no hyphens)
tidy_v2b %>%
  dplyr::filter(stringr::str_detect(string = depth_raw, pattern = "-") != T) %>%
  dplyr::select(dataset, raw_filename, depth_raw) %>%
  dplyr::distinct()

# Wrangle depth into actual numbers
tidy_v2c <- tidy_v2b %>%
  dplyr::mutate(depth_range_raw = dplyr::case_when(
    ## Andrews
    dataset == "HJAndrews_1" & depth_raw == "5" ~ "0-5",
    dataset == "HJAndrews_1" & depth_raw == "15" ~ "5-15", 
    dataset == "HJAndrews_1" & depth_raw == "30" ~ "15-30",
    dataset == "HJAndrews_1" & depth_raw == "60" ~ "30-60", 
    ## Bonanza (1)
    dataset == "Bonanza Creek_1" & depth_raw == "24" ~ "24-40",
    dataset == "Bonanza Creek_1" & depth_raw == "36" ~ "36-50",
    dataset == "Bonanza Creek_1" ~ gsub(pattern = "\\+", replacement = "", x = depth_raw),
    ## Bonanza (2)
    ### Starting depth listed in separate column
    dataset == "Bonanza Creek_2" ~ paste0("0-", depth_raw),
    ## Bonanza (3)
    # dataset == "Bonanza Creek_3" ~ "0-10",
    ## Brazil_SouthernAmazon
    dataset == "Brazil_SouthernAmazon" & depth_raw == "0--10" ~ "0-10",
    dataset == "Brazil_SouthernAmazon" & depth_raw == "10--30" ~ "10-30",
    ## Brazil_AtlanticForest
    dataset == "Brazil_AtlanticForest" & depth_raw == "0--10" ~ "0-10",
    dataset == "Brazil_AtlanticForest" & depth_raw == "10--30" ~ "10-30",
    ## Coweeta
    dataset == "Coweeta" & depth_raw == "10" ~ "10-30", # All other begin at 10 are 10-30
    dataset == "Coweeta" & depth_raw == "30+" ~ "30-60", # End of range is a guess
    ## Florida
    # dataset == "FloridaCoastal" & depth_raw == "0_10" ~ "0-10",
    dataset == "FloridaCoastal" & depth_raw == "hurricane_sediment" ~ "0-1",
    ## Hubbard Brook
    dataset == "Hubbard Brook" & depth_raw == "30+" ~ "30-40",
    dataset == "Hubbard Brook" & depth_raw == "C+" ~ "", # guess needed
    dataset == "Hubbard Brook" & depth_raw == "Oa" ~ "", # guess needed
    dataset == "Hubbard Brook" & depth_raw == "50-C" ~ "", # guess needed
    dataset == "Hubbard Brook" & depth_raw == "C0-25" ~ "0-25",
    dataset == "Hubbard Brook" & depth_raw == "C25+" ~ "", # guess needed
    dataset == "Hubbard Brook" & depth_raw == "C25-50" ~ "25-50",
    dataset == "Hubbard Brook" & depth_raw == "C50+" ~ "", # guess needed
    ## Jornada
    dataset == "Jornada_1" & depth_raw == "5" ~ "0-10",
    dataset == "Jornada_1" & depth_raw == "15" ~ "10-20",
    ## KBS
    dataset == "Kellogg_Bio_Station" ~ "0-2",
    ## Konza
    dataset == "Konza_1" & depth_raw == "81+" ~ "81-91", # guessing all KNZ are 10 cm cores
    dataset == "Konza_1" & depth_raw == "192+" ~ "192-202", 
    dataset == "Konza_1" & depth_raw == "198+" ~ "198-208", 
    dataset == "Konza_1" & depth_raw == "221+" ~ "221-231",
    dataset == "Konza_2" & depth_raw == "5" ~ "0-5",
    ## Luquillo (1)
    ### No ranges so we'll just add a constant to every depth value to get the end of the range
    dataset == "Luquillo_1" & stringr::str_detect(string = depth_raw, pattern = "-") != T ~ paste0(depth_raw, "-", suppressWarnings(as.numeric(depth_raw)) + 10),
    ## Luquillo (2)
    dataset == "Luquillo_2" & depth_raw == "1" ~ "0-2", 
    dataset == "Luquillo_2" & depth_raw == "2" ~ "2-10", 
    ## Niwot (3)
    dataset == "Niwot_3" & depth_raw == "10" ~ "0-10", 
    dataset == "Niwot_3" & depth_raw == "20" ~ "10-20",
    ## Niwot (4)
    dataset == "Niwot_4" ~ paste0("0-", (as.numeric(depth_m) * 100)),
    ## Sevilleta (1)
    dataset == "Sevilleta_1" ~ "0-10",
    ## Seviletta (2)
    dataset == "Sevilleta_2" & depth_raw == "10" ~ "0-10", 
    dataset == "Sevilleta_2" & depth_raw == "20" ~ "10-20", 
    dataset == "Sevilleta_2" & depth_raw == "30" ~ "20-30", 
    ## Otherwise raw depth assumed to be a functioning range
    TRUE ~ depth_raw), .after = depth_raw) %>%
  # Drop now-unneeded BNZ depth column
  dplyr::select(-org.depth_cm, -depth_m)

# Re-check for malformed depth ranges
tidy_v2c %>%
  dplyr::filter(stringr::str_detect(string = depth_range_raw, pattern = "-") != T) %>%
  dplyr::select(dataset, raw_filename, depth_raw, depth_range_raw, horizon_raw) %>%
  dplyr::distinct()

# Check structure again
tidy_v2c %>%
  dplyr::select(dataset:core, dplyr::contains("horizon"),
                dplyr::contains("depth")) %>%
  dplyr::select(-dplyr::contains(".by.depth")) %>%
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

# JUDGEMENT CALL NOTE:
## If depth is completely missing we need a filler value for core length (in cm)
core_lng <- 10

# Now can do numeric wrangling of depth columns
tidy_v2e <- tidy_v2d %>%
  # Do any needed fixes of non-numbers
  ## None needed currently
  # Make the depth columns numeric
  dplyr::mutate(depth_1 = as.numeric(depth_1),
                depth_2 = as.numeric(depth_2)) %>%
  # Now that all depths are numbers we can figure out start and end depths
  dplyr::mutate(depth.start_cm = ifelse(depth_1 < depth_2, 
                                        yes = depth_1, no = depth_2),
                depth.end_cm = ifelse(depth_2 > depth_1, 
                                      yes = depth_2, no = depth_1),
                .after = depth_range_raw) %>%
  # Drop intermediary columns and old raw depth columns
  dplyr::select(-depth_1, -depth_2, -depth_raw, -depth_range_raw) %>%
  # Calculate length of core as well
  dplyr::mutate(core.length_cm = ifelse(!is.na(depth.end_cm) & !is.na(depth.start_cm),
                                        yes = depth.end_cm - depth.start_cm,
                                        no = core_lng),
                .after = depth.end_cm) %>%
  # Move these columns to the left
  dplyr::relocate(depth.start_cm, depth.end_cm, core.length_cm, 
                  .after = horizon_raw)

# Check distribution of the new depth columns we just extracted
# hashtagged out these histograms because on 03/06/24 the code kept breaking here with the following error, "Error in par(old.par) : invalid value specified for graphical parameter "pin""
# psych::multi.hist(x = tidy_v2e$depth.start_cm)
# psych::multi.hist(x = tidy_v2e$depth.end_cm)
# psych::multi.hist(x = tidy_v2e$core.length_cm)

# Check structure yet again
tidy_v2e %>%
  dplyr::select(dataset:core, dplyr::contains("horizon"), dplyr::contains("depth")) %>%
  dplyr::select(-dplyr::contains(".by.depth")) %>%
  dplyr::glimpse()

# Wrangle horizon information to get other desired facets of that variable
tidy_v2f <- tidy_v2e %>%
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
    dataset == "Bonanza Creek_1" & depth.start_cm == 0 ~ "O",
    dataset == "Bonanza Creek_1" & depth.start_cm != 0 ~ "mineral",
    dataset == "Bonanza Creek_2" ~ "mixed",
    dataset %in% c("Brazil_SouthernAmazon", "Brazil_AtlanticForest", "Calhoun", "CedarCreek_1", "CedarCreek_2", "Coweeta", "Jornada_1", "Jornada_2", "Kellogg_Bio_Station", 
                   "Luquillo_1", "Luquillo_2", "Luquillo_3", 
                   "Niwot_1", "Niwot_2", "Niwot_3", "Niwot_4", 
                   "Sevilleta_1", "Sevilleta_2", "HJAndrews_1", "Niwot_5", "Konza_2", "Tapajos"
                   ) ~ "mineral", # Need to double check Brazil & Calhoun
    dataset == "FloridaCoastal" & horizon_raw == "mixed" ~ "mixed",
    dataset == "FloridaCoastal" & horizon_raw == "hurricane_sediment" ~ "hurricane_sediment",
    ### dataset == "HJAndrews_1" ~ "",
    # If not in data and not known, fill with NA
    T ~ NA), .after = horizon_raw) %>%
  # Identify the source of this information
  dplyr::mutate(horizon_source = dplyr::case_when(
    # If in data say that
    !is.na(horizon_raw) & nchar(horizon_raw) != 0 ~ "in data",
    !is.na(depth_horizon) & nchar(depth_horizon) != 0 ~ "in data",
    # If filled conditionally, enter that 
    dataset %in% c("Bonanza Creek_1", "Bonanza Creek_2" ,
                   "Brazil_SouthernAmazon","Brazil_AtlanticForest", "Calhoun", "CedarCreek_1", "CedarCreek_2",
                   "Coweeta", "FloridaCoastal", "Jornada_1", "Jornada_2",
                   "Kellogg_Bio_Station", "Luquillo_1", "Luquillo_2", "Luquillo_3", 
                   "Niwot_1", "Niwot_2", "Niwot_3", "Niwot_4", 
                   "Sevilleta_1", "Sevilleta_2"
    ) ~ "expert knowledge", # Need to double check Brazil, Calhoun, and CDR
    ### dataset == "HJAndrews_1" ~ "",
    # If no horizon information in this column, the source is NA
    is.na(horizon_actual) ~ NA,
    # Otherwise fill with NA
    T ~ NA), .after = horizon_actual) %>%
  # Create a 'mineral vs. organic' horizon column
  dplyr::mutate(horizon_binary = dplyr::case_when(
    horizon_actual %in% c("organic", "O", "Oi", "Oe", "Oa", "o", "oi", "oe", "oa") ~ "organic",
    horizon_actual %in% c("mineral", "A", "B", "C", "AEB") ~ "mineral",
    horizon_actual == "h" ~ "hurricane",
    horizon_actual == "m" ~ "mixed",
    T ~ NA), .after = horizon_source) %>%
  # Drop depth horizon column and original (un-tidied) horizon column
  dplyr::select(-depth_horizon, -horizon_simp) %>%
  # Rename tidied horizon column
  dplyr::rename(horizon = horizon_actual)

# For which datasets is horizon info *absent* (that could be filled by expert knowledge)?
tidy_v2f %>% 
  dplyr::filter(is.na(horizon) | nchar(horizon) == 0) %>%
  dplyr::select(dataset, raw_filename, horizon) %>%
  dplyr::distinct()

# Fix issue with Toolik (Toolik_1) depths
tidy_v3 <- tidy_v2f %>%
  # Need to update only mineral layer depth start/end
  dplyr::mutate(depth.end_cm = ifelse(dataset == "Toolik_1" & horizon == "mineral",
                                      yes = depth.end_cm - depth.start_cm,
                                      no = depth.end_cm)) %>%
  # Once subtraction from end is done, change start to 0
  dplyr::mutate(depth.start_cm = ifelse(dataset == "Toolik_1" & horizon == "mineral",
                                        yes = 0, no = depth.start_cm))

# Adding start and end depths for Niwot 1 
tidy_v3 <- tidy_v3 %>% 
  dplyr::mutate(depth.end_cm = ifelse(dataset == "Niwot_1",
                                      10,
                                      depth.end_cm))

tidy_v3 <- tidy_v3 %>% 
  dplyr::mutate(depth.start_cm = ifelse(dataset == "Niwot_1",
                                      0,
                                      depth.start_cm))

# Check contents of the specific horizon columns
sort(unique(tidy_v3$horizon_raw))
sort(unique(tidy_v3$horizon))
sort(unique(tidy_v3$horizon_source))
sort(unique(tidy_v3$horizon_binary))

# Ad nauseam at this point but check structure
tidy_v3 %>%
  dplyr::select(dataset:core, dplyr::contains("horizon"), dplyr::contains("depth"),
                core.length_cm) %>%
  dplyr::select(-dplyr::contains(".by.depth")) %>%
  dplyr::glimpse()

# Check again for column order
dplyr::glimpse(tidy_v3[1:22])

## ------------------------------------------ ##
          # Numeric Column Checks ----
## ------------------------------------------ ##

# Before we can continue, we need to make all columns that should be numeric actually be numeric
## Can also handle the 'sample replicate' rows once our response values are numbers

# Reshape into long format to make a single column to check for non-numbers
tidy_v3b <- tidy_v3 %>%
  tidyr::pivot_longer(cols = -lter:-core.length_cm,
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
    value_clean %in% c("M", ".", "NaN", "NA000", "0_10", "hurricane_sediment") ~ NA,
    nchar(value_clean) == 0 ~ NA,
    ## Handle 'less than' indications (note judgement call)
    value_clean == "< 0.5" ~ "0.25",
    ## Otherwise keep the value as it is
    T ~ value_clean))

# Make sure all numbers are 'good' numbers
supportR::num_check(data = tidy_v3c, col = "value_actual")

# Identify names of all columns except for sample replicate / old values columns
(keeps <- setdiff(x = names(tidy_v3c), y = c("sample.replicate", "sample.replicate.2",
                                             "value_raw", 
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
  dplyr::relocate(pH, .after = core.length_cm) %>%
  # Group C/N columns together
  dplyr::relocate(dplyr::contains("_data.type_"), .after = pH) %>% 
  dplyr::relocate(dplyr::starts_with("No_"), .after = pH) %>%  
  dplyr::relocate(dplyr::starts_with("N_stock"), .after = pH) %>%
  dplyr::relocate(dplyr::starts_with("N_conc"), .after = pH) %>%
  dplyr::relocate(dplyr::starts_with("Co_"), .after = pH) %>%  
  dplyr::relocate(dplyr::starts_with("Ci_"), .after = pH) %>%
  dplyr::relocate(dplyr::starts_with("C_stock"), .after = pH) %>%
  dplyr::relocate(dplyr::starts_with("C_conc"), .after = pH) %>%
  # Move P fractions to the right
  dplyr::relocate(dplyr::contains("P_data.type_"),
                  dplyr::starts_with("P_conc"), dplyr::starts_with("P_stock"), 
                  dplyr::starts_with("Po_conc"), dplyr::starts_with("Po_stock"), 
                  dplyr::starts_with("Pi_conc"), dplyr::starts_with("Pi_stock"),
                  .after = dplyr::everything())

# Re-check structure
dplyr::glimpse(tidy_v4)

# Check to see if any columns were lost/gained (should only be 'sample_replicate' lost)
supportR::diff_check(old = names(tidy_v3), new = names(tidy_v4))
## I checked and 'depth_units' is a one-off oddball column we can safely ignore

## ------------------------------------------ ##
        # Bulk Density / Soil Fixes ----
## ------------------------------------------ ##

# Wrangling for soil information
tidy_v5 <- tidy_v4 %>%
  # Relocate soil columns to the left
  dplyr::relocate(dplyr::contains("bulk"), coarse.volume_percent, 
                  dplyr::contains("soil"),
                  .after = pH) %>%
  # Rename the bulk density columns
  dplyr::rename(bulk.density_kg.ha = bulk.density.P_kg.ha,
                bulk.density_g.cm3_raw = bulk.density_g.cm3) %>%
  #  We're hard coding bulk density in here rather than typing manually
  ## Citations/justifications are included next to each bulk density value
  dplyr::mutate(bulk_density = dplyr::case_when(
  # If bulk density was provided, use that instead of doing conditionals
    !is.na(bulk.density_g.cm3_raw) & 
      nchar(bulk.density_g.cm3_raw) != 0 ~ as.numeric(bulk.density_g.cm3_raw),
    dataset == "HJAndrews_1" ~ 0.9,
    dataset == "Bonanza Creek_1" ~ 0.9,
    dataset == "Bonanza Creek_2" ~ 0.9,
    dataset == "Bonanza Creek_3" ~ 0.9,
    dataset == "Brazil_SouthernAmazon" ~ 0.9,
    dataset == "Brazil_AtlanticForest" ~ 0.9,
    dataset == "Calhoun" ~ 0.9,
    dataset == "CedarCreek_1" ~ 0.9,
    dataset == "CedarCreek_2" ~ 0.9,
    dataset == "Coweeta" ~ 0.9,
    dataset == "Fernow" ~ 0.9,
    dataset == "FloridaCoastal" ~ 0.9,
    dataset == "Hubbard Brook" ~ 0.9,
    dataset == "Jornada_1" ~ 0.9,
    dataset == "Kellogg_Bio_Station" ~ 0.9,
    dataset == "Konza_1" ~ 0.9,
    dataset == "Konza_2" ~ 0.9,
    dataset == "Luquillo_1" ~ 0.9,
    dataset == "Luquillo_2" ~ 0.9,
    dataset == "Luquillo_3" ~ 0.9,
    dataset == "Niwot_1" ~ 0.9,
    dataset == "Niwot_2" ~ 0.9,
    dataset == "Niwot_3" ~ 0.9,
    dataset == "Niwot_4" ~ 0.9,
    dataset == "Niwot_5" ~ 0.9,
    dataset == "Niwot_Liptzen2006" ~ 0.9,
    dataset == "Sevilleta_1" ~ 0.9,
    dataset == "Sevilleta_2" ~ 0.9,
    dataset == "Tapajos" ~ 0.9,
    dataset == "Toolik_1" ~ 0.9,
    dataset == "Toolik_2" ~ 0.9,
    # If no bulk density is supplied by above conditions, fill with NA
    TRUE ~ NA), .after = bulk.density_g.cm3_raw) %>%
  # Drop old column and rename remaining one to avoid confusion
  dplyr::select(-bulk.density_g.cm3_raw) %>%
  dplyr::rename(bulk.density_g.cm3 = bulk_density)

# Check whether we're missing any bulk density values
## If so, need to add another conditional to the above `case_when`
tidy_v5 %>%
  dplyr::filter(is.na(bulk.density_g.cm3)) %>%
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
  dplyr::select(dataset, dplyr::starts_with("N_"), dplyr::starts_with("C_"),
                dplyr::starts_with("No_"), dplyr::starts_with("Co_"),
                dplyr::starts_with("Ni_"), dplyr::starts_with("Ci_")) %>%
  dplyr::glimpse()

# Convert N & C concentrations into percentages
tidy_v5b <- tidy_v5 %>%
  # Standardize Nitrogen concentration units
  dplyr::mutate(N_conc_actual = dplyr::case_when(
    ## Percent
    !is.na(N_conc_percent) ~ N_conc_percent,
    ## _g / _g
    !is.na(N_conc_mg.kg) ~ N_conc_mg.kg * 0.0001,
    !is.na(N_conc_mg.g) ~ N_conc_mg.g * 0.1,
    !is.na(N_conc_g.kg) ~ N_conc_g.kg * 0.1,
    # If nothing exists, fill with NA
    TRUE ~ NA), .before = N_conc_percent) %>%
  # Do the same for Carbon
  dplyr::mutate(C_conc_actual = dplyr::case_when(
    ## Percent
    !is.na(C_conc_percent) ~ C_conc_percent,
    ## _g / _g
    !is.na(C_conc_mg.kg) ~ C_conc_mg.kg * 0.0001,
    !is.na(C_conc_mg.g) ~ C_conc_mg.g * 0.1,
    !is.na(C_conc_g.kg) ~ C_conc_g.kg * 0.1,
    TRUE ~ NA), .before = C_conc_percent) %>%
  # Drop now-superseded columns
  dplyr::select(-N_conc_percent, -N_conc_mg.kg, -N_conc_mg.g, -N_conc_g.kg,
                -C_conc_percent, -C_conc_mg.kg, -C_conc_mg.g, -C_conc_g.kg) %>%
  # Rename combined columns for clarity and to maintain snake_case
  dplyr::rename(N_conc_percent = N_conc_actual,
                C_conc_percent = C_conc_actual)

# How many NAs did we fill for Nitrogen?
summary(tidy_v5$N_conc_percent); summary(tidy_v5b$N_conc_percent)

# Check Carbon in the same way
summary(tidy_v5$C_conc_percent); summary(tidy_v5b$C_conc_percent)

# Check remaining columns
tidy_v5b %>%
  dplyr::select(dataset, dplyr::starts_with("N_"), dplyr::starts_with("C_"),
                dplyr::starts_with("No_"), dplyr::starts_with("Co_"),
                dplyr::starts_with("Ni_"), dplyr::starts_with("Ci_")) %>% 
  dplyr::glimpse()

# Also convert organic/inorganic N/C variants
tidy_v5c <- tidy_v5b %>%
  ## Organic N
  dplyr::mutate(No_conc_percent = (No_conc_g.kg / 10^3) * 0.0001,
                .after = N_conc_percent) %>%
  ## Organic C
  dplyr::mutate(Co_conc_percent = dplyr::case_when(
    !is.na(Co_conc_percent) ~ Co_conc_percent,
    !is.na(Co_conc_g.kg) ~ (Co_conc_g.kg / 10^3) * 0.0001,
    T ~ NA), .after = C_conc_percent) %>%
  ## Inorganic C
  dplyr::mutate(Ci_conc_percent = dplyr::case_when(
    !is.na(Ci_conc_percent) ~ Ci_conc_percent,
    !is.na(Ci_conc_g.kg) ~ (Ci_conc_g.kg / 10^3) * 0.0001,
    T ~ NA), .after = C_conc_percent) %>%
  # Drop old variants of columns
  dplyr::select(-No_conc_g.kg, -Co_conc_g.kg, -Ci_conc_g.kg)

# Check it
tidy_v5c %>%
  dplyr::select(dataset, dplyr::starts_with("N_"), dplyr::starts_with("C_"),
                dplyr::starts_with("No_"), dplyr::starts_with("Co_"),
                dplyr::starts_with("Ni_"), dplyr::starts_with("Ci_")) %>% 
  dplyr::glimpse()

# Now let's handle different units for stock
tidy_v6 <- tidy_v5c %>%
  # Convert Nitrogen stock into one unit (mg/m2)
  dplyr::mutate(N_stock_actual = dplyr::case_when(
    !is.na(N_stock_mg.m2) ~ N_stock_mg.m2,
    !is.na(N_stock_g.m2) ~ (N_stock_g.m2 / 10^3),
    T ~ NA), .after = N_conc_percent) %>%
  # Convert Carbon stocks too
  dplyr::mutate(C_stock_actual = dplyr::case_when(
    !is.na(C_stock_mg.m2) ~ C_stock_mg.m2,
    !is.na(C_stock_g.m2) ~ (C_stock_g.m2 / 10^3),
    T ~ NA), .after = C_conc_percent) %>%
  # Drop now-superseded columns
  dplyr::select(-N_stock_mg.m2, -N_stock_g.m2,
                -C_stock_mg.m2, -C_stock_g.m2) %>%
  # Rename remaining columns for clarity
  dplyr::rename(N_stock_mg.m2 = N_stock_actual,
                C_stock_mg.m2 = C_stock_actual) %>%
  # Relocate N/C 'by depth' columns to be near these others
  dplyr::relocate(dplyr::contains(".by.depth"), .after = N_stock_mg.m2)

# How many NAs did we fill for Nitrogran?
summary(tidy_v5c$N_stock_mg.m2); summary(tidy_v6$N_stock_mg.m2)

# Check Carbon in the same way
summary(tidy_v5c$C_stock_mg.m2); summary(tidy_v6$C_stock_mg.m2)

# Check remaining columns' structure again
tidy_v6 %>%
  dplyr::select(dataset, dplyr::starts_with("N_"), dplyr::starts_with("C_"),
                dplyr::starts_with("No_"), dplyr::starts_with("Co_"),
                dplyr::starts_with("Ni_"), dplyr::starts_with("Ci_")) %>% 
  dplyr::glimpse()

# Check structure of more columns
dplyr::glimpse(tidy_v6[c(1:10, 25:35)])

## ------------------------------------------ ##
      # P Fraction Unit Conversions ----
## ------------------------------------------ ##

# This is simpler if we rotate into long format first
tidy_v7 <- tidy_v6 %>%
  # Make a row number column to regenerate this structure easily
  dplyr::mutate(row_num = 1:nrow(.), .before = dplyr::everything()) %>%
  # Rotate into long format
  tidyr::pivot_longer(cols = c(dplyr::starts_with("P_"),
                               dplyr::starts_with("Po_"),
                               dplyr::starts_with("Pi_")),
                      names_to = "p_info", values_to = "value") %>%
  # Drop missing measurements
  dplyr::filter(!is.na(value)) %>%
  # Break the old column names into their component parts
  tidyr::separate_wider_delim(cols = p_info, delim = "_", 
                              names = c("p_type", "measurement", "units",
                                        "order", "molarity", "reagent",
                                        "time", "temp")) %>%
  # Drop filler content where we added it earlier
  dplyr::mutate(dplyr::across(.cols = measurement:temp,
                              .fns = ~ gsub(pattern = "units|data.type|order|reagent|molarity|time|temp",
                                            replacement = NA, x = .x)))

# Glance at structure
dplyr::glimpse(tidy_v7)

# Check units in the data
sort(unique(tidy_v7$units))

# In this format we can _much_ more easily do our unit conversions!
## In part because units are semi-independent of rest of context for the value
## This step is converting all P measurements into mg/kg units 
tidy_v8a <- tidy_v7 %>%
  # Start with mass / mass (i.e., concentration)
  ## Conditional on units, do appropriate algebra
  dplyr::mutate(value = dplyr::case_when(
    units == "g.kg" ~ (value * 10^3),
    units == "mg.g" ~ (value * 10^3),
    units == "percent" ~ (value * 10^4),
    T ~ value)) %>%
  ## Once done, update the units column to reflect the conversion
  dplyr::mutate(units = dplyr::case_when(
    units == "g.kg" ~ "mg.kg",
    units == "mg.g" ~ "mg.kg",
    units == "ppm" ~ "mg.kg", # ppm = mg/kg so no conversion needed
    units == "percent" ~ "mg.kg",
    units == "ug.g" ~ "mg.kg", # ug/g = mg/kg so no conversion needed
    T ~ units)) %>%
  # Do the same for mass / area (i.e., stock)
  dplyr::mutate(value = dplyr::case_when(
    units == "g.m2" ~ (value * 10^3),
    units == "kg.ha" ~ ((value * 10^-4) * 10^6), ## 10^4 m2 / 1 ha
    T ~ value)) %>%
  dplyr::mutate(units = dplyr::case_when(
    units == "g.m2" ~ "mg.m2",
    units == "kg.ha" ~ "mg.m2",
    T ~ units))

# What units are in the data now?
sort(unique(tidy_v8a$units))

# It is possible that concentration was measured in mg/kg _and_ provided as a %
## Would need to standardize those before continuing
tidy_v8b <- tidy_v8a %>%
  # Group by everything but value
  dplyr::group_by(dplyr::across(.cols = -value)) %>%
  dplyr::summarize(value = mean(value, na.rm = T)) %>%
  dplyr::ungroup()

# If there were any duplicates, we would lose rows
nrow(tidy_v8a); nrow(tidy_v8b)

# Re-check full data structure
dplyr::glimpse(tidy_v8b)

## ------------------------------------------ ##
          # Export Archival Data ----
## ------------------------------------------ ##

# Create a final data object
final_tidy <- tidy_v8b

# Check its structure
dplyr::glimpse(final_tidy)

# And identify the tidy data Drive URL
tidy_drive <- googledrive::as_id("https://drive.google.com/drive/u/0/folders/1pjgN-wRlec65NDLBvryibifyx6k9Iqy9")

# Define the file name for this harmonized/tidied data file
arch_name <- "sparc-soil-p_archival-data.csv"

# Export locally
write.csv(x = final_tidy, row.names = F, na = '',
          file = file.path("data", "tidy_data", arch_name))

# Upload that to the Drive as well
googledrive::drive_upload(media = file.path("data", "tidy_data", arch_name), 
                          overwrite = T, path = tidy_drive)

# End ----
