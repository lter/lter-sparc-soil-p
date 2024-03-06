## ------------------------------------------ ##
      # SPARC Soil P -- Stats/Viz Prep
## ------------------------------------------ ##
# Script author(s): Nick J Lyon

# Purpose:
## Take complete data (i.e., with P sums, ancillary data, and spatial info)
## And create subset(s) relevant to hypotheses

## ------------------------------------------ ##
            # Housekeeping -----
## ------------------------------------------ ##

# Load necessary libraries
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive, supportR)

# Create necessary sub-folder(s)
dir.create(path = file.path("data", "tidy_data"), showWarnings = F)
dir.create(path = file.path("data", "stats_ready"), showWarnings = F)

# Clear environment
rm(list = ls())

# Identify needed tidy file(s)
tidy_drive <- googledrive::as_id("https://drive.google.com/drive/u/0/folders/1pjgN-wRlec65NDLBvryibifyx6k9Iqy9")

# Identify the archival data in that folder and download it
googledrive::drive_ls(path = tidy_drive) %>%
  dplyr::filter(name == "sparc-soil-p_full-plus-ancil-and-spatial.csv") %>%
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "tidy_data", .$name))

# Read that file in
all_v1 <- read.csv(file = file.path("data", "tidy_data", 
                                    "sparc-soil-p_full-plus-ancil-and-spatial.csv"))

# un <- as.tibble(unique(all_v1$dataset))

# Glimpse it!
dplyr::glimpse(all_v1)

## ------------------------------------------ ##
           # Slim Down Columns -----
## ------------------------------------------ ##

# Only some columns are really needed from here on out
all_v2 <- all_v1 %>%
  dplyr::select(lter:core, treatment, dplyr::starts_with("rock_"), soil_code, generic_soil,
                dplyr::starts_with("mean.annual."), pH,
                dplyr::starts_with("horizon"), dplyr::starts_with("depth."),
                core.length_cm, bulk.density_g.cm3,
                dplyr::starts_with("Al_"), dplyr::starts_with("Fe"),
                dplyr::ends_with(".P_conc_mg.kg"),
                C_conc_percent, N_conc_percent) %>%
  # Drop non-unique rows (shouldn't be any but you never know)
  dplyr::distinct()

# See what we lost and make sure you're cool with that
supportR::diff_check(old = names(all_v1), new = names(all_v2), sort = T)

# Look at what you still kept
dplyr::glimpse(all_v2)

un <- as.tibble(unique(all_v2$dataset))

## ------------------------------------------ ##
       # Handle Luquillo_2 Core Issue ----
## ------------------------------------------ ##

# Need to do a weighted average across 0-2 and 2-10 depth cores
luq2_v1 <- all_v2 %>%
  # Subset as needed
  dplyr::filter(dataset_simp == "LUQ_2")

# Calculate across 0-2 and 2-10 depths into a single value
luq2_v2 <- luq2_v1 %>%
  # Flip to long format
  tidyr::pivot_longer(cols = -lter:-bulk.density_g.cm3) %>%
  # Do needed weighted calculation
  dplyr::mutate(value = case_when(
    depth.start_cm == 0 & depth.end_cm == 2 ~ value * 0.2,
    depth.start_cm == 2 & depth.end_cm == 10 ~ value * 0.8,
    T ~ value)) %>%
  # Fix depth values and core lengths
  dplyr::mutate(core.length_cm = dplyr::case_when(
    depth.start_cm == 0 & depth.end_cm == 2 ~ 10,
    depth.start_cm == 2 & depth.end_cm == 10 ~ 10,
    T ~ core.length_cm)) %>%
  dplyr::mutate(depth.end_cm = ifelse(depth.start_cm == 0 & depth.end_cm == 2,
                                      yes = 10, no = depth.end_cm)) %>%
  dplyr::mutate(depth.start_cm = ifelse(depth.start_cm == 2 & depth.end_cm == 10,
                                        yes = 0, no = depth.start_cm)) %>%
  # Sum our resolved depths
  dplyr::group_by(dplyr::across(.cols = -value)) %>%
  dplyr::summarize(value = sum(value, na.rm = T)) %>%
  dplyr::ungroup() %>%
  # Reshape wider
  tidyr::pivot_wider(names_from = name, values_from = value)

# Check structure
dplyr::glimpse(luq2_v2)
## Should go from 20 rows to 10 rows

# Split off on LUQ_2
not_luq2 <- all_v2 %>%
  dplyr::filter(dataset_simp != "LUQ_2")

# Recombine
all_v3 <- dplyr::bind_rows(luq2_v2, not_luq2)

un <- as.tibble(unique(all_v3$dataset))

## ------------------------------------------ ##
              # Remove Outliers ----
## ------------------------------------------ ##

# Begin by removing obvious outliers
stats_v1 <- all_v3 %>%
  # KBS has an outrageously high total P value in one spot (~5600 mg/kg)
  dplyr::filter(is.na(total.P_conc_mg.kg) | total.P_conc_mg.kg <= 5250)

# How many rows lost?
nrow(all_v3) - nrow(stats_v1)

## ------------------------------------------ ##
          # Mineral Subsetting ----
## ------------------------------------------ ##

# One set of hypotheses requires only 0-10 cm of mineral layer
mineral_v1 <- stats_v1 %>%
  # Keep only mineral layer (and mixed mineral/organic) data 
  dplyr::filter(horizon_binary %in% c("mineral", "mixed") |
                  # Also keep unspecified horizon information (assumes mineral)
                  nchar(horizon_binary) == 0) %>%
  # For HBR, keep only A horizon
  dplyr::filter((dataset == "Hubbard Brook" & horizon == "A") |
                  dataset != "Hubbard Brook") %>%
  # Coerce empty N/C percents into true NAs
  dplyr::mutate(C_conc_percent = ifelse(nchar(C_conc_percent) == 0,
                                        yes = NA, no = C_conc_percent),
                N_conc_percent = ifelse(nchar(N_conc_percent) == 0,
                                        yes = NA, no = N_conc_percent)) %>%
  # Also we need **either** N or C information in addition to P data for statistics
  dplyr::filter(!is.na(C_conc_percent) | !is.na(N_conc_percent)) # This step removes HJA & KBS because neither has C or N 

un2 <- as.tibble(unique(mineral_v1$dataset))

# How many rows does that lose?
nrow(stats_v1) - nrow(mineral_v1)

# US PLAYING AROUND 
# this is top horizon of each core for BNZ_1
BNZ <- mineral_v1 %>%
  filter(dataset_simp == "BNZ_1") %>%
  group_by(site,plot,block,core) %>% 
  filter(depth.start_cm == min(depth.start_cm))

mineral_v2 <- mineral_v1 %>%
  filter(dataset_simp != "BNZ_1") %>% 
  # Keep only cores beginning at the top of the horizon
  dplyr::filter(depth.start_cm == 0 | dataset == "Toolik_2" | dataset == "Luquillo_3" | dataset == "Niwot_2") # janky but works
  # Again, keep missing depths on assumption they start at 0
  # nchar(depth.start_cm) == 0 | is.na(depth.start_cm) # removing this for now because we feel like we addressed all situations where start depth was 0 or missing

# recombining BNZ_1 mineral cores with the rest of the cores after filtering to keep the top mineral layer in each core (because the start depth for these wasn't zero) 
mineral_v2 <- rbind(mineral_v2,BNZ)

un3 <- as.tibble(unique(mineral_v2$dataset))

# How many rows dropped at that step?
nrow(mineral_v1) - nrow(mineral_v2)

# Adding new treatment type category
## Adding by manually looking at the raw data of each site to remember what each treatment variable means and updating here 

mineral_v2 <- mineral_v2 %>% 
  mutate(treatment = ifelse(treatment == "",NA,treatment)) %>% 
  mutate(treatment_type = ifelse(is.na(treatment),NA,treatment)) %>% # adding a new column called treatment_type that is NA for all sites/observations that don't have treatment filled, but filling in with treatment value for now for those that do have treatment info. Although I'm going to update this manually by site 
  select(lter:treatment,treatment_type,everything())

mineral_v2 <- mineral_v2 %>% 
  mutate(treatment_type = ifelse(dataset_simp == "Calhoun","landscape_position",treatment_type)) %>% 
  mutate(treatment_type = ifelse(dataset_simp == "NWT_1","location",treatment_type)) %>% 
  mutate(treatment_type = ifelse(dataset_simp == "SEV_1","site",treatment_type)) %>% 
  mutate(treatment_type = ifelse(dataset_simp == "HBR","siteage_yrs",treatment_type)) %>% 
  mutate(treatment_type = ifelse(dataset_simp == "LUQ_1","cover",treatment_type)) %>% # not sure if cover is best description for this, the treatment variable is just called treatment in the dataset. Should verify with Whendee what this means. 
  mutate(treatment_type = ifelse(dataset_simp == "LUQ_2","cover",treatment_type)) %>%
  mutate(treatment_type = ifelse(dataset_simp == "Brazil_SA","burn_history",treatment_type)) %>% 
  mutate(treatment_type = ifelse(dataset_simp == "Brazil_AF","hillslope_position",treatment_type)) %>%  
  mutate(treatment_type = ifelse(dataset_simp == "SEV_2","location",treatment_type)) %>% # should distance be a block for this site? 
  mutate(treatment_type = ifelse(dataset_simp == "ARC_1","core_ID",treatment_type)) %>% 
  mutate(treatment_type = ifelse(dataset_simp == "FCE_1","distance",treatment_type)) %>% 
  mutate(treatment_type = ifelse(dataset_simp == "KBS","tillage",treatment_type)) %>% # educated guess, need to double check this  
  mutate(treatment_type = ifelse(dataset_simp == "KBS","tillage",treatment_type)) 

unique(mineral_v2$dataset_simp)

## ------------------------------------------ ##
          # Mineral Export (Local) ----
## ------------------------------------------ ##

# Make a final object
final_mineral <- mineral_v2

# Define the name for this file
mineral_name <- "sparc-soil-p_stats-ready_mineral_0-10.csv"

# Export locally as a CSV
write.csv(x = final_mineral, row.names = F, na = "",
          file = file.path("data", "stats_ready", mineral_name))

## ------------------------------------------ ##
       # Stats-Ready Export (Drive) ----
## ------------------------------------------ ##

# List all 'stats ready' files that we've made
( ready_files <- dir(path = file.path("data", "stats_ready")) )

# Loop across it uploading to Drive
for(focal_ready in ready_files){
  
  googledrive::drive_upload(media = file.path("data", "stats_ready", focal_ready),
                            path = tidy_drive, overwrite = T)
  
}

# End ----
