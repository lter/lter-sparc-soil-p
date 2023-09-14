## ------------------------------------------ ##
         # SPARC Soil P -- Statistics
## ------------------------------------------ ##
# Script author(s): Nick J Lyon

# Purpose:
## Perform needed pre-stats wrangling and statistical analysis
## Export / display summaries of tests

## ------------------------------------------ ##
              # Housekeeping -----
## ------------------------------------------ ##

# Load necessary libraries
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive, scicomptools, supportR)

# Create necessary sub-folder(s)
dir.create(path = file.path("tidy_data"), showWarnings = F)
dir.create(path = file.path("stat_results"), showWarnings = F)

# Clear environment
rm(list = ls())

# Identify and download the tidied megadata object from the Drive
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1pjgN-wRlec65NDLBvryibifyx6k9Iqy9")) %>%
  dplyr::filter(name == "tidy_soil_p.csv") %>%
  googledrive::drive_download(file = .$id, path = file.path("tidy_data", .$name), overwrite = T)

# Identify Drive folder to upload stat results
stat_drive <- googledrive::as_id("https://drive.google.com/drive/u/0/folders/1it7t9b3JF9V2Tdnt10lR130-CLs0Nxub")

## ------------------------------------------ ##
          # Pre-Stats Wrangling ----
## ------------------------------------------ ##

# Read in full megadata
mega <- read.csv(file.path("tidy_data", "tidy_soil_p.csv"))

# Check structure
dplyr::glimpse(mega)

# Depth subsetting is used to restrict core depth of samples used in analysis
## Decide how many centimeters from the first measured depth (of the A horizon) are allowed
depth_cutoff <- 15

# Megadata includes *a lot* of information and we only really need a subset of it for stats
stat_df <- mega %>%
  # Pare down to only columns of interest
  ## Unspecified columns are implicitly removed
  dplyr::select(lter, dataset, site, plot, block, core, dplyr::starts_with("treatment"),
                dplyr::starts_with("horizon"), dplyr::starts_with("depth."),
                core.length_cm, pH, dplyr::starts_with("bulk.density"),
                dplyr::starts_with("slow.P"), dplyr::starts_with("total.P"),
                C_conc_percent, N_conc_percent) %>%
  # Drop non-unique rows
  dplyr::distinct() %>%
  # Only interested in mineral layer
  ## Assuming that un-specified horizons are mineral layer
  dplyr::filter(horizon_binary == "mineral" | nchar(horizon_binary) == 0) %>%
  # Identify minimum depth of remaining data within sample info column groups
  dplyr::group_by(dplyr::across(c(lter:block, treatment:treatment.years))) %>%
  dplyr::mutate(min_depth = ifelse(!all(is.na(depth.start_cm)),
                                   yes = min(depth.start_cm, na.rm = T),
                                   no = NA),
                max_allowed_depth = (min_depth + depth_cutoff),
                .after = horizon_binary) %>%
  dplyr::ungroup() %>%
  # Filter to only samples in that range
  dplyr::filter(depth.start_cm >= min_depth & depth.end_cm <= max_allowed_depth) %>%
  # Drop columns needed for that filter but otherwise not needed
  dplyr::select(-min_depth, -max_allowed_depth) %>%
  # Now drop any columns that don't have at least one value
  dplyr::select(dplyr::where(~ !(all(is.na(.)) | all(. == ""))))

# Check to make sure we're okay with the columns we dropped
supportR::diff_check(old = names(mega), new = names(stat_df), sort = F)

# Check out the structure of the data
dplyr::glimpse(stat_df)

# Now create a version of this that is averaged within sites
site_avgs <- stat_df %>%
  # Drop depth columns so we can reshape to get averages
  dplyr::select(-dplyr::contains("depth")) %>%
  # Flip all numeric variables into long format
  tidyr::pivot_longer(cols = -lter:-horizon_binary,
                      names_to = 'variables',
                      values_to = 'values') %>%
  # Average within site-level information columns
  ## Note we'll implicitly drop any column not used in grouping or created by `summarize`
  dplyr::group_by(lter, dataset, site, treatment, treatment.years, variables) %>%
  dplyr::summarize(mean_val = mean(values, na.rm = T)) %>%
  dplyr::ungroup() %>%
  # Flip back to wide format
  tidyr::pivot_wider(names_from = variables, values_from = mean_val)

# Glimpse it
dplyr::glimpse(site_avgs)

## ------------------------------------------ ##
          # Custom Function(s) ----
## ------------------------------------------ ##

# Write a neat little custom function for extracting model outputs
mod_strip <- function(fit, resp, exp){
  
  # Strip out the summary stats for the table
  stat_table <- scicomptools::stat_extract(mod_fit = fit)
  
  # Add on new desired columns
  stat_table_v2 <- stat_table %>%
    dplyr::mutate(Response_Var = resp,
                  Explanatory_Var = exp,
                  .before = dplyr::everything())
  
  # Return that
  return(stat_table_v2) }

## ------------------------------------------ ##
         # Across-Site Statistics ----
## ------------------------------------------ ##

# Begin by fitting the four models of interest
xsite_N_totP_fit <- lm(N_conc_percent ~ total.P_conc_mg.kg, data = site_avgs)
xsite_C_totP_fit <- lm(C_conc_percent ~ total.P_conc_mg.kg, data = site_avgs)
xsite_N_slowP_fit <- lm(N_conc_percent ~ slow.P_conc_mg.kg, data = site_avgs)
xsite_C_slowP_fit <- lm(C_conc_percent ~ slow.P_conc_mg.kg, data = site_avgs)

# Strip out the summary statistics
xsite_N_totP_table <- mod_strip(fit = xsite_N_totP_fit, resp = "N Percent", 
                                exp = "Total P mg/kg")
xsite_C_totP_table <- mod_strip(fit = xsite_C_totP_fit, resp = "C Percent", 
                                exp = "Total P mg/kg")
xsite_N_slowP_table <- mod_strip(fit = xsite_N_slowP_fit, resp = "N Percent", 
                                 exp = "Slow P mg/kg")
xsite_C_slowP_table <- mod_strip(fit = xsite_C_slowP_fit, resp = "C Percent", 
                                 exp = "Slow P mg/kg")

# Bind them together for ease of exporting
xsite_outs <- xsite_N_totP_table %>%
  dplyr::bind_rows(xsite_C_totP_table, xsite_N_slowP_table, xsite_C_slowP_table)

# Check that out
xsite_outs

## ------------------------------------------ ##
            # Across-Site Export ----
## ------------------------------------------ ##

# Make a file path for across site results
xsite_path <- file.path("stat_results", "across-site-results.csv")

# Write that out as a CSV
write.csv(x = xsite_outs, file = xsite_path, row.names = F, na = '')

# Upload to drive
googledrive::drive_upload(media = xsite_path, path = stat_drive, overwrite = T)

## ------------------------------------------ ##
          # Within-Site Statistics ----
## ------------------------------------------ ##

# Make an empty list (for storing outputs)
within_list <- list()

# Loop across LTERs
for(LTER in unique(stat_df$lter)){
  
  # Print starting message
  message("Fitting models for LTER: ", LTER)
  
  # Subset data
  stat_sub <- dplyr::filter(stat_df, lter == LTER)
  
  # Do stats if have data
  ## Placeholder until we finalize data tidying
  if(!all(is.na(stat_sub$N_conc_percent)) & !all(nchar(stat_sub$N_conc_percent) == 0)){
    
    # Fit models
    mod1_fit <- lm(N_conc_percent ~ total.P_conc_mg.kg, data = stat_sub)
    mod3_fit <- lm(N_conc_percent ~ slow.P_conc_mg.kg, data = stat_sub)
    
    # Strip out the summary statistics
    mod1_table <- mod_strip(fit = mod1_fit, resp = "N Percent", exp = "Total P mg/kg")
    mod3_table <- mod_strip(fit = mod3_fit, resp = "N Percent", exp = "Slow P mg/kg")
    
    # Combine and add to list
    within_list[[paste(lter, "N")]] <- dplyr::bind_rows(mod1_table, mod3_table) %>%
      # Add LTER column
      dplyr::mutate(lter = LTER, .before = dplyr::everything())
  } # Close N conditional
  
  # Do stats if have data
  ## Placeholder until we finalize data tidying
  if(!all(is.na(stat_sub$C_conc_percent)) & !all(nchar(stat_sub$C_conc_percent) == 0)){
    
    # Fit models
    mod2_fit <- lm(C_conc_percent ~ total.P_conc_mg.kg, data = stat_sub)
    mod4_fit <- lm(C_conc_percent ~ slow.P_conc_mg.kg, data = stat_sub)
    
    # Strip out stats
    mod2_table <- mod_strip(fit = mod2_fit, resp = "C Percent", exp = "Total P mg/kg")
    mod4_table <- mod_strip(fit = mod4_fit, resp = "C Percent", exp = "Slow P mg/kg")
    
    # Combine and add to list
    within_list[[paste(lter, "N")]] <- dplyr::bind_rows(mod2_table, mod4_table) %>%
      # Add LTER column
      dplyr::mutate(lter = LTER, .before = dplyr::everything())
    } # Close C conditional
} # Close loop

# Unlist the output
within_outs <- purrr::list_rbind(x = within_list)

# Check that out
within_outs

## ------------------------------------------ ##
            # Within-Site Export ----
## ------------------------------------------ ##

# Make a file path for across site results
within_path <- file.path("stat_results", "within-site-results.csv")

# Write that out as a CSV
write.csv(x = within_outs, file = within_path, row.names = F, na = '')

# Upload to drive
googledrive::drive_upload(media = within_path, path = stat_drive, overwrite = T)

# End ----
