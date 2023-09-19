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

# Identify the needed data file(s) in the Drive
( file_ids <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1pjgN-wRlec65NDLBvryibifyx6k9Iqy9")) %>%
  dplyr::filter(name %in% c("stats-ready_tidy-soil-p.csv", "site-avgs_tidy-soil-p.csv")) )

# Download those files
purrr::walk2(.x = file_ids$id, .y = file_ids$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T, 
                                                path = file.path("tidy_data", .y)))
  
# Clear environment
rm(list = ls())

# Identify Drive folder to upload stat results
stat_drive <- googledrive::as_id("https://drive.google.com/drive/u/0/folders/1it7t9b3JF9V2Tdnt10lR130-CLs0Nxub")

# Clear environment
rm(list = ls())

# Read in stats/viz-ready file
main_df <- read.csv(file.path("tidy_data", "stats-ready_tidy-soil-p.csv"))

# Check structure
dplyr::glimpse(main_df)

# Read in site averages as well
avgs_df <- read.csv(file.path("tidy_data", "site-avgs_tidy-soil-p.csv"))

# Check structure
dplyr::glimpse(avgs_df)

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
xsite_N_totP_fit <- lm(N_conc_percent ~ total.P_conc_mg.kg, data = avgs_df)
xsite_C_totP_fit <- lm(C_conc_percent ~ total.P_conc_mg.kg, data = avgs_df)
xsite_N_slowP_fit <- lm(N_conc_percent ~ slow.P_conc_mg.kg, data = avgs_df)
xsite_C_slowP_fit <- lm(C_conc_percent ~ slow.P_conc_mg.kg, data = avgs_df)

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
    within_list[[paste(LTER, "N")]] <- dplyr::bind_rows(mod1_table, mod3_table) %>%
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
    within_list[[paste(LTER, "C")]] <- dplyr::bind_rows(mod2_table, mod4_table) %>%
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
