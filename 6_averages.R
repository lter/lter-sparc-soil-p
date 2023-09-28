## ------------------------------------------ ##
    # SPARC Soil P -- Calculating Averages
## ------------------------------------------ ##
# Script author(s): Nick J Lyon

# Purpose:
## Take all 'stats ready' files and create plot- and site-level averages

## ------------------------------------------ ##
              # Housekeeping -----
## ------------------------------------------ ##

# Load necessary libraries
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive)

# Create necessary sub-folder(s)
dir.create(path = file.path("data", "stats_ready"), showWarnings = F)
dir.create(path = file.path("data", "averages"), showWarnings = F)

# Clear environment
rm(list = ls())

# Identify needed tidy file(s)
tidy_drive <- googledrive::as_id("https://drive.google.com/drive/u/0/folders/1pjgN-wRlec65NDLBvryibifyx6k9Iqy9")

# Identify all files in that folder that are 'stats-ready'
ready_files <- googledrive::drive_ls(path = tidy_drive) %>%
  dplyr::filter(stringr::str_detect(string = name, pattern = "_stats-ready_"))

# Download those files locally
purrr::walk2(.x = ready_files$id, .y = ready_files$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("data", "stats_ready", .y)))

# Identify those files locally
( local_files <- dir(path = file.path("data", "stats_ready")) )

## ------------------------------------------ ##
              # Averaging Steps ----
## ------------------------------------------ ##

# We'll loop across each stats-ready file separately to get this done
for(focal_ready in local_files){
  
  # Starting message
  message("Beginning averaging steps for '", focal_ready, "'")
  
  # Read in file
  focal_df <- read.csv(file = file.path("data", "stats_ready", focal_ready))
  
  # Prepare to do averaging steps flexibly
  avgs_prep <- focal_df %>%
    # Move site information columns to left and all together
    dplyr::relocate(lter:raw_filename, site:core, 
                    .before = dplyr::everything()) %>%
    # Drop core-specific depth/horizon info
    dplyr::select(-dplyr::starts_with("horizon"),
                  -dplyr::starts_with("depth."), 
                  -core.length_cm, -bulk.density_g.cm3) %>%
    # Reshape to long format so all number columns are together
    tidyr::pivot_longer(cols = -lter:-core,
                        names_to = "variables", values_to = "vals")
  
  # Begin with averaging across cores within plots
  plots_v1 <- avgs_prep %>%
    # Group by everything and average the response variables
    dplyr::group_by(lter, dataset_simp, dataset, site, block, plot, variables) %>%
    dplyr::summarize(mean = mean(vals, na.rm = T),
                     std.dev = sd(vals, na.rm = T),
                     sample.size = dplyr::n(),
                     std.error = std.dev / sqrt(sample.size)) %>%
    dplyr::ungroup()
  
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
  
  # Get file names for both plot-level and site-level averages
  site_name <- gsub(pattern = "_stats-ready_", replacement = "_site-avgs_", x = focal_ready)
  plot_name <- gsub(pattern = "_stats-ready_", replacement = "_plot-avgs_", x = focal_ready)
  
  # Export both locally
  write.csv(x = sites_v2, na = '', row.names = F,
            file = file.path("data", "averages", site_name))
  write.csv(x = plots_v2, na = '', row.names = F,
            file = file.path("data", "averages", plot_name))
  
} # Closing loop

## ------------------------------------------ ##
          # Export Averages to Drive ----
## ------------------------------------------ ##

# Identify all average files
( avg_done <- dir(path = file.path("data", "averages")) )

# Loop across these and upload to Drive
for(focal_avg in avg_done){
  
  googledrive::drive_upload(media = file.path("data", "averages", focal_avg),
                            path = tidy_drive, overwrite = T)
  
}

# End ----

