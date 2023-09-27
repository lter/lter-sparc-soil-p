## ------------------------------------------ ##
    # SPARC Soil P -- Calculating Averages
## ------------------------------------------ ##
# Script author(s): Nick J Lyon

# Purpose:


# Pre-Requisites:


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
( ready_local <- dir(path = file.path("data", "stats_ready")) )



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
# Export Spatial Aggregations ----
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
