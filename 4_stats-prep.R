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
  dplyr::select(lter, dataset_simp, dataset, site, block, plot, core,
                dplyr::starts_with("horizon"), dplyr::starts_with("depth."),
                core.length_cm, bulk.density_g.cm3,
                dplyr::starts_with("Al_"), dplyr::starts_with("Fe"),
                dplyr::ends_with(".P_conc_mg.kg"),
                C_conc_percent, N_conc_percent) %>%
  # Drop non-unique rows
  dplyr::distinct()

# How do the dataframe dimensions change?
dim(sparc_tidy); dim(stats_v1)
## Lose many columns but no rows? Good!

# Check structure
dplyr::glimpse(stats_v1)

## ------------------------------------------ ##
# Statistics / Visualization Subsetting ----
## ------------------------------------------ ##

# Need to subset to only certain horizons and where N/C data are present
stats_v2 <- stats_v1 %>%
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
  dplyr::filter(!is.na(C_conc_percent) | !is.na(N_conc_percent))

# How do the dataframe dimensions change?
dim(stats_v1); dim(stats_v2)
## Lose many rows but no columns? Good!

# Check structure
dplyr::glimpse(stats_v2)

# Need to do a weighted average across 0-2 and 2-10 depth cores
luq2_v1 <- stats_v2 %>%
  # Subset as needed
  dplyr::filter(dataset_simp == "LUQ_2")

# Calculate across 0-2 and 2-10 depths into a single value
luq2_v2 <- luq2_v1 %>%
  # Flip to long format
  tidyr::pivot_longer(cols = -lter:-bulk.density_g.cm3) %>%
  # Do needed weighted calculation
  dplyr::mutate(value = case_when(
    depth.start_cm == 0 & depth.end_cm == 2 ~ value * 0.2,
    depth.start_cm == 8 & depth.end_cm == 10 ~ value * 0.8,
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

# Split off on LUQ_2
not_luq2 <- stats_v2 %>%
  dplyr::filter(dataset_simp != "LUQ_2")

# Recombine
stats_v3 <- dplyr::bind_rows(luq2_v2, not_luq2)

# Finally, we want to subset to only particular depths within those horizons
stats_v4 <- stats_v3 %>%
  # Keep only cores beginning at the top of the horizon
  dplyr::filter(depth.start_cm == 0 | 
                  # Again, keep missing depths on assumption they start at 0
                  nchar(depth.start_cm) == 0 | is.na(depth.start_cm))

# How do the dataframe dimensions change?
dim(stats_v3); dim(stats_v4)
## Lose some rows but no columns? Good!

# Check structure
dplyr::glimpse(stats_v4)
## tibble::view(stats_v4)

## ------------------------------------------ ##
# Export Statistics Data ----
## ------------------------------------------ ##

# Create a final data object
sparc_stats <- stats_v4

# Check its structure
dplyr::glimpse(sparc_stats)

# Define the tidy file name
stats_name <- "stats-ready_tidy-soil-p.csv"

# Save out the final data object
write.csv(x = sparc_stats, file = file.path("tidy_data", stats_name), 
          row.names = F, na = "")

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



# End ----
