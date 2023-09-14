## ------------------------------------------ ##
    # SPARC Soil P -- Data Visualization
## ------------------------------------------ ##
# Script author(s): Nick J Lyon

# Purpose:
## Create the desired data visualizations / explorations
## Not necessarily publication quality

## ------------------------------------------ ##
              # Housekeeping -----
## ------------------------------------------ ##

# Load necessary libraries
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive, supportR)

# Create necessary sub-folder(s)
dir.create(path = file.path("tidy_data"), showWarnings = F)
dir.create(path = file.path("graphs"), showWarnings = F)

# Identify and download the tidied megadata object from the Drive
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1pjgN-wRlec65NDLBvryibifyx6k9Iqy9")) %>%
  dplyr::filter(name == "tidy_soil_p.csv") %>%
  googledrive::drive_download(file = .$id, path = file.path("tidy_data", .$name), overwrite = T)

# Clear environment
rm(list = ls())

## ------------------------------------------ ##
            # Pre-Viz Wrangling ----
## ------------------------------------------ ##

# Read in full megadata
mega <- read.csv(file.path("tidy_data", "tidy_soil_p.csv"))

# Check structure
dplyr::glimpse(mega)

# Depth subsetting is used to restrict core depth of samples used in analysis
## Decide how many centimeters from the first measured depth (of the A horizon) are allowed
depth_cutoff <- 15

# Megadata includes *a lot* of information and we only really need a subset of it for stats
main_df <- mega %>%
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
supportR::diff_check(old = names(mega), new = names(main_df), sort = F)

# Check out the structure of the data
dplyr::glimpse(main_df)

# Now create a version of this that is averaged within sites
site_avgs <- main_df %>%
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
# Graph Housekeeping ----
## ------------------------------------------ ##

# Define some useful graph aesthetic components
n_color1 <- "#f4a261"
n_color2 <- "#e76f51"
c_color1 <- "#2a9d8f"
c_color2 <- "#264653"

# Custom ggplot theme
sparc_theme <- theme(panel.grid = element_blank(),
                     panel.background = element_blank(),
                     legend.position = "none",
                     axis.line = element_line(color = "black"))

## ------------------------------------------ ##
          # Site Average Graphs ----
## ------------------------------------------ ##

# Make a N ~ total P graph
ggplot(data = site_avgs, aes(x = total.P_conc_mg.kg, y = N_conc_percent)) +
  geom_smooth(method = "lm", formula = "y ~ x", se = F, color = "black") +
  geom_point(aes(fill = 'x'), pch = 21, size = 3) +
  # Custom labels / colors / theme
  labs(x = "Total P Conc. (mg/kg)", y = "N Conc. (%)") +
  scale_fill_manual(values = n_color1) +
  sparc_theme

# Export it
ggsave(filename = file.path("graphs", "nitrogen_total-P.png"),
       width = 5, height = 5, units = "in")

# Do the same for N ~ slow P
ggplot(data = site_avgs, aes(x = slow.P_conc_mg.kg, y = N_conc_percent)) +
  geom_smooth(method = "lm", formula = "y ~ x", se = F, color = "black") +
  geom_point(aes(fill = 'x'), pch = 21, size = 3) +
  # Custom labels / colors / theme
  labs(x = "Slow P Conc. (mg/kg)", y = "N Conc. (%)") +
  scale_fill_manual(values = n_color2) +
  sparc_theme

# Export it
ggsave(filename = file.path("graphs", "nitrogen_slow-P.png"),
       width = 5, height = 5, units = "in")

# Make a C ~ total P graph
ggplot(data = site_avgs, aes(x = total.P_conc_mg.kg, y = C_conc_percent)) +
  geom_smooth(method = "lm", formula = "y ~ x", se = F, color = "black") +
  geom_point(aes(fill = 'x'), pch = 21, size = 3) +
  # Custom labels / colors / theme
  labs(x = "Total P Conc. (mg/kg)", y = "C Conc. (%)") +
  scale_fill_manual(values = c_color1) +
  sparc_theme

# Export it
ggsave(filename = file.path("graphs", "carbon_total-P.png"),
       width = 5, height = 5, units = "in")

# Do the same for C ~ slow P
ggplot(data = site_avgs, aes(x = slow.P_conc_mg.kg, y = C_conc_percent)) +
  geom_smooth(method = "lm", formula = "y ~ x", se = F, color = "black") +
  geom_point(aes(fill = 'x'), pch = 21, size = 3) +
  # Custom labels / colors / theme
  labs(x = "Slow P Conc. (mg/kg)", y = "N Conc. (%)") +
  scale_fill_manual(values = c_color2) +
  sparc_theme

# Export it
ggsave(filename = file.path("graphs", "carbon_slow-P.png"),
       width = 5, height = 5, units = "in")

## ------------------------------------------ ##
# Site Average Combo Graph ----
## ------------------------------------------ ##





## ------------------------------------------ ##
# Within-Site Graphs ----
## ------------------------------------------ ##





# End ----
