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
librarian::shelf(tidyverse, googledrive, supportR, cowplot)

# Create necessary sub-folder(s)
dir.create(path = file.path("tidy_data"), showWarnings = F)
dir.create(path = file.path("graphs"), showWarnings = F)

# Identify the needed data file(s) in the Drive
( file_ids <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1pjgN-wRlec65NDLBvryibifyx6k9Iqy9")) %>%
    dplyr::filter(name %in% c("stats-ready_tidy-soil-p.csv", "site-avgs_tidy-soil-p.csv")) )

# Download those files
purrr::walk2(.x = file_ids$id, .y = file_ids$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T, 
                                                path = file.path("tidy_data", .y)))

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
                     axis.line = element_line(color = "black"),
                     axis.text.y = element_text(size = 14),
                     axis.text.x = element_text(size = 13),
                     axis.title = element_text(size = 16))

## ------------------------------------------ ##
          # Site Average Graphs ----
## ------------------------------------------ ##

# Make a N ~ total P graph
(xsite_ntotp <- ggplot(data = avgs_df, aes(x = total.P_conc_mg.kg, y = N_conc_percent)) +
  geom_smooth(method = "lm", formula = "y ~ x", se = F, color = "black") +
  geom_point(aes(fill = lter), pch = 21, size = 3) +
  # Custom labels / colors / theme
  labs(x = "Total P Conc. (mg/kg)", y = "N Conc. (%)") +
  # scale_fill_manual(values = n_color1) +
  sparc_theme)

# Export it
ggsave(filename = file.path("graphs", "xsite_nitrogen_total-P.png"),
       width = 5, height = 5, units = "in")

# Do the same for N ~ slow P
(xsite_nslowp <- ggplot(data = avgs_df, aes(x = slow.P_conc_mg.kg, y = N_conc_percent)) +
  geom_smooth(method = "lm", formula = "y ~ x", se = F, color = "black") +
  geom_point(aes(fill = lter), pch = 21, size = 3) +
  # Custom labels / colors / theme
  labs(x = "Slow P Conc. (mg/kg)", y = "N Conc. (%)") +
  # scale_fill_manual(values = n_color2) +
  sparc_theme)

# Export it
ggsave(filename = file.path("graphs", "xsite_nitrogen_slow-P.png"),
       width = 5, height = 5, units = "in")

# Make a C ~ total P graph
(xsite_ctotp <- ggplot(data = avgs_df, aes(x = total.P_conc_mg.kg, y = C_conc_percent)) +
  geom_smooth(method = "lm", formula = "y ~ x", se = F, color = "black") +
  geom_point(aes(fill = lter), pch = 23, size = 3) +
  # Custom labels / colors / theme
  labs(x = "Total P Conc. (mg/kg)", y = "C Conc. (%)") +
  # scale_fill_manual(values = c_color1) +
  sparc_theme)

# Export it
ggsave(filename = file.path("graphs", "xsite_carbon_total-P.png"),
       width = 5, height = 5, units = "in")

# Do the same for C ~ slow P
(xsite_cslowp <- ggplot(data = avgs_df, aes(x = slow.P_conc_mg.kg, y = C_conc_percent)) +
  geom_smooth(method = "lm", formula = "y ~ x", se = F, color = "black") +
  geom_point(aes(fill = lter), pch = 23, size = 3) +
  # Custom labels / colors / theme
  labs(x = "Slow P Conc. (mg/kg)", y = "C Conc. (%)") +
  # scale_fill_manual(values = c_color2) +
  sparc_theme)

# Export it
ggsave(filename = file.path("graphs", "xsite_carbon_slow-P.png"),
       width = 5, height = 5, units = "in")

## ------------------------------------------ ##
        # Site Average Combo Graph ----
## ------------------------------------------ ##

# Assemble a multi-panel figure!
cowplot::plot_grid(xsite_ntotp, xsite_nslowp, xsite_ctotp, xsite_cslowp,
                   labels = "AUTO", nrow = 2, ncol = 2)

# Export it
ggsave(filename = file.path("graphs", "multipanel_xsite_carbon_slow-P.png"),
       width = 10, height = 10, units = "in")

## ------------------------------------------ ##
# Within-Site Graphs ----
## ------------------------------------------ ##

# [ UNDER CONSTRUCTION ]



# End ----
