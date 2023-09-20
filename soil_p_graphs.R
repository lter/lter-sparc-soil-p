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

## ------------------------------------------ ##
            # Graph Housekeeping ----
## ------------------------------------------ ##

# Pick a color palette for LTERs
lter_colors <- c("ARC" = "#264653", "BNZ" = "#2a9d8f", "Brazil" = "#e9c46a", 
                 "Calhoun" = "#f4a261", "CWT" = "#f4a261", "HBR" = "#e76f51", 
                 "JRN"  = "#606c38", "KNZ" = "#ffafcc", "LUQ" = "#d9ed92", 
                 "NWT" = "#06d6a0", "SEV" = "#d62828")

# Custom ggplot theme
sparc_theme <- theme(panel.grid = element_blank(),
                     panel.background = element_blank(),
                     axis.line = element_line(color = "black"),
                     # Legend components
                     legend.title = element_blank(),
                     # Facet labels (where applicable)
                     strip.text = element_text(size = 16),
                     strip.background = element_rect(color = "black", fill = "white",
                                                     linewidth = 0.5),
                     # Axis tick mark / title elements
                     axis.text.y = element_text(size = 14),
                     axis.text.x = element_text(size = 13),
                     axis.title = element_text(size = 16))

## ------------------------------------------ ##
            # Custom Functions ----
## ------------------------------------------ ##

# Making the same graphs repeatedly with different X/Ys
## Makes sense to make them into functions in order to:
### 1. Centralize plotting aesthetics / structure
### 2. Avoid re-typing code for every graph

# Across-dataset averages graph
avg_graph <- function(data = avgs_df, x_var, y_var, 
                      text_nudge_x = -0.1, text_nudge_y = 0.2){
  
  # Error out if X axis isn't in data
  if(!x_var %in% names(data))
    stop("X variable not found in data. Check spelling")
  
  # Do the same for Y axis
  if(!y_var %in% names(data))
    stop("Y variable not found in data. Check spelling")
  
  # Deduce the names of the standard error columns
  x_se <- gsub(pattern = "mean", replacement = "std.error", x = x_var)
  y_se <- gsub(pattern = "mean", replacement = "std.error", x = y_var)
  
  # Generate plot
  q <- ggplot(data = data, aes(x = .data[[x_var]], y = .data[[y_var]])) +
    # Y-axis error bars
    geom_errorbar(aes(ymax = .data[[y_var]] + .data[[y_se]],
                      ymin = .data[[y_var]] - .data[[y_se]])) +
    # X-axis error bars
    geom_errorbarh(aes(xmax = .data[[x_var]] + .data[[x_se]],
                       xmin = .data[[x_var]] - .data[[x_se]])) +
    # Best fit line
    geom_smooth(method = "lm", formula = "y ~ x", se = F, color = "black") +
    # Points/labels for each dataset
    geom_point(aes(fill = lter), pch = 21, size = 3) +
    geom_text(aes(label = dataset_simp), nudge_y = text_nudge_y, nudge_x = text_nudge_x) +
    # Customizing theme elements
    sparc_theme +
    theme(legend.position = "none")
  
  # Return that plot
  return(q) }

# Within-dataset plots
reg_graph <- function(data = main_df, x_var, y_var){
  
  # Error out if X axis isn't in data
  if(!x_var %in% names(data))
    stop("X variable not found in data. Check spelling")
  
  # Do the same for Y axis
  if(!y_var %in% names(data))
    stop("Y variable not found in data. Check spelling")
  
  # Figure out how many point shapes are needed
  shps <- c(22, 24, 23, 21, 25)[1:length(unique(data$dataset_simp))]
  
  # Generate plot
  p <- ggplot(data = data, aes(x = .data[[x_var]], y = .data[[y_var]], shape = dataset_simp)) +
    # Best fit line
    geom_smooth(method = "lm", formula = "y ~ x", se = F, color = "black") +
    # Facet by LTER (just to get nice label)
    facet_grid(. ~ lter) +
    # Points/labels for each dataset
    geom_point(aes(fill = lter), size = 3) +
    # Customizing theme elements
    scale_shape_manual(values = shps) +
    sparc_theme +
    guides(fill = "none")
  
  # Return that plot
  return(p) }

## ------------------------------------------ ##
          # Site Average Graphs ----
## ------------------------------------------ ##

# Read in site averages data file
avgs_df <- read.csv(file.path("tidy_data", "site-avgs_tidy-soil-p.csv"))

# Check structure
dplyr::glimpse(avgs_df)

# Check simplified dataset names
sort(unique(avgs_df$dataset_simp))

# N% ~ total P
(xsite_ntotp <- avg_graph(x_var = "mean_total.P_conc_mg.kg", y_var = "mean_N_conc_percent") +
  labs(x = "Mean Total P (mg/kg) ± SE", y = "Mean N (%) ± SE") +
    scale_fill_manual(values = lter_colors))

# N% ~ slow P
(xsite_nslowp <- avg_graph(x_var = "mean_slow.P_conc_mg.kg", y_var = "mean_N_conc_percent") +
  labs(x = "Mean Slow P (mg/kg) ± SE", y = "Mean N (%) ± SE") +
    scale_fill_manual(values = lter_colors))

# C% ~ total P
(xsite_ctotp <- avg_graph(x_var = "mean_total.P_conc_mg.kg", y_var = "mean_C_conc_percent") +
  labs(x = "Mean Total P (mg/kg) ± SE", y = "Mean C (%) ± SE") +
    scale_fill_manual(values = lter_colors))

# C% ~ total P
(xsite_cslowp <- avg_graph(x_var = "mean_slow.P_conc_mg.kg", y_var = "mean_C_conc_percent") +
  labs(x = "Mean Slow P (mg/kg) ± SE", y = "Mean C (%) ± SE") +
    scale_fill_manual(values = lter_colors))

# Assemble a multi-panel figure!
cowplot::plot_grid(xsite_ntotp, xsite_nslowp, xsite_ctotp, xsite_cslowp,
                   labels = "AUTO", nrow = 2, ncol = 2)

# Export it
ggsave(filename = file.path("graphs", "figure-1_across-datasets.png"),
       width = 10, height = 10, units = "in")

## ------------------------------------------ ##
            # Within-Site Graphs ----
## ------------------------------------------ ##

# Read in more granular (spatially) data
main_df <- read.csv(file.path("tidy_data", "stats-ready_tidy-soil-p.csv"))

# Check structure
dplyr::glimpse(main_df)

# Loop across LTERs
for(LTER_site in unique(main_df$lter)){
  
  # Starting message
  message("Beginning graphs for ", LTER_site)
  
  # N% ~ total P
  (sub_ntotp <- reg_graph(data = dplyr::filter(main_df, lter == LTER_site),
                          x_var = "total.P_conc_mg.kg", y_var = "N_conc_percent") +
      labs(x = "Total P (mg/kg)", y = "N (%)") +
      scale_fill_manual(values = lter_colors))
  
  # N% ~ slow P
  (sub_nslowp <- reg_graph(data = dplyr::filter(main_df, lter == LTER_site),
                           x_var = "slow.P_conc_mg.kg", y_var = "N_conc_percent") +
      labs(x = "Slow P (mg/kg)", y = "N (%)") +
      scale_fill_manual(values = lter_colors))
  
  # C% ~ total P
  (sub_ctotp <- reg_graph(data = dplyr::filter(main_df, lter == LTER_site),
                          x_var = "total.P_conc_mg.kg", y_var = "C_conc_percent") +
      labs(x = "Total P (mg/kg)", y = "C (%)") +
      scale_fill_manual(values = lter_colors))
  
  # C% ~ total P
  (sub_cslowp <- reg_graph(data = dplyr::filter(main_df, lter == LTER_site),
                           x_var = "slow.P_conc_mg.kg", y_var = "C_conc_percent") +
      labs(x = "Slow P (mg/kg)", y = "C (%)") +
      scale_fill_manual(values = lter_colors))
  
  # Assemble into single graph
  cowplot::plot_grid(sub_ntotp, sub_nslowp, sub_ctotp, sub_cslowp,
                     labels = "AUTO", nrow = 2, ncol = 2)
  
  # Export it
  ggsave(filename = file.path("graphs", paste0("figure-2_within-LTER_", LTER_site, ".png")),
         width = 10, height = 10, units = "in")
  }

# End ----
