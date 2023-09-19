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

# Read in site averages data file
avgs_df <- read.csv(file.path("tidy_data", "site-avgs_tidy-soil-p.csv")) %>%
  # Simplify dataset names to make plot labels neater
  dplyr::mutate(dataset = gsub(pattern = "Bonanza Creek", replacement = "BNZ", x = dataset)) %>%
  dplyr::mutate(dataset = gsub(pattern = "Coweeta", replacement = "CWT", x = dataset)) %>%
  dplyr::mutate(dataset = gsub(pattern = "Hubbard Brook", replacement = "HBR", x = dataset)) %>%
  dplyr::mutate(dataset = gsub(pattern = "Jornada", replacement = "JRN", x = dataset)) %>%
  dplyr::mutate(dataset = gsub(pattern = "Konza", replacement = "KNZ", x = dataset)) %>%
  dplyr::mutate(dataset = gsub(pattern = "Luquillo", replacement = "LUQ", x = dataset)) %>%
  dplyr::mutate(dataset = gsub(pattern = "Niwot", replacement = "NWT", x = dataset)) %>%
  dplyr::mutate(dataset = gsub(pattern = "Sevilleta", replacement = "SEV", x = dataset)) %>%
  dplyr::mutate(dataset = gsub(pattern = "Toolik", replacement = "ARC", x = dataset))

# Check structure
dplyr::glimpse(avgs_df)

# Check simplified dataset names
sort(unique(avgs_df$dataset))

# Read in more granular (spatially) data
main_df <- read.csv(file.path("tidy_data", "stats-ready_tidy-soil-p.csv"))

# Check structure
dplyr::glimpse(main_df)

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
            # Custom Functions ----
## ------------------------------------------ ##

# Making the same graphs repeatedly with different X/Ys
## Makes sense to make them into functions in order to:
### 1. Centralize plotting aesthetics / structure
### 2. Avoid re-typing code for every graph

avg_graph <- function(data = avgs_df, x_var, y_var, 
                      text_nudge_x = -0.1, text_nudge_y = 0.05){
  
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
    geom_text(aes(label = dataset), nudge_y = text_nudge_y, nudge_x = text_nudge_x) +
    # scale_fill_manual(values = n_color1) +
    sparc_theme +
    theme(legend.position = "none")
  
  # Return that plot
  return(q) }




## ------------------------------------------ ##
          # Site Average Graphs ----
## ------------------------------------------ ##

# N% ~ total P
(xsite_ntotp <- avg_graph(x_var = "mean_total.P_conc_mg.kg", y_var = "mean_N_conc_percent") +
  labs(x = "Mean Total P (mg/kg) ± SE", y = "Mean N (%) ± SE"))

# N% ~ slow P
(xsite_nslowp <- avg_graph(x_var = "mean_slow.P_conc_mg.kg", y_var = "mean_N_conc_percent") +
  labs(x = "Mean Slow P (mg/kg) ± SE", y = "Mean N (%) ± SE"))

# C% ~ total P
(xsite_ctotp <- avg_graph(x_var = "mean_total.P_conc_mg.kg", y_var = "mean_C_conc_percent") +
  labs(x = "Mean Total P (mg/kg) ± SE", y = "Mean C (%) ± SE"))

# C% ~ total P
(xsite_cslowp <- avg_graph(x_var = "mean_slow.P_conc_mg.kg", y_var = "mean_C_conc_percent") +
  labs(x = "Mean Slow P (mg/kg) ± SE", y = "Mean C (%) ± SE"))

# Assemble a multi-panel figure!
cowplot::plot_grid(xsite_ntotp, xsite_nslowp, xsite_ctotp, xsite_cslowp,
                   labels = "AUTO", nrow = 2, ncol = 2)

# Export it
ggsave(filename = file.path("graphs", "figure-1_across-datasets.png"),
       width = 10, height = 10, units = "in")

## ------------------------------------------ ##
# Within-Site Graphs ----
## ------------------------------------------ ##

# [ UNDER CONSTRUCTION ]



# End ----
