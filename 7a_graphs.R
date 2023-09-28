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
dir.create(path = file.path("data"), showWarnings = F)
dir.create(path = file.path("data", "tidy_data"), showWarnings = F)
dir.create(path = file.path("graphs"), showWarnings = F)
dir.create(path = file.path("graphs", "cores"), showWarnings = F)
dir.create(path = file.path("graphs", "plots"), showWarnings = F)
dir.create(path = file.path("graphs", "sites"), showWarnings = F)

# Identify the needed data file(s) in the Drive
( file_ids <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1pjgN-wRlec65NDLBvryibifyx6k9Iqy9")) %>%
    dplyr::filter(name %in% c("sparc-soil-p_stats-ready_mineral_0-10.csv", 
                              "sparc-soil-p_site-avgs_mineral_0-10.csv", 
                              "sparc-soil-p_plot-avgs_mineral_0-10.csv")) )

# Download those files
purrr::walk2(.x = file_ids$id, .y = file_ids$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T, 
                                                path = file.path("data", "tidy_data", .y)))

# Clear environment
rm(list = ls())

## ------------------------------------------ ##
                # Load Data ----
## ------------------------------------------ ##

# Read in site averages data file
site_df <- read.csv(file.path("data", "tidy_data", "sparc-soil-p_site-avgs_mineral_0-10.csv")) %>%
  # Get a column that is the dataset number
  tidyr::separate_wider_delim(cols = dataset_simp, delim = "_", too_few = "align_start",
                              names = c("dataset_dup", "dataset_num"), cols_remove = F) %>%
  # Drop now-duplicated dataset column
  dplyr::select(-dataset_dup) %>%
  # If no number, fill with 1
  dplyr::mutate(dataset_num = ifelse(is.na(dataset_num), yes = 1, no = dataset_num))

# Check structure
dplyr::glimpse(site_df)

# Make one version each for N and C plotting respectively
site_n <- dplyr::filter(.data = site_df, !is.na(mean_N_conc_percent))
site_c <- dplyr::filter(.data = site_df, !is.na(mean_C_conc_percent))

# Read in more granular (spatially) data
plot_df <- read.csv(file.path("data", "tidy_data", "sparc-soil-p_plot-avgs_mineral_0-10.csv")) %>%
  # Do dataset number processing here too
  tidyr::separate_wider_delim(cols = dataset_simp, delim = "_", too_few = "align_start",
                              names = c("dataset_dup", "dataset_num"), cols_remove = F) %>%
  dplyr::select(-dataset_dup) %>%
  dplyr::mutate(dataset_num = ifelse(is.na(dataset_num), yes = 1, no = dataset_num))

# Check structure
dplyr::glimpse(plot_df)

# Make one version each for N and C plotting respectively
plot_n <- dplyr::filter(.data = plot_df, !is.na(mean_N_conc_percent))
plot_c <- dplyr::filter(.data = plot_df, !is.na(mean_C_conc_percent))

# Read in the unsummarized data
core_df <- read.csv(file.path("data", "tidy_data", "sparc-soil-p_stats-ready_mineral_0-10.csv")) %>%
  # Do dataset number processing here too
  tidyr::separate_wider_delim(cols = dataset_simp, delim = "_", too_few = "align_start",
                              names = c("dataset_dup", "dataset_num"), cols_remove = F) %>%
  dplyr::select(-dataset_dup) %>%
  dplyr::mutate(dataset_num = ifelse(is.na(dataset_num), yes = 1, no = dataset_num))

# Check graphs
dplyr::glimpse(core_df)

# Make one version each for N and C plotting respectively
core_n <- dplyr::filter(.data = core_df, !is.na(N_conc_percent))
core_c <- dplyr::filter(.data = core_df, !is.na(C_conc_percent))

## ------------------------------------------ ##
            # Graph Housekeeping ----
## ------------------------------------------ ##

# Pick a color palette for LTERs
lter_colors <- c("ARC" = "#264653", "BNZ" = "#2a9d8f", "Brazil" = "#e9c46a", 
                 "Calhoun" = "#f4a261", "CWT" = "#7209b7", "FCE" = "#00b4d8",
                 "HBR" = "#e76f51", 
                 "JRN"  = "#606c38", "KNZ" = "#ffafcc", "LUQ" = "#d9ed92", 
                 "NWT" = "#06d6a0", "SEV" = "#d62828")

# Shapes for dataset numbers
data_shapes <- c("1" = 21, "2" = 22, "3" = 24, "4" = 23)

# Define error bars for metrics we're sure to graph
## Carbon SE
c_se <- geom_errorbar(aes(ymax = mean_C_conc_percent + std.error_C_conc_percent,
                             ymin = mean_C_conc_percent - std.error_C_conc_percent),
                      width = 0, na.rm = T)
## Nitrogen SE
n_se <- geom_errorbar(aes(ymax = mean_N_conc_percent + std.error_N_conc_percent,
                             ymin = mean_N_conc_percent - std.error_N_conc_percent),
                      width = 0, na.rm = T)
## Slow P SE
slowp_se <- geom_errorbarh(aes(xmax = mean_slow.P_conc_mg.kg + std.error_slow.P_conc_mg.kg,
                                  xmin = mean_slow.P_conc_mg.kg - std.error_slow.P_conc_mg.kg),
                           height = 0, na.rm = T)
## Total P SE
totp_se <- geom_errorbarh(aes(xmax = mean_total.P_conc_mg.kg + std.error_total.P_conc_mg.kg,
                                 xmin = mean_total.P_conc_mg.kg - std.error_total.P_conc_mg.kg),
                          height = 0, na.rm = T)

## NaOH P SE
NaOH_se <- geom_errorbarh(aes(xmax = mean_NaOH.P_conc_mg.kg + std.error_NaOH.P_conc_mg.kg,
                              xmin = mean_NaOH.P_conc_mg.kg - std.error_NaOH.P_conc_mg.kg),
                          height = 0, na.rm = T)

# Custom ggplot theme
sparc_theme <- theme(panel.grid = element_blank(),
                     panel.background = element_blank(),
                     axis.line = element_line(color = "black"),
                     # Facet labels (where applicable)
                     strip.text = element_text(size = 16),
                     strip.background = element_rect(color = "black", fill = "white",
                                                     linewidth = 1),
                     # Axis tick mark / title elements
                     axis.text.y = element_text(size = 14),
                     axis.text.x = element_text(size = 13),
                     axis.title = element_text(size = 16),
                     # Legend tweaks
                     legend.key = element_blank(),
                     legend.background = element_blank())

## ------------------------------------------ ##
          # Site Average Graphs N and C versus Total and Slow ---- 
## ------------------------------------------ ##

# Check structure of relevant data
dplyr::glimpse(site_df)

# N% ~ total P
xsite_ntotp <- ggplot(data = site_n, aes(x = mean_total.P_conc_mg.kg, y = mean_N_conc_percent)) +
  # Error bars (defined above)
  n_se + totp_se +
  # Best fit line
  geom_smooth(method = "lm", formula = "y ~ x", se = F, color = "black") +
  # Points/labels for each dataset
  geom_point(aes(fill = lter, shape = dataset_num), size = 3, alpha = 0.95) +
  # Customizing theme elements
  labs(x = "Mean Total P (mg/kg) ± SE", y = "Mean N (%) ± SE",
       shape = "Dataset Number", fill = "LTER") +
  scale_shape_manual(values = data_shapes) +
  scale_fill_manual(values = lter_colors) +
  guides(fill = guide_legend(override.aes = list(shape = 21, size = 6)),
         shape = guide_legend(override.aes = list(size = 5))) +
  sparc_theme +
  theme(legend.position = "none"); xsite_ntotp
  
# N% ~ slow P
xsite_nslowp <- ggplot(data = site_n, aes(x = mean_slow.P_conc_mg.kg, y = mean_N_conc_percent)) +
  # Error bars (defined above)
  n_se + slowp_se +
  # Best fit line
  geom_smooth(method = "lm", formula = "y ~ x", se = F, color = "black") +
  # Points/labels for each dataset
  geom_point(aes(fill = lter, shape = dataset_num), size = 3, alpha = 0.95) +
  # Customizing theme elements
  labs(x = "Mean Slow P (mg/kg) ± SE", y = "Mean N (%) ± SE",
       shape = "Dataset Number", fill = "LTER") +
  scale_shape_manual(values = data_shapes) +
  scale_fill_manual(values = lter_colors) +
  guides(fill = guide_legend(override.aes = list(shape = 21, size = 6)),
         shape = guide_legend(override.aes = list(size = 5))) +
  sparc_theme +
  theme(legend.position = "none"); xsite_nslowp

# C% ~ total P
xsite_ctotp <- ggplot(data = site_c, aes(x = mean_total.P_conc_mg.kg, y = mean_C_conc_percent)) +
  # Error bars (defined above)
  c_se + totp_se +
  # Best fit line
  geom_smooth(method = "lm", formula = "y ~ x", se = F, color = "black") +
  # Points/labels for each dataset
  geom_point(aes(fill = lter, shape = dataset_num), size = 3, alpha = 0.95) +
  # Customizing theme elements
  labs(x = "Mean Total P (mg/kg) ± SE", y = "Mean C (%) ± SE",
       shape = "Dataset Number", fill = "LTER") +
  scale_shape_manual(values = data_shapes) +
  scale_fill_manual(values = lter_colors) +
  sparc_theme +
  guides(fill = F) +
  sparc_theme +
  theme(legend.position = c(0.4, 0.75),
        legend.direction = "horizontal"); xsite_ctotp

# C% ~ slow P
xsite_cslowp <- ggplot(data = site_c, aes(x = mean_slow.P_conc_mg.kg, y = mean_C_conc_percent)) +
  # Error bars (defined above)
  c_se + slowp_se +
  # Best fit line
  geom_smooth(method = "lm", formula = "y ~ x", se = F, color = "black") +
  # Points/labels for each dataset
  geom_point(aes(fill = lter, shape = dataset_num), size = 3, alpha = 0.95) +
  # Customizing theme elements
  labs(x = "Mean Slow P (mg/kg) ± SE", y = "Mean C (%) ± SE",
       shape = "Dataset Number", fill = "LTER") +
  scale_shape_manual(values = data_shapes) +
  scale_fill_manual(values = lter_colors) +
  guides(fill = guide_legend(override.aes = list(shape = 21, size = 6)),
         shape = guide_legend(override.aes = list(size = 5))) +
  guides(fill = guide_legend(override.aes = list(shape = 21, size = 6)),
       shape = guide_legend(override.aes = list(size = 5))) +
  guides(shape = F) +
  sparc_theme +
  theme(legend.position = c(0.6, 0.65),
        legend.direction = "horizontal"); xsite_cslowp

# Assemble a multi-panel figure!
cowplot::plot_grid(xsite_ntotp, xsite_nslowp, xsite_ctotp, xsite_cslowp,
                   labels = "AUTO", nrow = 2, ncol = 2)

# Export it
ggsave(filename = file.path("graphs", "figure-1_across-sites.png"),
       width = 10, height = 10, units = "in")


## ------------------------------------------ ##
# Site Average Graphs N and C versus Total and NaOH ---- 
## ------------------------------------------ ##

# Check structure of relevant data
dplyr::glimpse(site_df)

# N% ~ total P
xsite_ntotp <- ggplot(data = site_n, aes(x = mean_total.P_conc_mg.kg, y = mean_N_conc_percent)) +
  # Error bars (defined above)
  n_se + totp_se +
  # Best fit line
  geom_smooth(method = "lm", formula = "y ~ x", se = F, color = "black") +
  # Points/labels for each dataset
  geom_point(aes(fill = lter, shape = dataset_num), size = 3, alpha = 0.95) +
  # Customizing theme elements
  labs(x = "Mean Total P (mg/kg) ± SE", y = "Mean N (%) ± SE",
       shape = "Dataset Number", fill = "LTER") +
  scale_shape_manual(values = data_shapes) +
  scale_fill_manual(values = lter_colors) +
  guides(fill = guide_legend(override.aes = list(shape = 21, size = 6)),
         shape = guide_legend(override.aes = list(size = 5))) +
  sparc_theme +
  theme(legend.position = "none"); xsite_ntotp

# N% ~ NaOH P
xsite_nnoahp <- ggplot(data = site_n, aes(x = mean_NaOH.P_conc_mg.kg, y = mean_N_conc_percent)) +
  # Error bars (defined above)
  n_se + NaOH_se +
  # Best fit line
  geom_smooth(method = "lm", formula = "y ~ x", se = F, color = "black") +
  # Points/labels for each dataset
  geom_point(aes(fill = lter, shape = dataset_num), size = 3, alpha = 0.95) +
  # Customizing theme elements
  labs(x = "Mean NaOH P (mg/kg) ± SE", y = "Mean N (%) ± SE",
       shape = "Dataset Number", fill = "LTER") +
  scale_shape_manual(values = data_shapes) +
  scale_fill_manual(values = lter_colors) +
  guides(fill = guide_legend(override.aes = list(shape = 21, size = 6)),
         shape = guide_legend(override.aes = list(size = 5))) +
  sparc_theme +
  theme(legend.position = "none"); xsite_nnoahp

# C% ~ total P
xsite_ctotp <- ggplot(data = site_c, aes(x = mean_total.P_conc_mg.kg, y = mean_C_conc_percent)) +
  # Error bars (defined above)
  c_se + totp_se +
  # Best fit line
  geom_smooth(method = "lm", formula = "y ~ x", se = F, color = "black") +
  # Points/labels for each dataset
  geom_point(aes(fill = lter, shape = dataset_num), size = 3, alpha = 0.95) +
  # Customizing theme elements
  labs(x = "Mean Total P (mg/kg) ± SE", y = "Mean C (%) ± SE",
       shape = "Dataset Number", fill = "LTER") +
  scale_shape_manual(values = data_shapes) +
  scale_fill_manual(values = lter_colors) +
  sparc_theme +
  guides(fill = F) +
  sparc_theme +
  theme(legend.position = c(0.4, 0.75),
        legend.direction = "horizontal"); xsite_ctotp

# C% ~ NaOH P
xsite_cnoahp <- ggplot(data = site_c, aes(x =  mean_NaOH.P_conc_mg.kg, y = mean_C_conc_percent)) +
  # Error bars (defined above)
  c_se + NaOH_se +
  # Best fit line
  geom_smooth(method = "lm", formula = "y ~ x", se = F, color = "black") +
  # Points/labels for each dataset
  geom_point(aes(fill = lter, shape = dataset_num), size = 3, alpha = 0.95) +
  # Customizing theme elements
  labs(x = "Mean NaOH P (mg/kg) ± SE", y = "Mean C (%) ± SE",
       shape = "Dataset Number", fill = "LTER") +
  scale_shape_manual(values = data_shapes) +
  scale_fill_manual(values = lter_colors) +
  guides(fill = guide_legend(override.aes = list(shape = 21, size = 6)),
         shape = guide_legend(override.aes = list(size = 5))) +
  guides(fill = guide_legend(override.aes = list(shape = 21, size = 6)),
         shape = guide_legend(override.aes = list(size = 5))) +
  guides(shape = F) +
  sparc_theme +
  theme(legend.position = c(0.6, 0.65),
        legend.direction = "horizontal"); xsite_cnoahp

# Assemble a multi-panel figure!
cowplot::plot_grid(xsite_ntotp, xsite_nnoahp, xsite_ctotp, xsite_cnoahp,
                   labels = "AUTO", nrow = 2, ncol = 2)

# Export it
ggsave(filename = file.path("graphs", "figure-1_across-sites.png"),
       width = 10, height = 10, units = "in")

# Checking for where NaOH is missing 
# subset <- which(is.na(site_n$mean_NaOH.P_conc_mg.kg))
# missing <- site_n[subset,]
# 
# missing <- missing %>% 
#   select(lter:site,mean_NaOH.P_conc_mg.kg,std.error_NaOH.P_conc_mg.kg)

## ------------------------------------------ ##
     # Within-Site Graphs - Plot Avgs ----
## ------------------------------------------ ##

# Check structure
dplyr::glimpse(plot_df)

# Check out unique datasets
unique(plot_df$dataset_simp)

# Loop across datasets and create the four desired plots for each
## Note that not all datasets have both slow and total P
## Entirely missing P types are skipped and the resulting graphs account for this
for(focal_dataset in sort(unique(plot_df$dataset_simp))){ # Actual loop
  # for(focal_dataset in "BNZ_1"){ # Test loop
  
  # Starting message
  message("Beginning across-plots figure creation for dataset: ", focal_dataset)
  
  # Subset N & C data to only a particular dataset
  plot_nsub <- dplyr::filter(.data = plot_n, dataset_simp == focal_dataset)
  plot_csub <- dplyr::filter(.data = plot_c, dataset_simp == focal_dataset)
  
  # Identify correct point shape (in order to match figure 1)
  pt_shp <- data_shapes[unique(c(plot_nsub$dataset_num, plot_csub$dataset_num))]
  
  # Make a list for N graphs
  n_graphs <- list()
  
  # If there is any total P data in this dataset:
  if(any(!is.na(plot_nsub$mean_total.P_conc_mg.kg))){
    
    # Graph N% ~ total P and add to the plot list
    n_graphs[[1]] <- ggplot(data = plot_nsub, aes(x = mean_total.P_conc_mg.kg,
                                                  y = mean_N_conc_percent)) +
      # Best-fit line
      geom_smooth(method = "lm", formula = "y ~ x", se = F, color = "black") +
      # Error bars for Y and X (behind points)
      n_se + totp_se +
      # Points
      geom_point(aes(fill = lter), size = 3, pch = pt_shp) +
      # Facet by dataset to get nice label
      facet_grid(. ~ dataset_simp) +
      # Custom color, axis labels, and theme elements
      scale_fill_manual(values = lter_colors) +
      labs(x = "Mean Total P (mg/kg) ± SE", y = "Mean N (%) ± SE") +
      sparc_theme +
      theme(legend.position = "none") }
  
  # If there is any *slow* P in this dataset:
  if(any(!is.na(plot_nsub$mean_slow.P_conc_mg.kg))){
    
    # Graph N% ~ total P and add to the plot list
    n_graphs[[2]] <- ggplot(data = plot_nsub, aes(x = mean_slow.P_conc_mg.kg,
                                                  y = mean_N_conc_percent)) +
      geom_smooth(method = "lm", formula = "y ~ x", se = F, color = "black") +
      n_se + slowp_se +
      geom_point(aes(fill = lter), size = 3, pch = pt_shp) +
      facet_grid(. ~ dataset_simp) +
      scale_fill_manual(values = lter_colors) +
      labs(x = "Mean Slow P (mg/kg) ± SE", y = "Mean N (%) ± SE") +
      sparc_theme +
      theme(legend.position = "none") }
  
  # Define panel labels based on how many we need
  n_labs <- c("A", "B")[length(n_graphs)]
  
  # Assemble this into a two-panel graph
  if(length(n_graphs) != 0){
    n_bipanel <- cowplot::plot_grid(plotlist = n_graphs, ncol = 2, labels = n_labs) } else {
      n_bipanel <- NULL }
  
  # Now make a list for the C graphs
  c_graphs <- list()
  
  # If there is any total P data in this dataset:
  if(any(!is.na(plot_csub$mean_total.P_conc_mg.kg))){
    
    # Graph N% ~ total P and add to the plot list
    c_graphs[[1]] <- ggplot(data = plot_csub, aes(x = mean_total.P_conc_mg.kg,
                                                  y = mean_C_conc_percent)) +
      # Best-fit line
      geom_smooth(method = "lm", formula = "y ~ x", se = F, color = "black") +
      # Error bars for Y and X (behind points)
      c_se + totp_se +
      # Points
      geom_point(aes(fill = lter), size = 3, pch = pt_shp) +
      # Facet by dataset to get nice label
      facet_grid(. ~ dataset_simp) +
      # Custom color, axis labels, and theme elements
      scale_fill_manual(values = lter_colors) +
      labs(x = "Mean Total P (mg/kg) ± SE", y = "Mean C (%) ± SE") +
      sparc_theme +
      theme(legend.position = "none") }
  
  # If there is any *slow* P in this dataset:
  if(any(!is.na(plot_csub$mean_slow.P_conc_mg.kg))){
    
    # Graph N% ~ total P and add to the plot list
    c_graphs[[2]] <- ggplot(data = plot_csub, aes(x = mean_slow.P_conc_mg.kg,
                                                  y = mean_C_conc_percent)) +
      geom_smooth(method = "lm", formula = "y ~ x", se = F, color = "black") +
      c_se + slowp_se +
      geom_point(aes(fill = lter), size = 3, pch = pt_shp) +
      facet_grid(. ~ dataset_simp) +
      scale_fill_manual(values = lter_colors) +
      labs(x = "Mean Slow P (mg/kg) ± SE", y = "Mean C (%) ± SE") +
      sparc_theme +
      theme(legend.position = "none") }
  
  # Define panel labels based on how many we need
  c_labs <- c("C", "D")[length(c_graphs)]
  
  # Assemble this into a two-panel graph
  if(length(c_graphs) != 0){
    c_bipanel <- cowplot::plot_grid(plotlist = c_graphs, ncol = 2, labels = c_labs) } else {
      c_bipanel <- NULL }
  
  # If either N or C graphs can exist, finalize and export the figure!
  if(length(n_graphs) != 0 | length(c_graphs) != 0){
    
    # Combine the two bi-panel graphs into a single quad panel graph
    focal_figure <- cowplot::plot_grid(n_bipanel, c_bipanel, nrow = 2, labels = NULL)
    
    # Make a nice filename for this particular sub-figure
    focal_name <- paste0("figure-2_plots_", gsub(pattern = "_", replacement = "-", 
                                           x = focal_dataset), ".png")
    
    # Export with an informative file name
    ggsave(filename = file.path("graphs", "plots", focal_name), width = 10, height = 10, units = "in") } 
  
} # Close loop

## ------------------------------------------ ##
       # Within-Site Graphs - Cores ----
## ------------------------------------------ ##

# Check structure
dplyr::glimpse(core_df)

# Check out unique datasets
unique(core_df$dataset_simp)

# Loop across datasets and create the four desired plots for each
## Note that not all datasets have both slow and total P
## Entirely missing P types are skipped and the resulting graphs account for this
for(focal_dataset in sort(unique(core_df$dataset_simp))){ # Actual loop
  # for(focal_dataset in "BNZ_1"){ # Test loop
  
  # Starting message
  message("Beginning core-level figure creation for dataset: ", focal_dataset)
  
  # Subset N & C data to only a particular dataset
  core_nsub <- dplyr::filter(.data = core_n, dataset_simp == focal_dataset)
  core_csub <- dplyr::filter(.data = core_c, dataset_simp == focal_dataset)
  
  # Identify correct point shape (in order to match figure 1)
  pt_shp <- data_shapes[unique(c(core_nsub$dataset_num, core_csub$dataset_num))]
  
  # Make a list for N graphs
  n_graphs <- list()
  
  # If there is any total P data in this dataset:
  if(any(!is.na(core_nsub$total.P_conc_mg.kg))){
    
    # Graph N% ~ total P and add to the plot list
    n_graphs[[1]] <- ggplot(data = core_nsub, aes(x = total.P_conc_mg.kg,
                                                  y = N_conc_percent)) +
      # Best-fit line
      geom_smooth(method = "lm", formula = "y ~ x", se = F, color = "black") +
      # Points
      geom_point(aes(fill = site), size = 3, pch = pt_shp, alpha = 0.5) +
      # Facet by dataset to get nice label
      facet_grid(. ~ dataset_simp) +
      # Custom color, axis labels, and theme elements
      labs(x = "Total P (mg/kg)", y = "N (%)") +
      sparc_theme +
      theme(legend.position = "none") }
  
  # If there is any *slow* P in this dataset:
  if(any(!is.na(core_nsub$slow.P_conc_mg.kg))){
    
    # Graph N% ~ total P and add to the plot list
    n_graphs[[2]] <- ggplot(data = core_nsub, aes(x = slow.P_conc_mg.kg,
                                                  y = N_conc_percent)) +
      geom_smooth(method = "lm", formula = "y ~ x", se = F, color = "black") +
      geom_point(aes(fill = site), size = 3, pch = pt_shp, alpha = 0.5) +
      facet_grid(. ~ dataset_simp) +
      labs(x = "Slow P (mg/kg)", y = "N (%)") +
      sparc_theme +
      theme(legend.position = "none") }
  
  # Define panel labels based on how many we need
  n_labs <- c("A", "B")[length(n_graphs)]
  
  # Assemble this into a two-panel graph
  if(length(n_graphs) != 0){
    n_bipanel <- cowplot::plot_grid(plotlist = n_graphs, ncol = 2, labels = n_labs) } else {
      n_bipanel <- NULL }
  
  # Now make a list for the C graphs
  c_graphs <- list()
  
  # If there is any total P data in this dataset:
  if(any(!is.na(core_csub$total.P_conc_mg.kg))){
    
    # Graph N% ~ total P and add to the plot list
    c_graphs[[1]] <- ggplot(data = core_csub, aes(x = total.P_conc_mg.kg,
                                                  y = C_conc_percent)) +
      # Best-fit line
      geom_smooth(method = "lm", formula = "y ~ x", se = F, color = "black") +
      # Points
      geom_point(aes(fill = site), size = 3, pch = pt_shp, alpha = 0.5) +
      # Facet by dataset to get nice label
      facet_grid(. ~ dataset_simp) +
      # Custom color, axis labels, and theme elements
      labs(x = "Total P (mg/kg)", y = "C (%)") +
      sparc_theme +
      theme(legend.position = "none") }
  
  # If there is any *slow* P in this dataset:
  if(any(!is.na(core_csub$slow.P_conc_mg.kg))){
    
    # Graph N% ~ total P and add to the plot list
    c_graphs[[2]] <- ggplot(data = core_csub, aes(x = slow.P_conc_mg.kg,
                                                  y = C_conc_percent)) +
      geom_smooth(method = "lm", formula = "y ~ x", se = F, color = "black") +
      geom_point(aes(fill = site), size = 3, pch = pt_shp, alpha = 0.5) +
      facet_grid(. ~ dataset_simp) +
      labs(x = "Slow P (mg/kg)", y = "C (%)") +
      sparc_theme +
      theme(legend.position = "none") }
  
  # Define panel labels based on how many we need
  c_labs <- c("C", "D")[length(c_graphs)]
  
  # Assemble this into a two-panel graph
  if(length(c_graphs) != 0){
    c_bipanel <- cowplot::plot_grid(plotlist = c_graphs, ncol = 2, labels = c_labs) } else {
      c_bipanel <- NULL }
  
  # If either N or C graphs can exist, finalize and export the figure!
  if(length(n_graphs) != 0 | length(c_graphs) != 0){
    
    # Combine the two bi-panel graphs into a single quad panel graph
    focal_figure <- cowplot::plot_grid(n_bipanel, c_bipanel, nrow = 2, labels = NULL)
    
    # Make a nice filename for this particular sub-figure
    focal_name <- paste0("figure-2_cores_", gsub(pattern = "_", replacement = "-", 
                                           x = focal_dataset), ".png")
    
    # Export with an informative file name
    ggsave(filename = file.path("graphs", "cores", focal_name), width = 10, height = 10, units = "in") } 
  
} # Close loop

## ------------------------------------------ ##
          # Bonus Within-Site Graph ----
## ------------------------------------------ ##

# Glimpse plot data
dplyr::glimpse(plot_df)

# Make the experimental 'bonus' graph
## Panel 1 - N ~ total P
exp1 <- ggplot(data = plot_df, aes(x = mean_total.P_conc_mg.kg, y = mean_N_conc_percent, 
                                   color = lter)) +
  # Best-fit line
  geom_smooth(method = "lm", formula = "y ~ x", se = F) +
  # Y limits
  ylim(0, 1.7) +
  # Custom color, axis labels, and theme elements
  scale_color_manual(values = lter_colors) +
  labs(x = "Mean Total P (mg/kg)", y = "Mean N (%)") +
  sparc_theme +
  theme(legend.position = c(0.5, 0.95), 
        legend.title = element_blank(),
        legend.direction = "horizontal"); exp1

## Panel 2 - N ~ slow P
exp2 <- ggplot(data = plot_df, aes(x = mean_slow.P_conc_mg.kg, y = mean_N_conc_percent, 
                                   color = lter)) +
  geom_smooth(method = "lm", formula = "y ~ x", se = F) +
  ylim(0, 1.7) +
  scale_color_manual(values = lter_colors) +
  labs(x = "Mean Slow P (mg/kg)", y = "Mean N (%)") +
  sparc_theme +
  theme(legend.position = "none"); exp2

## Panel 3 - C ~ total P
exp3 <- ggplot(data = plot_df, aes(x = mean_total.P_conc_mg.kg, y = mean_C_conc_percent, 
                                   color = lter)) +
  # Best-fit line
  geom_smooth(method = "lm", formula = "y ~ x", se = F) +
  # Y limit
  ylim(c(min(plot_df$mean_C_conc_percent, na.rm = T),
         max(plot_df$mean_C_conc_percent, na.rm = T))) +
  # Custom color, axis labels, and theme elements
  scale_color_manual(values = lter_colors) +
  labs(x = "Mean Total P (mg/kg)", y = "Mean C (%)") +
  sparc_theme +
  theme(legend.position = "none"); exp3

## Panel 4 - C ~ slow P
exp4 <- ggplot(data = plot_df, aes(x = mean_slow.P_conc_mg.kg, y = mean_C_conc_percent, 
                                   color = lter)) +
  geom_smooth(method = "lm", formula = "y ~ x", se = F) +
  ylim(c(min(plot_df$mean_C_conc_percent, na.rm = T),
         max(plot_df$mean_C_conc_percent, na.rm = T))) +
  scale_color_manual(values = lter_colors) +
  labs(x = "Mean Slow P (mg/kg)", y = "Mean C (%)") +
  sparc_theme +
  theme(legend.position = "none"); exp4

# Assemble into a multi-panel graph
cowplot::plot_grid(exp1, exp2, exp3, exp4, labels = "AUTO", nrow = 2)

# Export
ggsave(filename = file.path("graphs", "plots", "figure-2_All-Datasets_across-plots.png"), 
       width = 10, height = 10, units = "in")

# End ----
