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
    dplyr::filter(name %in% c("site-avgs_tidy-soil-p.csv", "plot-avgs_tidy-soil-p.csv")) )

# Download those files
purrr::walk2(.x = file_ids$id, .y = file_ids$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T, 
                                                path = file.path("tidy_data", .y)))

# Clear environment
rm(list = ls())

## ------------------------------------------ ##
                # Load Data ----
## ------------------------------------------ ##

# Read in site averages data file
site_df <- read.csv(file.path("tidy_data", "site-avgs_tidy-soil-p.csv")) %>%
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
plot_df <- read.csv(file.path("tidy_data", "plot-avgs_tidy-soil-p.csv")) %>%
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

## ------------------------------------------ ##
            # Graph Housekeeping ----
## ------------------------------------------ ##

# Pick a color palette for LTERs
lter_colors <- c("ARC" = "#264653", "BNZ" = "#2a9d8f", "Brazil" = "#e9c46a", 
                 "Calhoun" = "#f4a261", "CWT" = "#7209b7", "HBR" = "#e76f51", 
                 "JRN"  = "#606c38", "KNZ" = "#ffafcc", "LUQ" = "#d9ed92", 
                 "NWT" = "#06d6a0", "SEV" = "#d62828")

# Shapes for dataset numbers
data_shapes <- c("1" = 21, "2" = 22, "3" = 24, "4" = 23)

# Define error bars for metrics we're sure to graph
## Carbon SE
c_se <- geom_errorbar(aes(ymax = mean_C_conc_percent + std.error_C_conc_percent,
                             ymin = mean_C_conc_percent - std.error_C_conc_percent))
## Nitrogen SE
n_se <- geom_errorbar(aes(ymax = mean_N_conc_percent + std.error_N_conc_percent,
                             ymin = mean_N_conc_percent - std.error_N_conc_percent))
## Slow P SE
slowp_se <- geom_errorbarh(aes(xmax = mean_slow.P_conc_mg.kg + std.error_slow.P_conc_mg.kg,
                                  xmin = mean_slow.P_conc_mg.kg - std.error_slow.P_conc_mg.kg))
## Total P SE
totp_se <- geom_errorbarh(aes(xmax = mean_total.P_conc_mg.kg + std.error_total.P_conc_mg.kg,
                                 xmin = mean_total.P_conc_mg.kg - std.error_total.P_conc_mg.kg))

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
          # Site Average Graphs ----
## ------------------------------------------ ##

# Check structure of relevant data
dplyr::glimpse(site_df)

# Early version of graph
xsite_ntotp <- ggplot(data = site_n, aes(x = mean_total.P_conc_mg.kg, y = mean_N_conc_percent)) +
  # Error bars (defined above)
  n_se + totp_se +
  # Best fit line
  geom_smooth(method = "lm", formula = "y ~ x", se = F, color = "black") +
  # Points/labels for each dataset
  geom_point(aes(fill = lter), pch = 21, size = 3) +
  geom_text(aes(label = dataset_simp), nudge_y = -0.1, nudge_x = 0.2) +
  # Custom colors / axis labels
  labs(x = "Mean Total P (mg/kg) ± SE", y = "Mean N (%) ± SE") +
  scale_fill_manual(values = lter_colors) +
  # Customizing theme elements
  sparc_theme +
  theme(legend.position = "none"); xsite_ntotp

# Export this for comparison purposes
ggsave(filename = file.path("graphs", "figure-1_varA.png"),
       height = 7.5, width = 7.5, units = "in")



# N% ~ total P
xsite_ntotp <- ggplot(data = site_df, aes(x = mean_total.P_conc_mg.kg, y = mean_N_conc_percent)) +
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
  theme(legend.position = "top"); xsite_ntotp
  
# Export an experimental version too
ggsave(filename = file.path("graphs", "figure-1_varB.png"),
       height = 7.5, width = 7.5, units = "in")












# N% ~ slow P
xsite_nslowp <- ggplot(data = site_df, aes(x = mean_slow.P_conc_mg.kg, y = mean_N_conc_percent)) +
  # Error bars (defined above)
  n_se + slowp_se +
  # Best fit line
  geom_smooth(method = "lm", formula = "y ~ x", se = F, color = "black") +
  # Points/labels for each dataset
  geom_point(aes(fill = lter), pch = 21, size = 3) +
  geom_text(aes(label = dataset_simp), nudge_y = -0.1, nudge_x = 0.2) +
  # Custom colors / axis labels
  labs(x = "Mean Slow P (mg/kg) ± SE", y = "Mean N (%) ± SE") +
  scale_fill_manual(values = lter_colors) +
  # Customizing theme elements
  sparc_theme +
  theme(legend.position = "none"); xsite_nslowp
  
# C% ~ total P
xsite_ctotp <- ggplot(data = site_df, aes(x = mean_total.P_conc_mg.kg, y = mean_C_conc_percent)) +
  # Error bars (defined above)
  c_se + totp_se +
  # Best fit line
  geom_smooth(method = "lm", formula = "y ~ x", se = F, color = "black") +
  # Points/labels for each dataset
  geom_point(aes(fill = lter), pch = 21, size = 3) +
  geom_text(aes(label = dataset_simp), nudge_y = -0.1, nudge_x = 0.2) +
  # Custom colors / axis labels
  labs(x = "Mean Total P (mg/kg) ± SE", y = "Mean C (%) ± SE") +
  scale_fill_manual(values = lter_colors) +
  # Customizing theme elements
  sparc_theme +
  theme(legend.position = "none"); xsite_ctotp

# C% ~ slow P
xsite_cslowp <- ggplot(data = site_df, aes(x = mean_slow.P_conc_mg.kg, y = mean_C_conc_percent)) +
  # Error bars (defined above)
  c_se + slowp_se +
  # Best fit line
  geom_smooth(method = "lm", formula = "y ~ x", se = F, color = "black") +
  # Points/labels for each dataset
  geom_point(aes(fill = lter), pch = 21, size = 3) +
  geom_text(aes(label = dataset_simp), nudge_y = -0.1, nudge_x = 0.2) +
  # Custom colors / axis labels
  labs(x = "Mean Slow P (mg/kg) ± SE", y = "Mean C (%) ± SE") +
  scale_fill_manual(values = lter_colors) +
  # Customizing theme elements
  sparc_theme +
  theme(legend.position = "none"); xsite_cslowp

# Assemble a multi-panel figure!
cowplot::plot_grid(xsite_ntotp, xsite_nslowp, xsite_ctotp, xsite_cslowp,
                   labels = "AUTO", nrow = 2, ncol = 2)

# Export it
ggsave(filename = file.path("graphs", "figure-1_across-sites.png"),
       width = 10, height = 10, units = "in")

## ------------------------------------------ ##
            # Within-Site Graphs ----
## ------------------------------------------ ##

# Check structure
dplyr::glimpse(plot_df)

# Check out unique datasets
unique(plot_df$dataset_simp)

# Define a dataset to use
focal_dataset <- "BNZ_1"

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
n_bipanel <- cowplot::plot_grid(plotlist = n_graphs, ncol = 2, labels = n_labs)

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
c_bipanel <- cowplot::plot_grid(plotlist = c_graphs, ncol = 2, labels = c_labs)

# Combine the two bi-panel graphs into a single quad panel graph
focal_figure <- cowplot::plot_grid(n_bipanel, c_bipanel, nrow = 2, labels = NULL)

# Export with an informative file name
ggsave(filename = file.path("graphs", 
                            paste0("figure-2_", gsub("_", "-", focal_dataset), "_across-plots.png")),
       width = 10, height = 10, units = "in")
















# basement ----

## N ~ slow P
ggplot(data = plot_nsub, aes(x = mean_slow.P_conc_mg.kg, y = mean_N_conc_percent)) +
  # Best-fit line
  geom_smooth(method = "lm", formula = "y ~ x", se = F, color = "black") +
  # Error bars for Y and X (behind points)
  n_se + slowp_se +
  # Points
  geom_point(aes(fill = lter), size = 3, pch = pt_shp) +
  # Facet by dataset to get nice label
  facet_grid(. ~ dataset_simp) +
  # Custom color, axis labels, and theme elements
  scale_fill_manual(values = lter_colors) +
  labs(x = "Mean Slow P (mg/kg) ± SE", y = "Mean N (%) ± SE") +
  sparc_theme +
  theme(legend.position = "none")


# xxxxxxx


















# Loop across LTERs
for(LTER_site in unique(plot_df$lter)){
  
  # Starting message
  message("Beginning graphs for ", LTER_site)
  
  # Filter the data to just that site
  plot_sub <- plot_df %>%
    dplyr::filter(lter == LTER_site)
  
  # N% ~ total P
  (sub_ntotp <- ggplot(data = plot_sub, aes(x = mean_total.P_conc_mg.kg, y = mean_N_conc_percent,
                                            shape = dataset_simp)) +
      # Best fit line
      geom_smooth(method = "lm", formula = "y ~ x", se = F, color = "black") +
      # Facet by LTER (just to get nice label)
      facet_grid(. ~ lter) +
      # Points/labels for each dataset
      geom_point(aes(fill = lter), size = 3) +
      # Customize color / axis labels
      labs(x = "Total P (mg/kg)", y = "N (%)") +
      scale_fill_manual(values = lter_colors) +
      # Customizing theme elements
      sparc_theme +
      guides(fill = "none") )
  
  # N% ~ slow P
  (sub_nslowp <- ggplot(data = plot_sub, aes(x = mean_slow.P_conc_mg.kg, y = mean_N_conc_percent,
                                             shape = dataset_simp)) +
      # Best fit line
      geom_smooth(method = "lm", formula = "y ~ x", se = F, color = "black") +
      # Facet by LTER (just to get nice label)
      facet_grid(. ~ lter) +
      # Points/labels for each dataset
      geom_point(aes(fill = lter), size = 3) +
      # Customize color / axis labels
      labs(x = "Slow P (mg/kg)", y = "N (%)") +
      scale_fill_manual(values = lter_colors) +
      # Customizing theme elements
      sparc_theme +
      guides(fill = "none") )
  
  # C% ~ total P
  (sub_ctotp <- ggplot(data = plot_sub, aes(x = mean_total.P_conc_mg.kg, y = mean_C_conc_percent,
                                            shape = dataset_simp)) +
      # Best fit line
      geom_smooth(method = "lm", formula = "y ~ x", se = F, color = "black") +
      # Facet by LTER (just to get nice label)
      facet_grid(. ~ lter) +
      # Points/labels for each dataset
      geom_point(aes(fill = lter), size = 3) +
      # Customize color / axis labels
      labs(x = "Total P (mg/kg)", y = "C (%)") +
      scale_fill_manual(values = lter_colors) +
      # Customizing theme elements
      sparc_theme +
      guides(fill = "none") )
  
  # C% ~ total P
  (sub_cslowp <- ggplot(data = plot_sub, aes(x = mean_slow.P_conc_mg.kg, y = mean_C_conc_percent,
                                             shape = dataset_simp)) +
      # Best fit line
      geom_smooth(method = "lm", formula = "y ~ x", se = F, color = "black") +
      # Facet by LTER (just to get nice label)
      facet_grid(. ~ lter) +
      # Points/labels for each dataset
      geom_point(aes(fill = lter), size = 3) +
      # Customize color / axis labels
      labs(x = "Slow P (mg/kg)", y = "C (%)") +
      scale_fill_manual(values = lter_colors) +
      # Customizing theme elements
      sparc_theme +
      guides(fill = "none") )
  
  # Assemble into single graph
  cowplot::plot_grid(sub_ntotp, sub_nslowp, sub_ctotp, sub_cslowp,
                     labels = "AUTO", nrow = 2, ncol = 2)
  
  # Export it
  ggsave(filename = file.path("graphs", paste0("figure-2_", LTER_site, "_across-plots.png")),
         width = 10, height = 10, units = "in") }

# End ----
