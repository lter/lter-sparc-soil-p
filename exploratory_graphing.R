## ------------------------------------------ ##
#     SPARC Soil P -- Exploratory Graphs
## ------------------------------------------ ##
# Script author(s): Angel Chen

# Purpose:
## Create a few exploratory graphs from the tidy megadata file

## ------------------------------------------ ##
#              Housekeeping -----
## ------------------------------------------ ##

# Load necessary libraries
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive)

# Create necessary sub-folder(s)
dir.create(path = file.path("tidy_data"), showWarnings = F)

# Identify megadata data file
tidy_id <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1pjgN-wRlec65NDLBvryibifyx6k9Iqy9"), type = "csv") %>%
  dplyr::filter(name %in% c("tidy_soil_p.csv"))

# Download data file into the new 'tidy_data' folder
purrr::walk2(.x = tidy_id$id, .y = tidy_id$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("tidy_data", .y)))

# Clear environment
rm(list = ls())

## ------------------------------------------ ##
#             Visualization ----
## ------------------------------------------ ##

# Read in megadata file
megadata <- read.csv(file.path("tidy_data", "tidy_soil_p.csv")) %>%
  # Filter out the rows with no treatment info
  dplyr::filter(nchar(treatment) != 0)

# Check columns
dplyr::glimpse(megadata)

## Exploring across-site ------------------------- ##

# Create necessary sub-folder(s) to export our visualizations in
# Customize as needed, for example I made a sub-folder to store all the total P vs. N conc graphs
dir.create(path = file.path("exploratory_graphs"), showWarnings = F)
dir.create(path = file.path("exploratory_graphs", "totalP_Nconc_across_site"), showWarnings = F)

# Point to our export folder(s)
totalP_Nconc_folder <- file.path("exploratory_graphs", "totalP_Nconc_across_site")

# Creating the exploratory plot
# Edit the x and y values as needed
ggplot(data = megadata, aes(x = total_P_mg_kg, y = N_conc_percent, color = site)) +
  geom_point() +
  # Best-fit line by site
  geom_smooth(aes(color = site, fill = site), method = "lm", formula = "y ~ x", alpha = 0.2, show.legend = F) +
  # Average across-site best-fit line 
  geom_smooth(color = "black",fill = "gray82", method = "lm", formula = "y ~ x")

# Export the plot if you want
ggsave(file.path(totalP_Nconc_folder, "overall_totalP_Nconc.png"))

## Exploring site + treatment combinations ------- ##

# Create necessary sub-folder(s) to export our visualizations in
# Customize as needed, for example I made a sub-folder to store all the slow P vs. N conc graphs
dir.create(path = file.path("exploratory_graphs", "slowP_Nconc_within_site"), showWarnings = F)

# Point to our export folder(s)
slowP_Nconc_folder <- file.path("exploratory_graphs", "slowP_Nconc_within_site")

for (a_subdataset in unique(megadata$dataset)){
  # Filter our megadata to only one subdataset
  example_site <- megadata %>%
    dplyr::filter(dataset == a_subdataset)
  
  # Saving the exploratory plot as a png
  # Edit the file path as needed
  png(filename = file.path(slowP_Nconc_folder, paste0(a_subdataset, ".png")), width = 850, height = 850, units = "px")
  
  # Creating the exploratory plot
  # Edit the x and y values as needed
  example_plot <- ggplot(data = example_site, aes(x = slow_P_mg_kg, y = N_conc_percent, color = treatment)) +
    geom_point(show.legend = T) +
    # Facet by site
    facet_grid(site ~ ., scales = "free") +
    # Best-fit line by treatment
    geom_smooth(aes(color = treatment, fill = treatment), method = "lm", formula = "y ~ x", alpha = 0.2, show.legend = F) 
  
  # Plotting it
  plot(example_plot)
  
  dev.off()
}
