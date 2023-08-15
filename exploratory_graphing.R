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
  # Replace missing treatment data with "no treatment"
  dplyr::mutate(treatment = ifelse(test = nchar(treatment) == 0,
                                   yes = "none",
                                   no = treatment))

# Check columns
dplyr::glimpse(megadata[1:30])

## Exploring across-site ------------------------- ##

# Create necessary sub-folder(s) to export our visualizations in
dir.create(path = file.path("exploratory_graphs"), showWarnings = F)

# Creating the exploratory plot
# Edit the x and y values as needed
ggplot(data = megadata, aes(x = total_P_mg_kg, y = N_conc_percent, color = lter)) +
  geom_point() +
  # Best-fit line by site
  geom_smooth(method = "lm", formula = "y ~ x", alpha = 0.2, show.legend = F) +
  # Average across-site best-fit line 
  geom_smooth(color = "black",fill = "gray82", method = "lm", formula = "y ~ x") +
  theme_bw()

# Export the plot if you want
ggsave(filename = file.path("exploratory_graphs", "overall_totalP_Nconc.png"),
       plot = last_plot(), width = 6, height = 6, units = "in")

## Exploring site + treatment combinations ------- ##

# Loop across LTERs
for (single_lter in unique(megadata$lter)){
  
  # Filter our megadata to only one subdataset
  example_site <- dplyr::filter(megadata, lter == single_lter)
  
  # Create a graph
  ggplot(data = example_site, aes(x = slow_P_mg_kg, y = C_conc_percent, color = lter)) +
    geom_point() +
    # Best-fit line by site
    geom_smooth(method = "lm", formula = "y ~ x", alpha = 0.2, show.legend = F) +
    # Average across-site best-fit line 
    geom_smooth(color = "black",fill = "gray82", method = "lm", formula = "y ~ x") +
    theme_bw()
  
  # Make a name for this graph
  example_name <- paste0(single_lter, "_slowP_Cconc.png")
  
  # Export the graph
  ggsave(filename = file.path("exploratory_graphs", example_name),
         plot = last_plot(), width = 6, height = 6, units = "in")
  
}
