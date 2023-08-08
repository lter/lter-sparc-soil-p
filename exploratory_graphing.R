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
dir.create(path = file.path("exploratory_graphs"), showWarnings = F)
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
megadata <- read.csv(file.path("tidy_data", "tidy_soil_p.csv"))

# Check columns
dplyr::glimpse(megadata)

# tinker
ggplot(data = megadata, aes(x = total_P_absolute, y = C_stock_mg_m2, color = treatment)) +
  geom_point(show.legend = F) +
  facet_grid(rows = vars(site))
