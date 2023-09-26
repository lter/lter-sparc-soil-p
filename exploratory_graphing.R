## ------------------------------------------ ##
#     SPARC Soil P -- Exploratory Graphs
## ------------------------------------------ ##
# Script author(s): Angel Chen, Nick Lyon

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
dir.create(path = file.path("exploratory_graphs"), showWarnings = F)

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
#                 Data Prep ----
## ------------------------------------------ ##

# Read in megadata file
megadata <- read.csv(file.path("tidy_data", "tidy_soil_p.csv")) %>%
  # Replace missing treatment data with "no treatment"
  dplyr::mutate(treatment = ifelse(test = nchar(treatment) == 0,
                                   yes = "none",
                                   no = treatment))

# Check columns
dplyr::glimpse(megadata[1:30])

## ------------------------------------------ ##
#         Cross-LTER Visualization ----
## ------------------------------------------ ##

# Choose an X variable and Y variable for across-site plotting
xsite_xvar <- "total_P_mg_kg"
xsite_yvar <- "N_conc_percent"

# Check that they are both in the data
## X
if(xsite_xvar %in% names(megadata)){
  message("X variable looks good!")
} else { message("X variable not found in 'megadata'. Check spelling") }
## Y
if(xsite_yvar %in% names(megadata)){
  message("Y variable looks good!")
} else { message("Y variable not found in 'megadata'. Check spelling") }

# Creating the exploratory plot
# Edit the x and y values as needed
ggplot(data = megadata, aes(x = .data[[xsite_xvar]], y = .data[[xsite_yvar]], color = lter)) +
  geom_point() +
  # Best-fit line by site
  geom_smooth(method = "lm", formula = "y ~ x", alpha = 0.2, show.legend = F) +
  # Average across-site best-fit line 
  geom_smooth(color = "black",fill = "gray82", method = "lm", formula = "y ~ x") +
  theme_bw()

# Assemble a descriptive name for this file
xsite_filename <- paste0("overall_", xsite_xvar, "_vs_", xsite_yvar, ".png")

# Export the plot if you want
ggsave(filename = file.path("exploratory_graphs", xsite_filename),
       plot = last_plot(), width = 6, height = 6, units = "in")

## Exploring site + treatment combinations ------- ##

# Choose within-LTER x and y axes
within_site_xvar <- "slow_P_mg_kg"
within_site_yvar <- "C_conc_percent"

# Check they are in the data too
## X
if(within_site_xvar %in% names(megadata)){
  message("X variable looks good!")
} else { message("X variable not found in 'megadata'. Check spelling") }
## Y
if(within_site_yvar %in% names(megadata)){
  message("Y variable looks good!")
} else { message("Y variable not found in 'megadata'. Check spelling") }

# Loop across LTERs
for (single_lter in unique(megadata$lter)){
  
  # Filter our megadata to only one subdataset
  example_site <- dplyr::filter(megadata, lter == single_lter)
  
  # Only make a plot if there are some values in this subset
  ## Translation: 'if not all of X and not all of Y are missing values, then make plot'
  if(!all(is.na(example_site[[within_site_xvar]])) & 
     !all(is.na(example_site[[within_site_yvar]]))){
    
  # Create a graph
  ggplot(data = example_site, aes(x = .data[[within_site_xvar]], 
                                  y = .data[[within_site_yvar]], 
                                  color = lter)) +
    geom_point() +
    # Best-fit line by site
    geom_smooth(method = "lm", formula = "y ~ x", alpha = 0.2, show.legend = F) +
    # Average across-site best-fit line 
    geom_smooth(color = "black",fill = "gray82", method = "lm", formula = "y ~ x") +
    theme_bw()
  
  # Make a name for this graph
  within_site_filename <- paste0(single_lter, "_", within_site_xvar, "_vs_", 
                                 within_site_yvar, ".png")
    
  # Export the graph
  ggsave(filename = file.path("exploratory_graphs", within_site_filename),
         plot = last_plot(), width = 6, height = 6, units = "in")
  
  } # Close conditional that checked for non-NA values
  
} # Close across LTER loop

# End ----

