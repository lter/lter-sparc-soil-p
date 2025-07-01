

## ------------------------------------------ ##
# Across-site and Within-site N and P relationships  ----
## ------------------------------------------ ##


## ------------------------------------------ ##
# Housekeeping / Loading libraries and data -----
## ------------------------------------------ ##

# Load necessary libraries
library(Rmisc)
library(ggplot2)
library(dplyr)
library(MuMIn)
library(sjPlot)
library(tidyverse)

# loading stats ready data from google drive 
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive, supportR)

# Create necessary sub-folder(s)
dir.create(path = file.path("data", "tidy_data"), showWarnings = F)
dir.create(path = file.path("data", "stats_ready"), showWarnings = F)

# Clear environment
rm(list = ls())

# Identify needed tidy file(s)
tidy_drive <- googledrive::as_id("https://drive.google.com/drive/u/0/folders/1pjgN-wRlec65NDLBvryibifyx6k9Iqy9")

# Identify the archival data in that folder and download it
googledrive::drive_ls(path = tidy_drive) %>%
  dplyr::filter(name == "sparc-soil-p_full-plus-ancil-and-spatial.csv") %>%
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "tidy_data", .$name))

# Read that file in
cores <- read.csv(file = file.path("data", "stats_ready", 
                                    "sparc-soil-p_stats-ready_mineral_0-10.csv"))
# Subset data to konza 1 
# not sure we want/need to do this anymore, hashtagging it out for now - EV 10/08
# JK this code makes it so that the three sites withing the Konza 1 dataset show up as their own datasets
cores$dataset <- ifelse(cores$dataset == "Konza_1", cores$site, cores$dataset )

cores <- cores %>% 
  mutate(PropSlowTotal = slow.P_conc_mg.kg/total.P_conc_mg.kg)

# Double checking all sites exist for slow P dataset here that shouls before subset code below

# Adding code to subset to only observations where both slow P and total N values exist for Niwot 1 
# Hashtag out to make total P figure (come back and make separate dataset later)
cores$slow.P_conc_mg.kg <- ifelse(cores$dataset == "Niwot_1" & is.na(cores$N_conc_percent) == TRUE, NA, cores$slow.P_conc_mg.kg)
cores$N_conc_percent <- ifelse(cores$dataset == "Niwot_1" & is.na(cores$slow.P_conc_mg.kg) == TRUE, NA, cores$N_conc_percent)
  
## ------------------------------------------ ##
# Making summary datasets -----
## ------------------------------------------ ##

# create a data frame that contains the number of rows with which we have sites
Final_table<-data.frame(matrix(nrow=length(unique(cores$dataset)),ncol=1))
names(Final_table) <- c('dataset')
Final_table$dataset<-unique(cores$dataset)

# PLOT LEVEL DATASETS

## TOTAL P 
# plot_totalP <-aggregate(cbind(N_conc_percent,total.P_conc_mg.kg,PropSlowTotal)~dataset+site+block+plot,mean, data=cores, na.rm=T)

plot_totalP  <- cores %>% 
  dplyr::group_by(dataset,site,block,plot) %>% 
  dplyr::summarise(N_conc_percent = mean(N_conc_percent),
            total.P_conc_mg.kg = mean(total.P_conc_mg.kg),
            mean.annual.precip_mm = mean(mean.annual.precip_mm),
            PropSlowTotal = mean(PropSlowTotal) )

## SLOW P
# plot_slowP<-aggregate(cbind(N_conc_percent,slow.P_conc_mg.kg,PropSlowTotal)~dataset+site+block+plot, mean, data=cores, na.rm = F)

plot_slowP <- cores %>% 
  dplyr::group_by(dataset,site,block,plot) %>% 
  dplyr::summarise(N_conc_percent = mean(N_conc_percent),
            slow.P_conc_mg.kg = mean(slow.P_conc_mg.kg),
            PropSlowTotal = mean(PropSlowTotal) )

# SITE LEVEL DATASETS

## TOTAL P 
# site_totalP<-aggregate(cbind(N_conc_percent,total.P_conc_mg.kg,PropSlowTotal)~dataset+site,mean, data=plot_totalP,na.rm=T) # changing data to plot slow P - this means the site means is the mean of the plot means 

site_totalP <- plot_totalP %>% 
  dplyr::group_by(dataset,site) %>% 
  dplyr::summarise(N_conc_percent = mean(N_conc_percent),
            total.P_conc_mg.kg = mean(total.P_conc_mg.kg),
            mean.annual.precip_mm = mean(mean.annual.precip_mm),
            PropSlowTotal = mean(PropSlowTotal) )

## SLOW P
# site_slowP<-aggregate(cbind(N_conc_percent,slow.P_conc_mg.kg,PropSlowTotal)~dataset+site,mean, data=plot_slowP,na.rm=T)

site_slowP <- plot_slowP %>% 
  dplyr::group_by(dataset,site) %>% 
  dplyr::summarise(N_conc_percent = mean(N_conc_percent),
            slow.P_conc_mg.kg = mean(slow.P_conc_mg.kg),
            PropSlowTotal = mean(PropSlowTotal) )

# checking how many sites we have per dataset 
table(site_slowP$dataset)

## SLOW P SUMMARIZED AND SITE SELECTED DATASETS 

# Grouping site Slow P dataset by DATASET and adding columns for mean, standard deviation and standard error for N and P 
dataset_means_slowP <- site_slowP %>% 
  dplyr::select(dataset,N_conc_percent,slow.P_conc_mg.kg,PropSlowTotal) %>% 
  dplyr::group_by(dataset) %>% 
  dplyr::summarise(mean_N = mean(N_conc_percent, na.rm = TRUE),
                   mean_P = mean(slow.P_conc_mg.kg, na.rm = TRUE),
                   mean_ratio = mean(PropSlowTotal, na.rm = TRUE),
                   sd_N = sd(N_conc_percent, na.rm = TRUE),
                   sd_P = sd(slow.P_conc_mg.kg, na.rm = TRUE),
                   sd_ratio = sd(PropSlowTotal, na.rm = TRUE),
                   se_N = plotrix::std.error(N_conc_percent, na.rm = TRUE),
                   se_P = plotrix::std.error(slow.P_conc_mg.kg, na.rm = TRUE) )

## SELECTING ONLY THE SITES WHERE WE HAVE SLOW P (AND REMOVING FCE AND TOOLIK 1)
dataset_means_slowP <- dataset_means_slowP %>% 
  filter(dataset %in% c("Calhoun","Niwot_1","Sevilleta_1","Coweeta","Hubbard Brook","Luquillo_2","Niwot_2","Konza_1","HJ_Andrews_1","CedarCreek","Jornada_2","Tapajos","Niwot_5","Konza_2","Smokey Valley","Hays","Arikaree") )

# MANUALLY CHANGING SEV 1 TOTAL N MEAN FOR NOW, NEED TO DISCUSS WITH ANNE FINAL SOLUTION
# Sev total N mean of grasslands and shrub sites from Anne's thesis = 0.055
# Getting rid of this 7/1/25 because fixed unit conversion in raw dataset 
# dataset_means_slowP <- dataset_means_slowP %>% 
#   mutate(mean_N = ifelse(dataset == "Sevilleta_1",0.055,mean_N))

# Grouping site Slow P dataset by SITE and adding columns for mean, standard deviation and standard error for N and P 
site_means_slowP <- site_slowP %>% 
  select(dataset,site,N_conc_percent,slow.P_conc_mg.kg) %>% 
  group_by(dataset,site) %>% 
  dplyr::summarise(mean_N = mean(N_conc_percent, na.rm = TRUE),
                   mean_P = mean(slow.P_conc_mg.kg, na.rm = TRUE),
                   sd_N = sd(N_conc_percent, na.rm = TRUE),
                   sd_P = sd(slow.P_conc_mg.kg, na.rm = TRUE),
                   se_N = plotrix::std.error(N_conc_percent, na.rm = TRUE),
                   se_P = plotrix::std.error(slow.P_conc_mg.kg, na.rm = TRUE))

site_means_slowP <- site_means_slowP %>% 
  filter(dataset %in% c("Calhoun","Niwot_1","Sevilleta_1","Coweeta","Hubbard Brook","Luquillo_2","Niwot_2","Konza_1","HJ_Andrews_1","CedarCreek","Jornada_2","Luquillo_3","Tapajos","Niwot_5","Konza_2","Smokey Valley","Hays","Arikaree") )

# Getting rid of this 7/1/25 because fixed unit conversion in raw dataset 
# site_means_slowP <- site_means_slowP %>% 
#   mutate(mean_N = ifelse(dataset == "Sevilleta_1",0.055,mean_N))

## TOTAL P SUMMARIZED AND SITE SELECTED DATASETS 
dataset_means_totalP <- site_totalP %>% 
  dplyr::select(dataset, N_conc_percent, total.P_conc_mg.kg, PropSlowTotal, mean.annual.precip_mm) %>% 
  group_by(dataset) %>% 
  dplyr::summarise(mean_N = mean(N_conc_percent, na.rm = TRUE),
                   mean_P = mean(total.P_conc_mg.kg, na.rm = TRUE),
                   mean_precip = mean(mean.annual.precip_mm, na.rm = TRUE),
                   mean_ratio = mean(PropSlowTotal, na.rm = TRUE),
                   sd_N = sd(N_conc_percent, na.rm = TRUE),
                   sd_P = sd(total.P_conc_mg.kg, na.rm = TRUE),
                   sd_ratio = sd(PropSlowTotal, na.rm = TRUE),
                   se_N = plotrix::std.error(N_conc_percent, na.rm = TRUE),
                   se_P = plotrix::std.error(total.P_conc_mg.kg, na.rm = TRUE))


# selecting all sites with full total P info 
dataset_means_totalP <- dataset_means_totalP %>% 
  filter(dataset %in% c("Calhoun","Niwot_1","Sevilleta_1","Coweeta","Bonanza Creek_1","Hubbard Brook","Luquillo_1","Luquillo_2","Brazil_SouthernAmazon","Brazil_AtlanticForest","Jornada_1","Bonanza Creek_2","Sevilleta_2","Niwot_2","Niwot_3","Niwot_4","Toolik_1","Kellogg_Bio_Station","Konza_1","HJAndrews_1","CedarCreek","ChichaquaBottoms","Jornada_2","Toolik_2","Bonanza Creek_3","Luquillo_3","Tapajos","Niwot_5","Konza_2","Smokey Valley","Hays","Arikaree") )

# MANUALLY CHANGING SEV 1 TOTAL N MEAN FOR NOW, NEED TO DISCUSS WITH ANNE FINAL SOLUTION
# Sev total N mean of grasslands and shrub sites from Anne's thesis = 0.055
# Getting rid of this 7/1/25 because fixed unit conversion in raw dataset 
# dataset_means_totalP <- dataset_means_totalP %>% 
#   mutate(mean_N = ifelse(dataset == "Sevilleta_1",0.055,mean_N))

site_means_totalP <- site_totalP %>% 
  select(dataset,site,N_conc_percent,total.P_conc_mg.kg) %>% 
  group_by(dataset,site) %>% 
  dplyr::summarise(mean_N = mean(N_conc_percent, na.rm = TRUE),mean_P = mean(total.P_conc_mg.kg, na.rm = TRUE),sd_N = sd(N_conc_percent, na.rm = TRUE),sd_P = sd(total.P_conc_mg.kg, na.rm = TRUE),se_N = plotrix::std.error(N_conc_percent, na.rm = TRUE),se_P = plotrix::std.error(total.P_conc_mg.kg, na.rm = TRUE))

# selecting all sites with full total P info 
site_means_totalP <- site_means_totalP %>% 
  filter(dataset %in% c("Calhoun","Niwot_1","Sevilleta_1","Coweeta","Bonanza Creek_1","Hubbard Brook","Luquillo_1","Luquillo_2","Brazil_SouthernAmazon","Brazil_AtlanticForest","Jornada_1","Bonanza Creek_2","Sevilleta_2","Niwot_2","Niwot_3","Niwot_4","Toolik_1","Kellogg_Bio_Station","Konza_1","HJAndrews_1","CedarCreek","ChichaquaBottoms","Jornada_2","Toolik_2","Bonanza Creek_3","Luquillo_3","Tapajos","Niwot_5","Konza_2","Smokey Valley","Hays","Arikaree") )

## ------------------------------------------ ##
# SIMPLE LINEAR REGRESSIONS AND EXPONENTIAL DECAY MODEL FOR CROSS-SITE ANALYSES FOR SLOW AND TOTAL P -----
## ------------------------------------------ ##

### SLOW P ANALYSES

## MAKING FIGURES 
# library(MASS) # to access Animals data sets
# library(scales) # to access break formatting functions

dataset_means_slowP <- dataset_means_slowP %>% 
  mutate(log_mean_N = log(mean_N) )

# Renaming datasets to remomve underscores 
dataset_means_slowP <- dataset_means_slowP %>% 
  mutate(dataset = recode(dataset, 
                          Jornada_2 = "Jornada (2)",
                          Luquillo_1= "Luquillo (1)",
                          Luquillo_2 = "Luquillo (2)",
                          Luquillo_3 = "Luquillo (3)",
                          Niwot_1 = "Niwot (1)",
                          Niwot_2 = "Niwot (2)",
                          Niwot_5 = "Niwot (5)",
                          Konza_2 = "Konza (2)",
                          Sevilleta_1 = "Sevilleta (1)")) 

# %>% 
# rename('Ratio of Slow P over Total P' = mean_ratio) 


## ------------------------------------------ ##
# Final figures for paper and statistical model results  -----
## ------------------------------------------ ##

library(ggrepel)

TotalN_SlowPfig <- ggplot(data = dataset_means_slowP,
                           aes(x=mean_P, y=mean_N) ) +
  geom_point(aes(color = mean_ratio), size=3) + # removing se size for now 
  labs(title = "Slow P versus Total N",
       y = "Total N (%)") + 
  xlab(bquote(Slow~P~(mg~kg^-1))) +
  # geom_label_repel(data = dataset_means_slowP,
  #           aes(label = dataset), nudge_x=0.45, nudge_y=0.025,
  #           arrow=NULL) +
  # stat_smooth(method = 'lm', se = FALSE, color = "black", linetype = "dashed") +
  theme_bw() +
  scale_color_gradientn(colours = rainbow(5)) +
  labs(color = "Slow to Total P")  +
  theme(
    plot.title = element_text(size = 20),        # Title text size
    axis.title.x = element_text(size = 14),      # X-axis title text size
    axis.title.y = element_text(size = 14),      # Y-axis title text size
    axis.text.x = element_text(size = 12),       # X-axis text size
    axis.text.y = element_text(size = 12),       # Y-axis text size
    legend.title = element_text(size = 14),      # Legend title text size
    legend.text = element_text(size = 12)        # Legend text size
  ) +
  theme(legend.position = c(0.95, 0.95),  # x and y coordinates (0 to 1)
    legend.justification = c(1, 1),   # aligns the legend's top right corner to that position
    legend.background = element_rect(fill = "white", color = "black", size = 0.5),
    panel.grid = element_blank() ) 
    
ggsave(plot = TotalN_SlowPfig, filename = "figures/TotalN_SlowPfig_dataset.7.1.png", width = 15, height = 10)

SlowP_dataset_lm <- lm(mean_N ~ mean_P, data = dataset_means_slowP)
summary(SlowP_dataset_lm)
tab_model(SlowP_dataset_lm)

SlowP_dataset_ExpM <- nls(mean_N ~ exp(-k*mean_P), start = list(k=0.5), data = dataset_means_slowP)
summary(SlowP_dataset_ExpM)
tab_model(SlowP_dataset_ExpM)


dataset_means_totalP <- dataset_means_totalP %>% 
  mutate(dataset = recode(dataset, 
                          Jornada_2 = "Jornada (2)",
                          # Luquillo_1= "Luquillo (1)",
                          Luquillo_2 = "Luquillo (2)",
                          # Luquillo_3 = "Luquillo (3)",
                          Niwot_1 = "Niwot (1)",
                          # Niwot_2 = "Niwot (2)",
                          Niwot_5 = "Niwot (5)",
                          Konza_2 = "Konza (2)",
                          Sevilleta_1 = "Sevilleta (1)")) 

# Adding a column to color by whether sites have slow P too or not 
names <- dataset_means_slowP$dataset

dataset_means_totalP <- dataset_means_totalP %>% 
  mutate(has_slow_P = if_else(dataset %in% names, "yes", "no") )

TotalN_TotalPfig <- ggplot(data = dataset_means_totalP,
                                       aes(x=mean_P, y=mean_N) ) +
  geom_point(size=3, aes(color = has_slow_P) ) + # removing se size for now 
  scale_color_manual(values = c(
    "no" = "black",
    "yes" = "red") ) +
  labs(title = "Total P versus Total N",
       y = "Total N (%)") + 
  xlab(bquote(Total~P~(mg~kg^-1))) +
  # geom_label_repel(data = dataset_means_totalP,
  #                  aes(label = dataset), nudge_x=0.45, nudge_y=0.025,
  #                  arrow=NULL, max.overlaps = 20) +
  stat_smooth(method = 'lm', se = TRUE, color = "black") +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 20),        # Title text size
    axis.title.x = element_text(size = 14),      # X-axis title text size
    axis.title.y = element_text(size = 14),      # Y-axis title text size
    axis.text.x = element_text(size = 12),       # X-axis text size
    axis.text.y = element_text(size = 12),       # Y-axis text size
    panel.grid = element_blank() ) 

TotalP_dataset_lm <- lm(mean_N ~ mean_P, data = dataset_means_totalP)
summary(TotalP_dataset_lm)
tab_model(TotalP_dataset_lm)

comb <- cowplot::plot_grid(TotalN_TotalPfig,TotalN_SlowPfig)

# controls_y_FIXED <- cowplot::plot_grid(group1_fig_F, group2_fig_F, group3_fig_F, group4_fig_F)

ggsave(plot = comb, filename = "figures/CrossSite_Figure.7.1.png", width = 11, height = 5)


TotalN_TotalPfig_dataset_log <- ggplot(data = dataset_means_totalP,
                                  aes(x=mean_P, y=mean_N) ) +
  geom_point(aes(color=dataset), size=3) + # removing se size for now 
  labs(title = "Total P versus Total N (Log transformed)",
       y = "Total N (%)") + 
  xlab(bquote(Total~P~(mg~kg^-1))) +
  geom_label_repel(data = dataset_means_totalP,
                   aes(label = dataset), nudge_x=0.45, nudge_y=0.025,
                   arrow=NULL, max.overlaps = 20) +
  stat_smooth(method = 'lm', se = TRUE, color = "black") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 20),        # Title text size
    axis.title.x = element_text(size = 14),      # X-axis title text size
    axis.title.y = element_text(size = 14),      # Y-axis title text size
    axis.text.x = element_text(size = 12),       # X-axis text size
    axis.text.y = element_text(size = 12),       # Y-axis text size
    legend.title = element_text(size = 14),      # Legend title text size
    legend.text = element_text(size = 12)        # Legend text size
  ) +
  labs(color = "Dataset")  +
  scale_x_log10() +
  scale_y_log10()

ggsave(plot = TotalN_TotalPfig_dataset_log, filename = "figures/TotalN_TotalPfig_dataset_logged.png", width = 15, height = 10)
# ggsave(plot = TotalN_TotalPfig_dataset, filename = "figures/TotalN_TotalPfig_dataset.png", width = 15, height = 10)


test <- subset(dataset_means_slowP, dataset == "Niwot (1)" | dataset == "Niwot (2)" | dataset == "Niwot (5)" )

# figure with just Niwot sites on it 
Niwot_sites <- ggplot(data = subset(dataset_means_slowP, dataset == "Niwot (1)" | dataset == "Niwot (2)" | dataset == "Niwot (5)" ),aes(x=mean_P, y=mean_N) ) +
  geom_point(aes(color = mean_ratio), size=3) + # removing se size for now 
  labs(title = "Slow P versus Total N",
       y = "Total N (%)") + 
  xlab(bquote(Slow~P~(mg~kg^-1))) +
  geom_label_repel(data = dataset_means_slowP,
                   aes(label = dataset), nudge_x=0.45, nudge_y=0.025,
                   arrow=NULL) +
  stat_smooth(method = 'lm', se = TRUE, color = "black") +
  theme_bw() +
  scale_color_gradientn(colours = rainbow(5)) +
  labs(color = "Ratio of Slow P to Total P")  +
  theme(
    plot.title = element_text(size = 20),        # Title text size
    axis.title.x = element_text(size = 14),      # X-axis title text size
    axis.title.y = element_text(size = 14),      # Y-axis title text size
    axis.text.x = element_text(size = 12),       # X-axis text size
    axis.text.y = element_text(size = 12),       # Y-axis text size
    legend.title = element_text(size = 14),      # Legend title text size
    legend.text = element_text(size = 12)        # Legend text size
  ) 

ggsave(plot = Niwot_sites, filename = "figures/TotalN_SlowPfig_Niwot_sites.png", width = 15, height = 10)


SlowPfig_dataset2 <- ggplot(data = subset(dataset_means_slowP, dataset != "Niwot_5"),
                           aes(x=mean_ratio, y=mean_N) ) +
  geom_point(aes(color = dataset), ) + # removing se size for now 
  labs(title = "Slow P versus Total N by Dataset",
       x = "Ratio of Slow P over Total P",
       y = "Total N %") + 
  geom_text_repel(data = subset(dataset_means_slowP, dataset != "Niwot_5"), 
            aes(label = dataset), nudge_x=0.05, nudge_y=0.01,
            check_overlap=T) +
  stat_smooth(method = 'lm', se = TRUE, color = "black") +
  theme_bw() 

ratio_dataset_lm <- lm(mean_N ~ mean_ratio, data = dataset_means_slowP)
summary(ratio_dataset_lm)
tab_model(ratio_dataset_lm)














### END of Figure 1 ###

# + 
#   scale_color_gradientn(colours = rainbow(5))


# data = subset(dataset_means_slowP, dataset != "Niwot_5")

# +
#   scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                 labels = trans_format("log10", math_format(10^.x)))

# +
#   scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x) )

ggsave(plot = SlowPfig_dataset, filename = "figures/SlowP_TotalN_datasets_woNWT5.png", width = 10, height = 8)

SlowPfig_site <- ggplot(data = site_means_slowP, aes(x=mean_P, y=mean_N, color = dataset) ) +
  geom_point() + #size = 1/se_P removing se size for now 
  labs(title = "Slow P versus Total N by Site",
       x = "Slow P mg/kg",
       y = "Total N %") + 
  geom_text(data = site_means_slowP, aes(label = dataset), nudge_x=0.45, nudge_y=0.025,
            check_overlap=T) +
  stat_smooth(method = 'lm', se = TRUE, color = "black") +
  theme_bw()

ggsave(plot = SlowPfig_site, filename = "figures/SlowP_TotalN_sites.png", width = 7, height = 4)

## RUNNING MODELS

# Slow P modeling with SITE averages # p-value: 0.8663
SlowP_site_lm <- lm(mean_N ~ mean_P, data = site_means_slowP)
summary(SlowP_site_lm)
tab_model(SlowP_site_lm)

# Slow P modeling with DATASET averages

# Simple linear model# = p-value: 0.1401
# woah p-value went way up after the edit to SEV, p-value: 0.8467
# after manually updating Sev1 mean p-value: 0.128
SlowP_dataset_lm <- lm(mean_N ~ mean_P, data = dataset_means_slowP)
summary(SlowP_dataset_lm)
tab_model(SlowP_dataset_lm)

# running model without Niwot 5 
SlowP_dataset_lm_woNWT5 <- lm(mean_N ~ mean_P, data = subset(dataset_means_slowP, dataset != "Niwot_5"))
summary(SlowP_dataset_lm_woNWT5)
tab_model(SlowP_dataset_lm_woNWT5)

# Log-log transformed linear model # p-value: 0.2318
# After editing, SEV p-value: 0.818
SlowP_dataset_lm_log <- lm(log(mean_N) ~ mean_P, data = dataset_means_slowP)
summary(SlowP_dataset_lm_log)
tab_model(SlowP_dataset_lm_log)
# , weights = 1/se_P

## Exponential decay models 
# After editing, SEV p-value: 0.3
# Hmm not sure if this is working well anymore after adding the SEV data 
SlowP_dataset_ExpDec <- nls(mean_N ~ exp(-k*mean_P), start = list(k=0.5), data = dataset_means_slowP)
summary(SlowP_dataset_ExpDec)
tab_model(SlowP_dataset_ExpDec)

# Adding model fit line to figure 
SlowPfig_dataset <- SlowPfig_dataset + 
  stat_smooth(method = 'nls', 
              method.args = list(start = c(a=-0.4096, b=0.005)), 
              formula = y~a*exp(b*x), colour = 'black', linetype="dashed", se = FALSE) +
  theme_minimal() 

### Running dataset model making Luquillo means zero 
dataset_means_slowP_LUQ_Zero <- dataset_means_slowP
dataset_means_slowP_LUQ_Zero$mean_P <- ifelse(dataset_means_slowP_LUQ_Zero$dataset == "Luquillo_1",0.00001,dataset_means_slowP_LUQ_Zero$mean_P)
dataset_means_slowP_LUQ_Zero$mean_P <- ifelse(dataset_means_slowP_LUQ_Zero$dataset == "Luquillo_2",0.00001,dataset_means_slowP_LUQ_Zero$mean_P)

model_params <- dataset_means_slowP_LUQ_Zero %>%       # save the regression line yhat points
  mutate(fit=fitted(SlowP_dataset_lm_LUQ0))

# slow P fig with Luquillo being 0.00001
SlowPfig_dataset_LUQZERO <- ggplot(data = dataset_means_slowP_LUQ_Zero, aes(x=mean_P, y=mean_N, color = dataset) ) +
  geom_point() + # removing se size for now 
  labs(title = "Slow Phosphorus versus Total Nitrogen by Dataset",
       x = "Slow P mg/kg",
       y = "Total N %") + 
  geom_text(data = dataset_means_slowP_LUQ_Zero, aes(label = dataset), nudge_x=0.45, nudge_y=0.025,
            check_overlap=T) +
  theme_minimal() +
  geom_line(data = model_params, aes(y=fit), color='blue')
  
  # stat_smooth(method = 'lm',
  #             method.args = list(start = c(m=-0.02, b=0.005)),
  #             formula = mean_N~m*log(mean_P)+b, colour = 'black', linetype="dashed", se = FALSE, data = dataset_means_slowP_LUQ_Zero)
 
# #   stat_smooth(method = 'nls', 
#   method.args = list(start = c(a=-0.4096, b=0.005)), 
# formula = y~a*exp(b*x), colour = 'black', linetype="dashed", se = FALSE) +



# fig and model 
SlowPfig_dataset_Luqillo_zero <- ggplot(data = dataset_means_slowP_LUQ_Zero, aes(x=mean_P, y=mean_N, color = dataset) ) +
  geom_point() + # removing se size for now 
  labs(title = "Slow P versus Total N by Dataset",
       x = "Slow P mg/kg",
       y = "Total N %") + 
  geom_text(data = dataset_means_slowP_LUQ_Zero, aes(label = dataset), nudge_x=0.45, nudge_y=0.025,
            check_overlap=T) +
  theme_minimal()

# Simple linear model # = p-value: 0.1078
SlowP_dataset_lm_LUQ0 <- lm(mean_N ~ log(mean_P), data = dataset_means_slowP_LUQ_Zero)
summary(SlowP_dataset_lm_LUQ0)
tab_model(SlowP_dataset_lm_LUQ0)

## Exponential decay models p-value = 0.11
SlowP_dataset_ExpDec_LUQ0 <- nls(mean_N ~ exp(-k*mean_P), start = list(k=0.5), data = dataset_means_slowP_LUQ_Zero)
summary(SlowP_dataset_ExpDec_LUQ0)
tab_model(SlowP_dataset_ExpDec_LUQ0)

AIC(SlowP_dataset_lm_LUQ0,SlowP_dataset_ExpDec_LUQ0)

### Running site model making Luquillo means zero 
site_means_slowP_LUQ_Zero <- site_means_slowP
site_means_slowP_LUQ_Zero$mean_P <- ifelse(site_means_slowP_LUQ_Zero$dataset == "Luquillo_1",0.00001,site_means_slowP_LUQ_Zero$mean_P)
site_means_slowP_LUQ_Zero$mean_P <- ifelse(site_means_slowP_LUQ_Zero$dataset == "Luquillo_2",0.00001,site_means_slowP_LUQ_Zero$mean_P)

SlowPfig_site_Luqillo_zero <- ggplot(data = site_means_slowP_LUQ_Zero, aes(x=mean_P, y=mean_N, color = dataset) ) +
  geom_point() + #size = 1/se_P removing se size for now 
  labs(title = "Slow P versus Total N by Site",
       x = "Slow P mg/kg",
       y = "Total N %") + 
  geom_text(data = site_means_slowP_LUQ_Zero, aes(label = dataset), nudge_x=0.45, nudge_y=0.025,
            check_overlap=T) +
  theme_minimal()

### TOTAL P ANALYSES

## MAKING FIGURES 
TotalPfig_dataset <- ggplot(data = dataset_means_totalP, aes(x=mean_P, y=mean_N) ) +
  geom_point(aes(color = dataset)) + # removing se size for now 
  # geom_smooth(method=lm, se=TRUE, color = "black") +
  labs(title = "Total P versus Total N by Dataset",
       x = "Total P mg/kg",
       y = "Total N %") + 
  geom_text(data = dataset_means_totalP, aes(label = dataset,color = dataset), nudge_x=0.45, nudge_y=0.025,
            check_overlap=T) +
  theme_bw() 
  
ggsave(plot = TotalPfig_dataset, filename = "figures/TotalP_TotalN_datasets.png", width = 7, height = 4)

TotalPfig_site <- ggplot(data = site_means_totalP, aes(x=mean_P, y=mean_N, color = dataset) ) +
  geom_point() + #size = 1/se_P removing se size for now +
  geom_smooth(method=lm, se=FALSE, color = "black") +
  labs(title = "Total P versus Total N by Site",
       x = "Total P mg/kg",
       y = "Total N %") + 
  geom_text(data = site_means_totalP, aes(label = dataset), nudge_x=0.45, nudge_y=0.025,
            check_overlap=T) +
  theme_minimal()

## RUNNING MODELS

# Total P modeling with SITE 
# After fixing SEV, p-value: 6.301e-06
TotalP_site_lm <- lm(mean_N ~ mean_P, data = site_means_totalP)
summary(TotalP_site_lm)
tab_model(TotalP_site_lm)

# Slow P modeling with DATASET averages

# Simple linear model
# After fixing SEV, p-value: p-value: 0.03679
TotalP_dataset_lm <- lm(mean_N ~ mean_P, data = dataset_means_totalP)
summary(TotalP_dataset_lm)
tab_model(TotalP_dataset_lm)

# Log-log transformed linear model 
TotalP_dataset_lm_log <- lm(log(mean_N) ~ log(mean_P), data = dataset_means_totalP)
summary(TotalP_dataset_lm_log)
tab_model(TotalP_dataset_lm_log)
# , weights = 1/se_P

# THIS MODEL FIT DOESN'T REALLY MAKE SENSE ANYMORE
## Exponential decay models 
# TotalP_dataset_ExpDec <- nls(mean_N ~ exp(-k*mean_P), start = list(k=0.4096), data = dataset_means_totalP)
# summary(TotalP_dataset_ExpDec)
# tab_model(TotalP_dataset_ExpDec)
# 
# # Adding model fit line to figure 
# SlowPfig_dataset <- SlowPfig_dataset + 
#   stat_smooth(method = 'nls', 
#               method.args = list(start = c(a=-0.4096, b=0.005)), 
#               formula = y~a*exp(b*x), colour = 'black', linetype="dashed", se = FALSE) +
#   theme_minimal() 

## ------------------------------------------ ##
# For loops by dataset for simple linear regressions of total P and slow P versus total N  -----
## ------------------------------------------ ##

# BEGIN CODE ELLERY IS EDITING - (Started by copying and editing Craig's for loop code from below)

# SIMPLE LINEAR REGRESSIONS BY SITE OF TOTAL P VS TOTAL N

# The following code chunks produces a summary table with the slope, p-value and r-squared for each dataset, doing a simple linear model for each. (Write 1-2 sentences in results section just summarizing this table (put table in appendix and reference))

# making copy of cores dataset
cores_totalP<-cores
# filter out missing total P or N observations
cores_totalP<-subset(cores_totalP,is.na(total.P_conc_mg.kg)==F) # keep rows where total P is not not na 
cores_totalP<-subset(cores_totalP,is.na(N_conc_percent)==F) # same for total N
cores_totalP<-subset(cores_totalP,lter!="CDR")#remove CDR because only 1 observation
cores_totalP<-subset(cores_totalP,dataset!="Konza_2")#remove Konza_2 because only 1 observation
cores_totalP<-subset(cores_totalP,dataset!="Toolik_1")# Total P showing up as zero but should be NA
cores_totalP$set<-as.numeric(as.factor(cores_totalP$dataset)) # produces table with only observations that have both total P and total N

# double check no NA in N or P columns
print("Count of missing values in P column")
sum(is.na(cores_totalP$total.P_conc_mg.kg))

print("Count of missing values in N column")
sum(is.na(cores_totalP$N_conc_percent))

# for loop subsets by dataset and produces the summary of an lm for each dataset, pulls parameters from summary and includes in table for each site 

# Initialize an empty data frame
sum_table <- data.frame(
  dataset = character(),
  n = integer(),
  slope = numeric(),
  intercept = numeric(),
  Equation = character(),
  adj_rsquared = numeric(),
  m_rsquared = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

# Loop through datasets
for (i in 1:max(cores_totalP$set)) {
  a <- subset(cores_totalP, set == i)
  
  if (nrow(a) >= 3 && length(unique(a$total.P_conc_mg.kg)) > 1) {
    model <- lm(N_conc_percent ~ total.P_conc_mg.kg, data = a)
    b <- summary(model)
    
    intercept_val <- b$coefficients[1, 1]
    slope_val <- b$coefficients[2, 1]
    intercept_r <- round(intercept_val, 3)
    slope_r <- round(slope_val, 3)
    eqn <- paste0("y = ", slope_r, "x + ", intercept_r)
    
    new_row <- data.frame(
      dataset = a$dataset[1],
      n = nrow(a),
      slope = slope_val,
      intercept = intercept_val,
      Equation = eqn,
      adj_rsquared = round(b$adj.r.squared, 3),
      m_rsquared = round(b$r.squared, 3),
      p_value = round(b$coefficients[2, 4], 4)
    )
  } else {
    new_row <- data.frame(
      dataset = a$dataset[1],
      n = nrow(a),
      slope = NA,
      intercept = NA,
      Equation = NA,
      adj_rsquared = NA,
      m_rsquared = NA,
      p_value = NA
    )
  }
  
  sum_table <- rbind(sum_table, new_row)
}

Final_table_totalPstats<-merge(Final_table,sum_table,all.x = T) # merging the looped product table with final table that has all possible datasets in our study       

# double checking stats values are correct
testdata <- cores %>% filter(dataset == "Calhoun")
testdata_lm <- lm(N_conc_percent ~ total.P_conc_mg.kg, data = testdata)
summary(testdata_lm)
tab_model(testdata_lm)

# SIMPLE LINEAR REGRESSIONS BY SITE OF SLOW P VS TOTAL N

# making copy of cores dataset
cores_SlowP<-cores
# filter out missing slow P or N observations
cores_SlowP<-subset(cores_SlowP,is.na(slow.P_conc_mg.kg)==F)
cores_SlowP<-subset(cores_SlowP,is.na(N_conc_percent)==F)
cores_SlowP<-subset(cores_SlowP,lter!="CDR") #remove CDR because only 1 observation
cores_SlowP<-subset(cores_SlowP,dataset!="Konza_2") #remove Konza_2 because only 1 observation
# cores_SlowP<-subset(cores_SlowP,dataset!="Niwot_5") # Slow P same values for all 3 observations
cores_SlowP$set<-as.numeric(as.factor(cores_SlowP$dataset))

# double check no NA in N or P columns
print("Count of missing values in P column")
sum(is.na(cores_SlowP$slow.P_conc_mg.kg))

print("Count of missing values in N column")
sum(is.na(cores_SlowP$N_conc_percent))

# for loop subsets by dataset and produces the summary of an lm for each dataset, pulls parameters from summary and includes in table for each site 

# Initialize an empty data frame
sum_table2 <- data.frame(
  dataset = character(),
  n = integer(),
  slope = numeric(),
  intercept = numeric(),
  Equation = character(),
  adj_rsquared = numeric(),
  m_rsquared = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

# Loop through datasets
for (i in 1:max(cores_SlowP$set)) {
  a <- subset(cores_SlowP, set == i)
  
  if (nrow(a) >= 3 && length(unique(a$slow.P_conc_mg.kg)) > 1) {
    model <- lm(N_conc_percent ~ slow.P_conc_mg.kg, data = a)
    b <- summary(model)
    
    intercept_val <- b$coefficients[1, 1]
    slope_val <- b$coefficients[2, 1]
    intercept_r <- round(intercept_val, 3)
    slope_r <- round(slope_val, 3)
    eqn <- paste0("y = ", slope_r, "x + ", intercept_r)
    
    new_row <- data.frame(
      dataset = a$dataset[1],
      n = nrow(a),
      slope = slope_val,
      intercept = intercept_val,
      Equation = eqn,
      adj_rsquared = round(b$adj.r.squared, 3),
      m_rsquared = round(b$r.squared, 3),
      p_value = round(b$coefficients[2, 4], 4)
    )
  } else {
    new_row <- data.frame(
      dataset = a$dataset[1],
      n = nrow(a),
      slope = NA,
      intercept = NA,
      Equation = NA,
      adj_rsquared = NA,
      m_rsquared = NA,
      p_value = NA
    )
  }
  
  sum_table2 <- rbind(sum_table2, new_row)
}

Final_table_slowPstats<-merge(Final_table,sum_table2,all.x = T) # merging the looped product table with final table that has all possible datasets in our study       

# double checking stats values are correct
testdata <- cores %>% filter(dataset == "Niwot_1")
testdata_lm <- lm(N_conc_percent ~ slow.P_conc_mg.kg, data = testdata)
summary(testdata_lm)
tab_model(testdata_lm)

# END CODE ELLERY IS EDITING 


# SIMPLE LINEAR REGRESSIONS BY SITE OF TOTAL P VS TOTAL N

# This code produces a summary table with the slope and p-value for each dataset, doing a simple linear model
# Write 1-2 sentences in results section just summarizing this table (put table in appendix and reference)
# creates empty summary table
sum_table<-data.frame(matrix(nrow=length(unique(cores$dataset)),ncol=3)) 
names(sum_table) <- c('dataset', 'Total_P.N_slope', 'Total_P.N_.pvalue')
# making copy of cores dataset
cores_totalP<-cores
# filter out missing total P or N observations
cores_totalP<-subset(cores_totalP,is.na(total.P_conc_mg.kg)==F) # keep rows where total P is not not na 
cores_totalP<-subset(cores_totalP,is.na(N_conc_percent)==F) # same for total N
cores_totalP<-subset(cores_totalP,lter!="CDR")#remove CDR because only 1 observation
cores_totalP<-subset(cores_totalP,dataset!="Konza_2")#remove Konza_2 because only 1 observation
cores_totalP<-subset(cores_totalP,dataset!="Toolik_1")# Total P showing up as zero but should be NA
cores_totalP$set<-as.numeric(as.factor(cores_totalP$dataset)) # produces table with only observations that have both total P and total N
# for loop subsets by dataset and produces the summary of an lm for each dataset, pulls parameters from summary and includes in table for each site 
for(i in 1:max(cores_totalP$set)){
  a<-subset(cores_totalP,set==i)
  b<-summary(lm(N_conc_percent~total.P_conc_mg.kg,data = a))
  sum_table[i,1]<-first(a$dataset)
  sum_table[i,2]<-b$coefficients[2,1]
  sum_table[i,3]<-b$coefficients[2,4]
}
Final_table<-merge(Final_table,sum_table,all.x = T) # merging the looped product table with final table that has all possible datasets in our study                

# SIMPLE LINEAR REGRESSIONS BY SITE OF SLOW P VS TOTAL N
# 
sum_table<-data.frame(matrix(nrow=length(unique(cores$dataset)),ncol=3))
names(sum_table) <- c('dataset', 'Slow_P.N_slope', 'Slow_P.N_.pvalue')
cores_SlowP<-cores
cores_SlowP<-subset(cores_SlowP,is.na(slow.P_conc_mg.kg)==F)
cores_SlowP<-subset(cores_SlowP,is.na(N_conc_percent)==F)
cores_SlowP<-subset(cores_SlowP,lter!="CDR") #remove CDR because only 1 observation
cores_SlowP<-subset(cores_SlowP,dataset!="Konza_2") #remove Konza_2 because only 1 observation
cores_SlowP<-subset(cores_SlowP,dataset!="Niwot_5") # Slow P same values for all 3 observations
cores_SlowP$set<-as.numeric(as.factor(cores_SlowP$dataset))
for(i in 1:max(cores_SlowP$set)){
  a<-subset(cores_SlowP,set==i)
  b<-summary(lm(N_conc_percent~slow.P_conc_mg.kg,data = a))
  sum_table[i,1]<-first(a$dataset)
  sum_table[i,2]<-b$coefficients[2,1]
  sum_table[i,3]<-b$coefficients[2,4]
}
Final_table<-merge(Final_table,sum_table,all.x = T)     

sum_table<-data.frame(matrix(nrow=length(unique(cores$dataset)),ncol=3))
names(sum_table) <- c('dataset', 'Total_P.N_slope_Plot', 'Total_P.N_.pvalue_Plot')
# plot_totalP<-cores
plot_totalP<-subset(plot_totalP,is.na(total.P_conc_mg.kg)==F)
plot_totalP<-subset(plot_totalP,is.na(N_conc_percent)==F)
plot_totalP<-subset(plot_totalP,lter!="CDR")#remove CDR because only 1 observation
plot_totalP<-subset(plot_totalP,dataset!="Konza_2")#remove Konza_2 because only 1 observation
plot_totalP<-subset(plot_totalP,dataset!="Toolik_1")# Total P showing up as zero but should be NA
plot_totalP$set<-as.numeric(as.factor(plot_totalP$dataset))

for(i in 1:max(plot_totalP$set)){
  a<-subset(plot_totalP,set==i)
  b<-summary(lm(N_conc_percent~total.P_conc_mg.kg,data = a))
  sum_table[i,1]<-first(a$dataset)
  sum_table[i,2]<-b$coefficients[2,1]
  sum_table[i,3]<-b$coefficients[2,4]
}

Final_table<-merge(Final_table,sum_table,all.x = T)               

sum_table<-data.frame(matrix(nrow=length(unique(cores$dataset)),ncol=3))
names(sum_table) <- c('dataset', 'Slow_P.N_slope_Plot', 'Slow_P.N_.pvalue_Plot')
plot_slowP<-subset(plot_slowP,is.na(slow.P_conc_mg.kg)==F)
plot_slowP<-subset(plot_slowP,is.na(N_conc_percent)==F)
plot_slowP<-subset(plot_slowP,dataset!="CedarCreek_1")#remove CDR because only 1 observation
plot_slowP<-subset(plot_slowP,dataset!="Konza_2")#remove Konza_2 because only 1 observation
plot_slowP<-subset(plot_slowP,dataset!="Niwot_5")# 
plot_slowP$set<-as.numeric(as.factor(plot_slowP$dataset))
for(i in 1:max(plot_slowP$set)){
  a<-subset(plot_slowP,set==i)
  b<-summary(lm(N_conc_percent~slow.P_conc_mg.kg,data = a))
  sum_table[i,1]<-first(a$dataset)
  sum_table[i,2]<-b$coefficients[2,1]
  sum_table[i,3]<-b$coefficients[2,4]
}
Final_table<-merge(Final_table,sum_table,all.x = T)   

sum_table<-data.frame(matrix(nrow=length(unique(cores$dataset)),ncol=3))
names(sum_table) <- c('dataset', 'Total_P.N_slope_site', 'Total_P.N_.pvalue_site')
site_totalP<-subset(site_totalP,is.na(total.P_conc_mg.kg)==F)
site_totalP<-subset(site_totalP,is.na(N_conc_percent)==F)
site_totalP<-subset(site_totalP,dataset!="CedarCreek_1")#remove CDR because only 1 observation
site_totalP<-subset(site_totalP,dataset!="CedarCreek_2")#remove CDR because only 1 observation
site_totalP<-subset(site_totalP,dataset!="Konza_2")#remove Konza_2 because only 1 observation
site_totalP<-subset(site_totalP,dataset!="Toolik_1")# Total P showing up as zero but should be NA
site_totalP<-subset(site_totalP,dataset!="Niwot_3")# Total P showing up as zero but should be NA
site_totalP<-subset(site_totalP,dataset!="Niwot_4")# Total P showing up as zero but should be NA

site_totalP$set<-as.numeric(as.factor(site_totalP$dataset))
for(i in 1:max(site_totalP$set)){
  a<-subset(site_totalP,set==i)
  b<-summary(lm(N_conc_percent~total.P_conc_mg.kg,data = a))
  sum_table[i,1]<-first(a$dataset)
  sum_table[i,2]<-b$coefficients[2,1]
  sum_table[i,3]<-b$coefficients[2,4]
}
Final_table<-merge(Final_table,sum_table,all.x = T)               

sum_table<-data.frame(matrix(nrow=length(unique(cores$dataset)),ncol=3))
names(sum_table) <- c('dataset', 'Slow_P.N_slope_site', 'Slow_P.N_.pvalue_site')
site_slowP<-subset(site_slowP,is.na(slow.P_conc_mg.kg)==F)
site_slowP<-subset(site_slowP,is.na(N_conc_percent)==F)
site_slowP<-subset(site_slowP,dataset!="CedarCreek_1")#remove CDR because only 1 observation
site_slowP<-subset(site_slowP,dataset!="Konza_2")#remove Konza_2 because only 1 observation
site_slowP<-subset(site_slowP,dataset!="Niwot_5")# 
site_slowP$set<-as.numeric(as.factor(site_slowP$dataset))
for(i in 1:max(site_slowP$set)){
  a<-subset(site_slowP,set==i)
  b<-summary(lm(N_conc_percent~slow.P_conc_mg.kg,data = a))
  sum_table[i,1]<-first(a$dataset)
  sum_table[i,2]<-b$coefficients[2,1]
  sum_table[i,3]<-b$coefficients[2,4]
}
Final_table<-merge(Final_table,sum_table,all.x = T)   

## ------------------------------------------ ##
# Moving previous code down here that we don't need for now 5/12/24 EV  -----
## ------------------------------------------ ##

# LOWERING CODE WE DONT NEED FOR NOW
# mean 
dataset_slowP_mean <- aggregate(cbind(N_conc_percent,slow.P_conc_mg.kg)~dataset,mean,data=cores,na.rm=T)

# standard deviation 
dataset_slowP_sd <- aggregate(cbind(N_conc_percent,slow.P_conc_mg.kg)~dataset,sd,data=cores,na.rm=T)

# sample size
dataset_slowP_ss <- aggregate(cbind(N_conc_percent,slow.P_conc_mg.kg)~dataset,n(),data=cores,na.rm=T)


sum_table<-data.frame(matrix(nrow=length(unique(cores$dataset)),ncol=3))
names(sum_table) <- c('dataset', 'Slow_P.N_slope_dataset', 'Slow_P.N_.pvalue_dataset')
dataset_slowP<-subset(dataset_slowP,is.na(slow.P_conc_mg.kg)==F)
dataset_slowP<-subset(dataset_slowP,is.na(N_conc_percent)==F)
dataset_slowP<-subset(dataset_slowP,dataset!="CedarCreek_1")#remove CDR because only 1 observation
dataset_slowP<-subset(dataset_slowP,dataset!="Konza_2")#remove Konza_2 because only 1 observation
dataset_slowP<-subset(dataset_slowP,dataset!="Niwot_5")# 
dataset_slowP$set<-as.numeric(as.factor(dataset_slowP$dataset))
for(i in 1:max(dataset_slowP$set)){
  a<-subset(dataset_slowP,set==i)
  b<-summary(lm(N_conc_percent~slow.P_conc_mg.kg,data = a))
  sum_table[i,1]<-first(a$dataset)
  sum_table[i,2]<-b$coefficients[2,1]
  sum_table[i,3]<-b$coefficients[2,4]
}
Final_table<-merge(Final_table,sum_table,all.x = T)  




# Now we'll want to add together our various types of P (conditionally)
p_sums <- sparc_v2 %>%
  # First need to fill NAs with 0s to avoid making NA sums
  ## Pivot longer
  tidyr::pivot_longer(cols = c(dplyr::starts_with("P_"),
                               dplyr::starts_with("Po_"),
                               dplyr::starts_with("ReBHsin_"),
                               dplyr::starts_with("Pi_")),
                      names_to = "names", values_to = "values") %>%
  ## Remove NA / missing values
  dplyr::filter(!is.na(values) & nchar(values) != 0) %>%
  ## Pivot back to wide format and fill empty cells with 0
  tidyr::pivot_wider(names_from = names, values_from = values, values_fill = 0) %>%
  # Calculate slow P conditionally
  ## NOTE: using placeholder (obviously wrong) while we await complete methods data knowledge / inclusion in data key
  dplyr::mutate(slow.P_conc_mg.kg = 1) %>%
  # dplyr::mutate(slow.P_mg_kg = dplyr::case_when(
  #   dataset == "HJAndrews_1" ~ (P_conc_HCl_mg_kg_1M),
  #   dataset == "Bonanza Creek_1" ~ NA,
  #   dataset == "Bonanza Creek_2" ~ NA,
  #   dataset == "Brazil" ~ NA,
  #   dataset == "Calhoun" ~ (P_conc_HCl_mg_kg_1M),
  #   # (vvv) Data seem to only have total P but searching for Hedley fraction
  #   dataset == "CedarCreek_1" ~ NA, 
  #   dataset == "Coweeta" ~ (P_conc_HCl_mg_kg_0.5M), 
  #   # (vvv) Only have a neutral salt extraction (available P). May remove dataset entirely
  #   dataset == "Fernow" ~ NA,
#   dataset == "FloridaCoastal" ~ (P_conc_HCl_mg_kg_1M),
#   dataset == "Hubbard Brook" ~ (P_conc_HNO3_cold_mg_kg_1M),
#   dataset == "Jornada_1" ~ NA,
#   dataset == "Jornada_2" ~ (P_conc_HCl_mg_kg_1M),
#   dataset == "Kellog_Biological_Station" ~ (Pi_conc_HCl_mg_kg_1M),
#   # (vvv) Based on 2 different methodologies
#   dataset == "Konza_1" ~ (P_conc_Ca_bound_mg_kg),
#   dataset == "Luquillo_1" ~ NA,
#   # (vvv) Re-check! assuming these are the correct units, and dilute HCl is 1M step from Hedley
#   dataset == "Luquillo_2" ~ (P_conc_HCl_mg_kg_1M),
#   dataset == "Niwot_1" ~ (P_conc_HCl_mg_kg_1M),
#   dataset == "Niwot_2" ~ (P_conc_HCl_mg_kg), ###Checking mmolarity but likely 1M
#   dataset == "Niwot_3" ~ NA,
#   dataset == "Niwot_4" ~ NA,
#   dataset == "Sevilleta_1" ~ (P_conc_HCl_mg_kg_1M),
#   dataset == "Sevilleta_2" ~ NA,
#   # (vvv) If resulting number is negative it gets set to zero
#   dataset == "Toolik_1" ~ ifelse((P_conc_HCl_mg_kg_1M - P_conc_citrate_mg_kg_0.01M) < 0,
#                                yes = 0, 
#                                no = P_conc_HCl_mg_kg_1M - P_conc_citrate_mg_kg_0.01M), 
#   TRUE ~ NA )) %>%



#TOTAL P CALCULATION

dplyr::mutate(slow.P_conc_mg.kg = 1) %>%
  # dplyr::mutate(slow.P_mg_kg = dplyr::case_when(
  #   dataset == "HJAndrews_1" ~ (P_conc_total_mg_kg),
  #   dataset == "Bonanza Creek_1" ~ (P_conc_total_mg_kg),
  #   dataset == "Bonanza Creek_2" ~ (P_conc_total_mg_kg),
  #   dataset == "Brazil" ~ (P_conc_total_mg_kg),
  #   dataset == "Calhoun" ~ (P_conc_total_mg_kg),
  #   # (vvv) Data seem to only have total P but searching for Hedley fraction
  #   dataset == "CedarCreek_1" ~ NA, 
  #   dataset == "Coweeta" ~ (P_conc_NH4Cl_mg_kg_1M + P_conc_HCO3_mg_kg_0.1M + P_conc_NaOH_mg_kg_0.1M + P_conc_HCl_mg_kg_0.5M + P_conc_residual), 
  #   # (vvv) Only have a neutral salt extraction (available P). May remove dataset entirely
  #   dataset == "Fernow" ~ NA,
#   dataset == "FloridaCoastal" ~ (P_stock_resin_mg_cm3 + P_stock_HCO3_mg_cm3_0.5M + P_stock_NaOH_mg_cm3_0.1M + P_stock_HCl_mg_cm3_1M  + P_stock_residual_mg_cm3),
#   dataset == "Hubbard Brook" ~ (P_conc_HNO3__mg_kg_1M),
#   dataset == "Jornada_1" ~ (P_conc_total_mg_kg),
#   dataset == "Jornada_2" ~ (P_conc_MgCl2_mg_kg + P_conc_NaOH_mg_kg_0.1M + P_conc_HCl_mg_kg_1M + P_conc_residual),
#   dataset == "Kellog_Biological_Station" ~ (Pi_conc_resin_mg_kg + Pi_conc_NaHCO3_mg_kg_0.5M + Po_conc_NaHCO3_mg_kg_0.5M + Pi_conc_microbial_mg_kg + Po_conc_microbial_mg_kg + Pi_conc_NaOH_mg_kg_0.1M + Po_conc_NaOH_mg_kg_0.1M + Pi_conc_sonic_NaOH_mg_kg_0.1M + Po_conc_sonic_NaOH_mg_kg_0.1M + Pi_conc_HCl_mg_kg_1M + Po_conc_residual_mg_kg),
#   # (vvv) Based on 2 different methodologies
#   dataset == "Konza_1" ~ (P_conc_Al_Fe_mg_kg + P_conc_occluded_mg_kg + P_conc_Ca_bound_mg_kg),
#   dataset == "Luquillo_1" ~ NA,
#   # (vvv) Re-check! assuming these are the correct units, and dilute HCl is 1M step from Hedley
#   dataset == "Luquillo_2" ~ (P_conc_total_mg_kg),
#   dataset == "Niwot_1" ~ (P_conc_resin_mg_kg + Po_conc_HCO3_mg_kg_0.5M + Pi_conc_HCO3_mg_kg_0.5M+ Po_conc_NaOH_mg_kg_0.1M + Pi_conc_NaOH_mg_kg_0.1M + P_conc_HCl_mg_kg_1M + Po_conc_sonic_HCl_mg_kg_1M + Pi_conc_sonic_HCl_mg_kg_1M + P_conc_residual_mg_kg),
#   dataset == "Niwot_2" ~ (P_conc_resin_mg_kg + Po_conc_HCO3_mg_kg + Pi_conc_HCO3_mg_kg + Po_conc_NaOH_mg_kg + Pi_conc_NaOH_mg_kg + P_conc_HCl_mg_kg + P_conc_residual_mg_kg),
#   dataset == "Niwot_3" ~ (P_conc_total_mg_kg),
#   dataset == "Niwot_4" ~ (P_conc_total_mg_kg),
#   dataset == "Sevilleta_1" ~ (P_conc_total_mg_kg),
#   dataset == "Sevilleta_2" ~ (P_conc_total_mg_kg),
#   # (vvv) If resulting number is negative it gets set to zero
#   dataset == "Toolik_1" ~ NA, 
#   TRUE ~ NA )) %>%


## ------------------------------------------ ##
# Within site Slow P versus Total N Figures  -----
## ------------------------------------------ ##

#### Sevilleta ##### 
aa<-subset(cores,dataset=="Sevilleta_1")
plot<-aggregate(cbind(N_conc_percent,total.P_conc_mg.kg,slow.P_conc_mg.kg)~plot,mean, data=aa,na.rm=T)

Sevilleta_1 <-ggplot(data = aa, aes(slow.P_conc_mg.kg,N_conc_percent,  fill = plot) ) +
  geom_smooth(aes(group = 1), method = "lm", colour="black", se = TRUE, size=0.5, alpha=0.3) +
  # geom_smooth(data = plot, aes(slow.P_conc_mg.kg,N_conc_percent),method = "lm", colour="blue", se = TRUE, fill = "blue", alpha=0.15) +
  geom_point(shape = 21, size = 3, color = "black", stroke = .5 ) +
  # geom_point(data = plot, aes(slow.P_conc_mg.kg,N_conc_percent,  fill = plot),shape = 21, size=7, color = "black", stroke = .5, alpha = 0.7 ) +
  ggtitle("Sevilleta") + ylab ("Soil N (%)") +
  xlab(bquote(Slow~P~(mg~kg^-1))) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank() ) 

Sevilleta_1 

summary(lm(aa$N_conc_percent ~ aa$slow.P_conc_mg.kg))
summary(lm(plot$N_conc_percent ~ plot$slow.P_conc_mg.kg))

ggsave(plot=Sevilleta_1, filename="figures/slowP_site/Sevilleta_1.png", width = 4, height = 4)

rm(aa,plot,site)

#### CALHOUN #####
aa<-subset(cores,dataset=="Calhoun")
site<-aggregate(cbind(N_conc_percent,total.P_conc_mg.kg,slow.P_conc_mg.kg)~site,mean, data=aa,na.rm=T)

Calhoun <-ggplot(data = aa, aes(slow.P_conc_mg.kg,N_conc_percent,  fill = site) ) +
  geom_smooth(aes(group = 1), method = "lm", colour="black", se = TRUE, size=0.5, alpha=0.3) +
  # geom_smooth(data = site, aes(slow.P_conc_mg.kg,N_conc_percent),method = "lm", colour="blue", se = TRUE, fill = "blue", alpha=0.15) +
  geom_point(shape = 21, size = 3, color = "black", stroke = .5 ) +
  # geom_point(data = site, aes(slow.P_conc_mg.kg,N_conc_percent,  fill = site),shape = 21, size=7, color = "black", stroke = .5, alpha = 0.7 ) +
  ggtitle("Calhoun") + ylab ("Soil N (%)") +
  xlab(bquote(Slow~P~(mg~kg^-1))) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank() )

Calhoun

summary(lm(aa$N_conc_percent ~ aa$slow.P_conc_mg.kg))
summary(lm(site$N_conc_percent ~ site$slow.P_conc_mg.kg))

ggsave(plot=Calhoun, filename="figures/slowP_site/Calhoun.png", width = 4, height = 4)

rm(aa,plot,site)
  
#### COWEETA #####  
aa<-subset(cores,dataset=="Coweeta")
site<-aggregate(cbind(N_conc_percent,total.P_conc_mg.kg,slow.P_conc_mg.kg)~site,mean, data=aa,na.rm=T)

Coweeta <- ggplot(data = aa, aes(slow.P_conc_mg.kg,N_conc_percent,  fill = site) ) +
  geom_smooth(aes(group = 1), method = "lm", colour="black", se = TRUE, size=0.5, alpha=0.3) +
  # geom_smooth(data = site, aes(slow.P_conc_mg.kg,N_conc_percent),method = "lm", colour="blue", se = TRUE, fill = "blue", alpha=0.15) +
  geom_point(shape = 21, size = 3, color = "black", stroke = .5 ) +
  # geom_point(data = site, aes(slow.P_conc_mg.kg,N_conc_percent,  fill = site),shape = 21, size=7, color = "black", stroke = .5, alpha=0.7 ) +
  ggtitle("Coweeta") + ylab ("Soil N (%)") +
  xlab(bquote(Slow~P~(mg~kg^-1))) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank() )
Coweeta

summary(lm(aa$N_conc_percent ~ aa$slow.P_conc_mg.kg))
summary(lm(site$N_conc_percent ~ site$slow.P_conc_mg.kg))

ggsave(plot=Coweeta, filename="figures/slowP_site/Coweeta.png", width = 4, height = 4)

rm(aa,plot,site)

#### FLO RIDA #####  
# aa<-subset(cores,dataset=="FloridaCoastal")
# site<-aggregate(cbind(N_conc_percent,total.P_conc_mg.kg,slow.P_conc_mg.kg)~site,mean, data=aa,na.rm=T)
# 
# f<-ggplot(aa, aes(slow.P_conc_mg.kg,N_conc_percent,  colour = site)) +
#   geom_point(size = 2 ) +
#   geom_smooth(aes(group = 1),method = "lm", colour="black", se = FALSE) +
#   geom_point(data = site, aes(slow.P_conc_mg.kg,N_conc_percent,  colour = site),size=4, alpha=0.5)+
#   geom_smooth(data = site, aes(slow.P_conc_mg.kg,N_conc_percent),method = "lm", colour="red", se = FALSE)+
#   ggtitle("Everglades")+ xlab("Slowly Cycling P (mg/kg)") + ylab ("Soil N (%)")+
#   theme_bw()
#   f

#### HubbardBrook #####  
aa<-subset(cores,dataset=="Hubbard Brook")
site<-aggregate(cbind(N_conc_percent,total.P_conc_mg.kg,slow.P_conc_mg.kg)~site,mean, data=aa,na.rm=T)
  
Hubbard_Brook <- ggplot(data = aa, aes(slow.P_conc_mg.kg,N_conc_percent,  fill = site) ) +
  geom_smooth(aes(group = 1), method = "lm", colour="black", se = TRUE, size=0.5, alpha=0.3) +
  # geom_smooth(data = site, aes(slow.P_conc_mg.kg,N_conc_percent),method = "lm", colour="blue", se = TRUE, fill = "blue", alpha=0.15) +
  geom_point(shape = 21, size = 3, color = "black", stroke = .5 ) +
  # geom_point(data = site, aes(slow.P_conc_mg.kg,N_conc_percent,  fill = site),shape = 21, size=7, color = "black", stroke = .5, alpha=0.7 ) +
  ggtitle("Hubbard Brook") + ylab ("Soil N (%)") +
  xlab(bquote(Slow~P~(mg~kg^-1))) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank() )
Hubbard_Brook

summary(lm(aa$N_conc_percent ~ aa$slow.P_conc_mg.kg))
summary(lm(site$N_conc_percent ~ site$slow.P_conc_mg.kg))

ggsave(plot=Hubbard_Brook, filename="figures/slowP_site/Hubbard_Brook.png", width = 4, height = 4)

rm(aa,plot,site)

#### Jornada #####  
aa<-subset(cores,dataset=="Jornada_2")
site<-aggregate(cbind(N_conc_percent,total.P_conc_mg.kg,slow.P_conc_mg.kg)~site,mean, data=aa,na.rm=T)

Jornada_2<-ggplot(data = aa, aes(slow.P_conc_mg.kg,N_conc_percent,  fill = site) ) +
  geom_smooth(aes(group = 1), method = "lm", colour="black", se = TRUE, size=0.5, alpha=0.3, linetype="dashed") +
  # geom_smooth(data = site, aes(slow.P_conc_mg.kg,N_conc_percent),method = "lm", colour="blue", se = TRUE, fill = "blue", alpha=0.15) +
  geom_point(shape = 21, size = 3, color = "black", stroke = .5 ) +
  # geom_point(data = site, aes(slow.P_conc_mg.kg,N_conc_percent,  fill = site),shape = 21, size=7, color = "black", stroke = .5, alpha=0.7 ) +
  ggtitle("Jornada") + ylab ("Soil N (%)") +
  xlab(bquote(Slow~P~(mg~kg^-1))) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank() )
Jornada_2 

summary(lm(aa$N_conc_percent ~ aa$slow.P_conc_mg.kg))
summary(lm(site$N_conc_percent ~ site$slow.P_conc_mg.kg))

ggsave(plot=Jornada_2, filename="figures/slowP_site/Jornada_2.png", width = 4, height = 4)

rm(aa,plot,site)

#### Luquillo ##### 
aa<-subset(cores,dataset=="Luquillo_2")
site<-aggregate(cbind(N_conc_percent,total.P_conc_mg.kg,slow.P_conc_mg.kg)~site,mean, data=aa,na.rm=T)

Luquillo_2<-ggplot(data = aa, aes(slow.P_conc_mg.kg,N_conc_percent,  fill = site) ) +
  geom_smooth(aes(group = 1), method = "lm", colour="black", se = TRUE, size=0.5, alpha=0.3) +
  # geom_smooth(data = site, aes(slow.P_conc_mg.kg,N_conc_percent),method = "lm", colour="blue", se = TRUE, fill = "blue", alpha=0.15, linetype="dashed") +
  geom_point(shape = 21, size = 3, color = "black", stroke = .5 ) +
  # geom_point(data = site, aes(slow.P_conc_mg.kg,N_conc_percent,  fill = site),shape = 21, size=7, color = "black", stroke = .5, alpha=0.7 ) +
  ggtitle("Luquillo") + ylab ("Soil N (%)") +
  xlab(bquote(Slow~P~(mg~kg^-1))) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank() )
Luquillo_2 

summary(lm(aa$N_conc_percent ~ aa$slow.P_conc_mg.kg))
summary(lm(site$N_conc_percent ~ site$slow.P_conc_mg.kg))

ggsave(plot=Luquillo_2, filename="figures/slowP_site/Luquillo_2.png", width = 4, height = 4)

rm(aa,plot,site)

#### Niwot ##### 
aa<-subset(cores,dataset=="Niwot_5")
site<-aggregate(cbind(N_conc_percent,total.P_conc_mg.kg,slow.P_conc_mg.kg)~site,mean, data=aa,na.rm=T)

Niwot_5<-ggplot(data = aa, aes(slow.P_conc_mg.kg,N_conc_percent,  fill = site) ) +
  geom_smooth(aes(group = 1), method = "lm", colour="black", se = TRUE, fill="darkgrey", size=0.5, alpha=0.3) +
  # geom_smooth(data = site, aes(slow.P_conc_mg.kg,N_conc_percent),method = "lm", colour="blue", se = TRUE, fill = "blue", alpha=0.15) +
  geom_point(shape = 21, size = 3, color = "black", stroke = .5 ) +
  # geom_point(data = site, aes(slow.P_conc_mg.kg,N_conc_percent,  fill = site),shape = 21, size=7, color = "black", stroke = .5, alpha=0.7 ) +
  ggtitle("Niwot Ridge") + ylab ("Soil N (%)") +
  xlab(bquote(Slow~P~(mg~kg^-1))) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank() )
Niwot_5 

summary(lm(aa$N_conc_percent ~ aa$slow.P_conc_mg.kg))

ggsave(plot=Niwot_5 , filename="figures/slowP_site/Niwot_5.png", width = 4, height = 4)

rm(aa,plot,site)


#### Tapajos ##### 
aa<-subset(cores,dataset=="Tapajos")
site<-aggregate(cbind(N_conc_percent,total.P_conc_mg.kg,slow.P_conc_mg.kg)~site,mean, data=aa,na.rm=T)

Tapajos<-ggplot(data = aa, aes(slow.P_conc_mg.kg,N_conc_percent,  fill = site) ) +
  # geom_smooth(aes(group = 1), method = "lm", colour="black", se = TRUE, size=0.5, alpha=0.3) +
  # geom_smooth(data = site, aes(slow.P_conc_mg.kg,N_conc_percent),method = "lm", colour="blue", se = TRUE, fill = "blue", alpha=0.15) +
  geom_point(shape = 21, size = 3, color = "black", stroke = .5 ) +
  # geom_point(data = site, aes(slow.P_conc_mg.kg,N_conc_percent,  fill = site),shape = 21, size=7, color = "black", stroke = .5, alpha=0.7 ) +
  ggtitle("Tapajos") + ylab ("Soil N (%)") +
  xlab(bquote(Slow~P~(mg~kg^-1))) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank() )
Tapajos 

summary(lm(aa$N_conc_percent ~ aa$slow.P_conc_mg.kg))

ggsave(plot=Tapajos, filename="figures/slowP_site/Tapajos.png", width = 4, height = 4)

rm(aa,plot,site)

## ------------------------------------------ ##
# COMBINED Within site Slow P versus Total N figures  -----
## ------------------------------------------ ##

WithinSiteSlow <- cowplot::plot_grid(Coweeta, Calhoun, Luquillo_2, Niwot_5, Sevilleta_1,Hubbard_Brook, Jornada_2, ncol = 4)

WithinSiteSlow

ggsave(plot=WithinSiteSlow, filename="figures/slowP_site/WithinSiteSlow.5.30_nomeanpts.png", width = 13, height = 6)

## ------------------------------------------ ##
# Within site Total P versus Total N Figures  -----
## ------------------------------------------ ##

# Doesn't make since for Arikaree, Hays, Smokey Valley, ChichaquaBottoms, or Konza 2 because they all have one point each 

# linewidths 
# solid is < 0.05
# dashed between 0.05 and .10
# no line p > .10

unique(cores$dataset)

#### Bonanza Creek_1 ##### 
aa<-subset(cores,dataset=="Bonanza Creek_1")
site<-aggregate(cbind(N_conc_percent,total.P_conc_mg.kg)~site,mean, data=aa,na.rm=T)

BonanzaCreek_1 <-ggplot(data = aa, aes(total.P_conc_mg.kg,N_conc_percent, fill = site ) ) +
  geom_smooth(aes(group = 1), method = "lm", colour="black", se = TRUE, size=0.5, alpha=0.3 ) +
  geom_smooth(data = site, aes(total.P_conc_mg.kg,N_conc_percent), method = "lm", colour="blue", se = TRUE, fill = "blue", alpha=0.15, linetype = "dashed")  +
  geom_point(shape = 21, size = 3, color = "black", stroke = .5 ) +
  geom_point(data = site, aes(total.P_conc_mg.kg,N_conc_percent,  fill = site), shape = 21, size=7, color = "black", stroke = .5, alpha = 0.7)  +
  ggtitle("Bonanza Creek (1)") + ylab ("Soil N (%)") +
  xlab(bquote(Total~P~(mg~kg^-1))) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank() ) 

BonanzaCreek_1

summary(lm(aa$N_conc_percent ~ aa$total.P_conc_mg.kg)) # 0.6615 
summary(lm(site$N_conc_percent ~ site$total.P_conc_mg.kg)) # 0.6394 

ggsave(plot=BonanzaCreek_1, filename="figures/totalP_site/BNZ_1.png", width = 3, height = 3)

rm(aa,site)

#### Bonanza Creek_2 ##### 
aa<-subset(cores,dataset=="Bonanza Creek_2")
site<-aggregate(cbind(N_conc_percent,total.P_conc_mg.kg)~site,mean, data=aa,na.rm=T)

BonanzaCreek_2 <-ggplot(data = aa, aes(total.P_conc_mg.kg,N_conc_percent,  fill = site) ) +
  geom_smooth(aes(group = 1), method = "lm", colour="black", se = TRUE, size=0.5, alpha=0.3) +
  # geom_smooth(data = site, aes(total.P_conc_mg.kg,N_conc_percent),method = "lm", colour="blue", se = TRUE, fill = "blue", alpha=0.15) +
  geom_point(shape = 21, size = 3, color = "black", stroke = .5 ) +
  geom_point(data = site, aes(total.P_conc_mg.kg,N_conc_percent,  fill = site),shape = 21, size=7, color = "black", stroke = .5, alpha = 0.7 ) +
  ggtitle("Bonanza Creek (2)") + ylab ("Soil N (%)") +
  xlab(bquote(Total~P~(mg~kg^-1))) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank() ) 

BonanzaCreek_2

summary(lm(aa$N_conc_percent ~ aa$total.P_conc_mg.kg)) # 0.6615
summary(lm(site$N_conc_percent ~ site$total.P_conc_mg.kg)) # 0.6394 

ggsave(plot=BonanzaCreek_2, filename="figures/totalP_site/BNZ_2.png", width = 3, height = 3)

rm(aa,site)

#### Bonanza Creek_3 ##### 
aa<-subset(cores,dataset=="Bonanza Creek_3")
site<-aggregate(cbind(N_conc_percent,total.P_conc_mg.kg)~site,mean, data=aa,na.rm=T)

BonanzaCreek_3 <-ggplot(data = aa, aes(total.P_conc_mg.kg,N_conc_percent,  fill = site) ) +
  geom_smooth(aes(group = 1), method = "lm", colour="black", se = TRUE, size=0.5, alpha=0.3) +
  # geom_smooth(data = site, aes(total.P_conc_mg.kg,N_conc_percent),method = "lm", colour="blue", se = TRUE, fill = "blue", alpha=0.15) +
  geom_point(shape = 21, size = 3, color = "black", stroke = .5 ) +
  geom_point(data = site, aes(total.P_conc_mg.kg,N_conc_percent,  fill = site),shape = 21, size=7, color = "black", stroke = .5, alpha = 0.7 ) +
  ggtitle("Bonanza Creek (3)") + ylab ("Soil N (%)") +
  xlab(bquote(Total~P~(mg~kg^-1))) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank() ) 

BonanzaCreek_3

summary(lm(aa$N_conc_percent ~ aa$total.P_conc_mg.kg)) #  0.4621 
summary(lm(site$N_conc_percent ~ site$total.P_conc_mg.kg)) # 0.821

ggsave(plot=BonanzaCreek_3, filename="figures/totalP_site/BNZ_3.png", width = 3, height = 3)

rm(aa,site)

#### Brazil_AtlanticForest ##### 
aa<-subset(cores,dataset=="Brazil_AtlanticForest")
site<-aggregate(cbind(N_conc_percent,total.P_conc_mg.kg)~site,mean, data=aa,na.rm=T)

Brazil_AtlanticForest <-ggplot(data = aa, aes(total.P_conc_mg.kg,N_conc_percent,  fill = site) ) +
  geom_smooth(aes(group = 1), method = "lm", colour="black", se = TRUE, size=0.5, alpha=0.3) +
  # geom_smooth(data = site, aes(total.P_conc_mg.kg,N_conc_percent),method = "lm", colour="blue", se = TRUE, fill = "blue", alpha=0.15) +
  geom_point(shape = 21, size = 3, color = "black", stroke = .5 ) +
  geom_point(data = site, aes(total.P_conc_mg.kg,N_conc_percent,  fill = site),shape = 21, size=7, color = "black", stroke = .5, alpha = 0.7 ) +
  ggtitle("Brazil (AtlanticForest)") + ylab ("Soil N (%)") +
  xlab(bquote(Total~P~(mg~kg^-1))) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank() ) 

Brazil_AtlanticForest

summary(lm(aa$N_conc_percent ~ aa$total.P_conc_mg.kg)) # 0.2224 
summary(lm(site$N_conc_percent ~ site$total.P_conc_mg.kg))

ggsave(plot=Brazil_AtlanticForest, filename="figures/totalP_site/BrazilAF.png", width = 3, height = 3)

rm(aa,site)

#### Brazil_SouthernAmazon ##### 
aa<-subset(cores,dataset=="Brazil_SouthernAmazon")
site<-aggregate(cbind(N_conc_percent,total.P_conc_mg.kg)~site,mean, data=aa,na.rm=T)

Brazil_SouthernAmazon <-ggplot(data = aa, aes(total.P_conc_mg.kg,N_conc_percent,  fill = site) ) +
  # geom_smooth(aes(group = 1), method = "lm", colour="black", se = TRUE, size=0.5, alpha=0.3) +
  # geom_smooth(data = site, aes(total.P_conc_mg.kg,N_conc_percent),method = "lm", colour="blue", se = TRUE, fill = "blue", alpha=0.15) +
  geom_point(shape = 21, size = 3, color = "black", stroke = .5 ) +
  geom_point(data = site, aes(total.P_conc_mg.kg,N_conc_percent,  fill = site),shape = 21, size=7, color = "black", stroke = .5, alpha = 0.7 ) +
  ggtitle("Brazil (SouthernAmazon)") + ylab ("Soil N (%)") +
  xlab(bquote(Total~P~(mg~kg^-1))) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank() ) 

Brazil_SouthernAmazon

summary(lm(aa$N_conc_percent ~ aa$total.P_conc_mg.kg))
summary(lm(site$N_conc_percent ~ site$total.P_conc_mg.kg))


ggsave(plot=Brazil_SouthernAmazon, filename="figures/totalP_site/BrazilSA.png", width = 3, height = 3)

rm(aa,site)

#### Calhoun ##### 
aa<-subset(cores,dataset=="Calhoun")
site<-aggregate(cbind(N_conc_percent,total.P_conc_mg.kg)~site,mean, data=aa,na.rm=T)

Calhoun <-ggplot(data = aa, aes(total.P_conc_mg.kg,N_conc_percent,  fill = site, color = site) ) +
  geom_smooth(aes(group = 1), method = "lm", colour="black", se = TRUE, size=0.5, alpha=0.3, linetype = "dashed") +
  # geom_smooth(data = site, aes(total.P_conc_mg.kg,N_conc_percent),method = "lm", colour="blue", se = TRUE, fill = "blue", alpha=0.15) +
  geom_point(shape = 21, size = 3, stroke = .5, color = "black" ) +
  geom_point(data = site, aes(total.P_conc_mg.kg,N_conc_percent,  fill = site, color = site), shape = 21, size=7,  stroke = .5, alpha = 0.7, color = "black" ) +
  ggtitle("Calhoun") + ylab ("Soil N (%)") +
  xlab(bquote(Total~P~(mg~kg^-1))) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank() ) 

Calhoun

summary(lm(aa$N_conc_percent ~ aa$total.P_conc_mg.kg))
summary(lm(site$N_conc_percent ~ site$total.P_conc_mg.kg))

ggsave(plot=Calhoun, filename="figures/totalP_site/Calhoun.png", width = 3, height = 3)

rm(aa,site)

#### CedarCreek ##### 
aa<-subset(cores,dataset=="CedarCreek")
plot<-aggregate(cbind(N_conc_percent,total.P_conc_mg.kg)~plot,mean, data=aa,na.rm=T)

CedarCreek <-ggplot(data = aa, aes(total.P_conc_mg.kg,N_conc_percent,  fill = plot) ) +
  geom_smooth(aes(group = 1), method = "lm", colour="black", se = FALSE, size=0.5, alpha=0.3, linetype = "dashed") +
  # geom_smooth(data = plot, aes(total.P_conc_mg.kg,N_conc_percent),method = "lm", colour="blue", se = TRUE, fill = "blue", alpha=0.15) +
  geom_point(shape = 21, size = 3, color = "black", stroke = .5 ) +
  # geom_point(data = plot, aes(total.P_conc_mg.kg,N_conc_percent,  fill = plot),shape = 21, size=7, color = "black", stroke = .5, alpha = 0.7 ) +
  ggtitle("CedarCreek") + ylab ("Soil N (%)") +
  xlab(bquote(Total~P~(mg~kg^-1))) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank() ) 

CedarCreek

summary(lm(aa$N_conc_percent ~ aa$total.P_conc_mg.kg))
summary(lm(plot$N_conc_percent ~ plot$total.P_conc_mg.kg))

ggsave(plot=CedarCreek, filename="figures/totalP_site/CedarCreek.png", width = 3, height = 3)

rm(aa,site,plot)

#### Coweeta ##### 
aa<-subset(cores,dataset=="Coweeta")
site<-aggregate(cbind(N_conc_percent,total.P_conc_mg.kg)~site,mean, data=aa,na.rm=T)

Coweeta <-ggplot(data = aa, aes(total.P_conc_mg.kg,N_conc_percent,  fill = site) ) +
  geom_smooth(aes(group = 1), method = "lm", colour="black", se = TRUE, size=0.5, alpha=0.3) +
  geom_smooth(data = site, aes(total.P_conc_mg.kg,N_conc_percent),method = "lm", colour="blue", se = TRUE, fill = "blue", alpha=0.15) +
  geom_point(shape = 21, size = 3, color = "black", stroke = .5 ) +
  geom_point(data = site, aes(total.P_conc_mg.kg,N_conc_percent,  fill = site),shape = 21, size=7, color = "black", stroke = .5, alpha = 0.7 ) +
  ggtitle("Coweeta") + ylab ("Soil N (%)") +
  xlab(bquote(Total~P~(mg~kg^-1))) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank() ) 

Coweeta

summary(lm(aa$N_conc_percent ~ aa$total.P_conc_mg.kg))
summary(lm(site$N_conc_percent ~ site$total.P_conc_mg.kg))

ggsave(plot=Coweeta, filename="figures/totalP_site/Coweeta.png", width = 3, height = 3)

rm(aa,site,plot)

#### Hubbard Brook ##### 
aa<-subset(cores,dataset=="Hubbard Brook")
site<-aggregate(cbind(N_conc_percent,total.P_conc_mg.kg)~site,mean, data=aa,na.rm=T)

Hubbard_Brook <-ggplot(data = aa, aes(total.P_conc_mg.kg,N_conc_percent,  fill = site) ) +
  geom_smooth(aes(group = 1), method = "lm", colour="black", se = TRUE, size=0.5, alpha=0.3) +
  geom_smooth(data = site, aes(total.P_conc_mg.kg,N_conc_percent),method = "lm", colour="blue", se = TRUE, fill = "blue", alpha=0.15) +
  geom_point(shape = 21, size = 3, color = "black", stroke = .5 ) +
  geom_point(data = site, aes(total.P_conc_mg.kg,N_conc_percent,  fill = site),shape = 21, size=7, color = "black", stroke = .5, alpha = 0.7 ) +
  ggtitle("Hubbard Brook") + ylab ("Soil N (%)") +
  xlab(bquote(Total~P~(mg~kg^-1))) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank() ) 

Hubbard_Brook

summary(lm(aa$N_conc_percent ~ aa$total.P_conc_mg.kg))
summary(lm(site$N_conc_percent ~ site$total.P_conc_mg.kg))

ggsave(plot=Hubbard_Brook, filename="figures/totalP_site/Hubbard_Brook.png", width = 3, height = 3)

rm(aa,site,plot)

#### Jornada_2 ##### 
aa<-subset(cores,dataset=="Jornada_2")
site<-aggregate(cbind(N_conc_percent,total.P_conc_mg.kg)~site,mean, data=aa,na.rm=T)

Jornada_2 <-ggplot(data = aa, aes(total.P_conc_mg.kg,N_conc_percent,  fill = site) ) +
  geom_smooth(aes(group = 1), method = "lm", colour="black", se = TRUE, size=0.5, alpha=0.3) +
  # geom_smooth(data = site, aes(total.P_conc_mg.kg,N_conc_percent),method = "lm", colour="blue", se = TRUE, fill = "blue", alpha=0.15) +
  geom_point(shape = 21, size = 3, color = "black", stroke = .5 ) +
  geom_point(data = site, aes(total.P_conc_mg.kg, N_conc_percent,  fill = site),shape = 21, size=7, color = "black", stroke = .5, alpha = 0.7 ) +
  ggtitle("Jornada (2)") + ylab ("Soil N (%)") +
  xlab(bquote(Total~P~(mg~kg^-1))) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank() ) 

Jornada_2

summary(lm(aa$N_conc_percent ~ aa$total.P_conc_mg.kg))
summary(lm(site$N_conc_percent ~ site$total.P_conc_mg.kg))

ggsave(plot=Jornada_2, filename="figures/totalP_site/Jornada_2.png", width = 3, height = 3)

rm(aa,site,plot)

#### Jornada_1 ##### 
aa<-subset(cores,dataset=="Jornada_1")
site<-aggregate(cbind(N_conc_percent,total.P_conc_mg.kg)~site,mean, data=aa,na.rm=T)

Jornada_1 <-ggplot(data = aa, aes(total.P_conc_mg.kg,N_conc_percent,  fill = site) ) +
  geom_smooth(aes(group = 1), method = "lm", colour="black", se = TRUE, size=0.5, alpha=0.3) +
  # geom_smooth(data = site, aes(total.P_conc_mg.kg,N_conc_percent),method = "lm", colour="blue", se = TRUE, fill = "blue", alpha=0.15) +
  geom_point(shape = 21, size = 3, color = "black", stroke = .5 ) +
  geom_point(data = site, aes(total.P_conc_mg.kg, N_conc_percent,  fill = site),shape = 21, size=7, color = "black", stroke = .5, alpha = 0.7 ) +
  ggtitle("Jornada (1)") + ylab ("Soil N (%)") +
  xlab(bquote(Total~P~(mg~kg^-1))) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank() ) 

Jornada_1

summary(lm(aa$N_conc_percent ~ aa$total.P_conc_mg.kg))
summary(lm(site$N_conc_percent ~ site$total.P_conc_mg.kg))

ggsave(plot=Jornada_1, filename="figures/totalP_site/Jornada_1.png", width = 3, height = 3)

rm(aa,site,plot)

#### Luquillo_2 ##### 
aa<-subset(cores,dataset=="Luquillo_2")
site<-aggregate(cbind(N_conc_percent,total.P_conc_mg.kg)~site,mean, data=aa,na.rm=T)

Luquillo_2 <-ggplot(data = aa, aes(total.P_conc_mg.kg,N_conc_percent,  fill = site) ) +
  # geom_smooth(aes(group = 1), method = "lm", colour="black", se = TRUE, size=0.5, alpha=0.3) +
  # geom_smooth(data = site, aes(total.P_conc_mg.kg,N_conc_percent),method = "lm", colour="blue", se = TRUE, fill = "blue", alpha=0.15) +
  geom_point(shape = 21, size = 3, color = "black", stroke = .5 ) +
  geom_point(data = site, aes(total.P_conc_mg.kg, N_conc_percent,  fill = site),shape = 21, size=7, color = "black", stroke = .5, alpha = 0.7 ) +
  ggtitle("Luquillo (2)") + ylab ("Soil N (%)") +
  xlab(bquote(Total~P~(mg~kg^-1))) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank() ) 

Luquillo_2

summary(lm(aa$N_conc_percent ~ aa$total.P_conc_mg.kg))
summary(lm(site$N_conc_percent ~ site$total.P_conc_mg.kg))

ggsave(plot=Luquillo_2, filename="figures/totalP_site/Luquillo_2.png", width = 3, height = 3)

rm(aa,site,plot)

#### Luquillo_1 ##### # NOT WORKING RN
# Not including because there is not total P for this site 

rm(aa,site,plot)

#### Luquillo_3 ##### 
aa<-subset(cores,dataset=="Luquillo_3")
site<-aggregate(cbind(N_conc_percent,total.P_conc_mg.kg)~site,mean, data=aa,na.rm=T)

Luquillo_3 <-ggplot(data = aa, aes(total.P_conc_mg.kg,N_conc_percent,  fill = site) ) +
  geom_smooth(aes(group = 1), method = "lm", colour="black", se = TRUE, size=0.5, alpha=0.3, linetype = "dashed") +
  # geom_smooth(data = site, aes(total.P_conc_mg.kg,N_conc_percent),method = "lm", colour="blue", se = TRUE, fill = "blue", alpha=0.15) +
  geom_point(shape = 21, size = 3, color = "black", stroke = .5 ) +
  geom_point(data = site, aes(total.P_conc_mg.kg, N_conc_percent,  fill = site),shape = 21, size=7, color = "black", stroke = .5, alpha = 0.7 ) +
  ggtitle("Luquillo (3)") + ylab ("Soil N (%)") +
  xlab(bquote(Total~P~(mg~kg^-1))) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank() ) 

Luquillo_3

summary(lm(aa$N_conc_percent ~ aa$total.P_conc_mg.kg))
summary(lm(site$N_conc_percent ~ site$total.P_conc_mg.kg))

ggsave(plot=Luquillo_3, filename="figures/totalP_site/Luquillo_3.png", width = 3, height = 3)

rm(aa,site,plot)

#### Niwot_1 ##### 
aa<-subset(cores,dataset=="Niwot_1")
site<-aggregate(cbind(N_conc_percent,total.P_conc_mg.kg)~site,mean, data=aa,na.rm=T)

Niwot_1 <-ggplot(data = aa, aes(total.P_conc_mg.kg,N_conc_percent,  fill = site) ) +
  geom_smooth(aes(group = 1), method = "lm", colour="black", se = TRUE, size=0.5, alpha=0.3) +
  # geom_smooth(data = site, aes(total.P_conc_mg.kg,N_conc_percent),method = "lm", colour="blue", se = TRUE, fill = "blue", alpha=0.15) +
  geom_point(shape = 21, size = 3, color = "black", stroke = .5 ) +
  geom_point(data = site, aes(total.P_conc_mg.kg, N_conc_percent,  fill = site),shape = 21, size=7, color = "black", stroke = .5, alpha = 0.7 ) +
  ggtitle("Niwot (1)") + ylab ("Soil N (%)") +
  xlab(bquote(Total~P~(mg~kg^-1))) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank() ) 

Niwot_1

summary(lm(aa$N_conc_percent ~ aa$total.P_conc_mg.kg))
summary(lm(site$N_conc_percent ~ site$total.P_conc_mg.kg))

ggsave(plot=Niwot_1, filename="figures/totalP_site/Niwot_1.png", width = 3, height = 3)


#### Niwot_2 ##### 
# no N data

rm(aa,site,plot)

#### Niwot_3 ##### 
aa<-subset(cores,dataset=="Niwot_3")
site<-aggregate(cbind(N_conc_percent,total.P_conc_mg.kg)~site,mean, data=aa,na.rm=T)

Niwot_3 <-ggplot(data = aa, aes(total.P_conc_mg.kg,N_conc_percent,  fill = site) ) +
  geom_smooth(aes(group = 1), method = "lm", colour="black", se = TRUE, size=0.5, alpha=0.3, fill = "grey") +
  # geom_smooth(data = site, aes(total.P_conc_mg.kg,N_conc_percent),method = "lm", colour="blue", se = TRUE, fill = "blue", alpha=0.15) +
  geom_point(shape = 21, size = 3, color = "black", stroke = .5 ) +
  geom_point(data = site, aes(total.P_conc_mg.kg, N_conc_percent,  fill = site), shape = 21, size=7, color = "black", stroke = .5, alpha = 0.7 ) +
  ggtitle("Niwot (3)") + ylab ("Soil N (%)") +
  xlab(bquote(Total~P~(mg~kg^-1))) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank() ) 

Niwot_3

summary(lm(aa$N_conc_percent ~ aa$total.P_conc_mg.kg))
# summary(lm(site$N_conc_percent ~ site$total.P_conc_mg.kg))

ggsave(plot=Niwot_3, filename="figures/totalP_site/Niwot_3.png", width = 3, height = 3)

rm(aa,site,plot)

#### Niwot_4 ##### 
aa<-subset(cores,dataset=="Niwot_4")
site<-aggregate(cbind(N_conc_percent,total.P_conc_mg.kg)~site,mean, data=aa,na.rm=T)

Niwot_4 <-ggplot(data = aa, aes(total.P_conc_mg.kg,N_conc_percent,  fill = site) ) +
  geom_smooth(aes(group = 1), method = "lm", colour="black", se = TRUE, size=0.5, alpha=0.3) +
  geom_smooth(data = site, aes(total.P_conc_mg.kg,N_conc_percent),method = "lm", colour="blue", se = TRUE, fill = "blue", alpha=0.15) +
  geom_point(shape = 21, size = 3, color = "black", stroke = .5 ) +
  geom_point(data = site, aes(total.P_conc_mg.kg, N_conc_percent,  fill = site),shape = 21, size=7, color = "black", stroke = .5, alpha = 0.7 ) +
  ggtitle("Niwot (4)") + ylab ("Soil N (%)") +
  xlab(bquote(Total~P~(mg~kg^-1))) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank() ) 

Niwot_4

summary(lm(aa$N_conc_percent ~ aa$total.P_conc_mg.kg))
summary(lm(site$N_conc_percent ~ site$total.P_conc_mg.kg))

ggsave(plot=Niwot_4, filename="figures/totalP_site/Niwot_4.png", width = 3, height = 3)

rm(aa,site,plot)

#### Niwot_5 ##### 
aa<-subset(cores,dataset=="Niwot_5")
site<-aggregate(cbind(N_conc_percent,total.P_conc_mg.kg)~site,mean, data=aa,na.rm=T)

Niwot_5 <-ggplot(data = aa, aes(total.P_conc_mg.kg,N_conc_percent,  fill = site) ) +
  # geom_smooth(aes(group = 1), method = "lm", colour="black", se = TRUE, size=0.5, alpha=0.3) +
  # geom_smooth(data = site, aes(total.P_conc_mg.kg,N_conc_percent),method = "lm", colour="blue", se = TRUE, fill = "blue", alpha=0.15) +
  geom_point(shape = 21, size = 3, color = "black", stroke = .5 ) +
  geom_point(data = site, aes(total.P_conc_mg.kg, N_conc_percent,  fill = site),shape = 21, size=7, color = "black", stroke = .5, alpha = 0.7 ) +
  ggtitle("Niwot (5)") + ylab ("Soil N (%)") +
  xlab(bquote(Total~P~(mg~kg^-1))) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank() ) 

Niwot_5

summary(lm(aa$N_conc_percent ~ aa$total.P_conc_mg.kg))
# summary(lm(site$N_conc_percent ~ site$total.P_conc_mg.kg))

ggsave(plot=Niwot_5, filename="figures/totalP_site/Niwot_5.png", width = 3, height = 3)

rm(aa,site,plot)

#### Sevilleta_1 ##### 
aa<-subset(cores,dataset=="Sevilleta_1")
site<-aggregate(cbind(N_conc_percent,total.P_conc_mg.kg)~site,mean, data=aa,na.rm=T)

Sevilleta_1 <-ggplot(data = aa, aes(total.P_conc_mg.kg,N_conc_percent,  fill = site) ) +
  geom_smooth(aes(group = 1), method = "lm", colour="black", se = TRUE, size=0.5, alpha=0.3) +
  # geom_smooth(data = site, aes(total.P_conc_mg.kg,N_conc_percent),method = "lm", colour="blue", se = TRUE, fill = "blue", alpha=0.15) +
  geom_point(shape = 21, size = 3, color = "black", stroke = .5 ) +
  geom_point(data = site, aes(total.P_conc_mg.kg, N_conc_percent,  fill = site),shape = 21, size=7, color = "black", stroke = .5, alpha = 0.7 ) +
  ggtitle("Sevilleta (1)") + ylab ("Soil N (%)") +
  xlab(bquote(Total~P~(mg~kg^-1))) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank() ) 

Sevilleta_1

summary(lm(aa$N_conc_percent ~ aa$total.P_conc_mg.kg))
# summary(lm(site$N_conc_percent ~ site$total.P_conc_mg.kg))

ggsave(plot=Sevilleta_1, filename="figures/totalP_site/Sevilleta_1.png", width = 3, height = 3)

rm(aa,site,plot)

#### Sevilleta_2 ##### 
aa<-subset(cores,dataset=="Sevilleta_2")
site<-aggregate(cbind(N_conc_percent,total.P_conc_mg.kg)~site,mean, data=aa,na.rm=T)

Sevilleta_2 <-ggplot(data = aa, aes(total.P_conc_mg.kg,N_conc_percent,  fill = site) ) +
  geom_smooth(aes(group = 1), method = "lm", colour="black", se = TRUE, size=0.5, alpha=0.3, fill = "gray") +
  # geom_smooth(data = site, aes(total.P_conc_mg.kg,N_conc_percent),method = "lm", colour="blue", se = TRUE, fill = "blue", alpha=0.15) +
  geom_point(shape = 21, size = 3, color = "black", stroke = .5 ) +
  geom_point(data = site, aes(total.P_conc_mg.kg, N_conc_percent,  fill = site),shape = 21, size=7, color = "black", stroke = .5, alpha = 0.7 ) +
  ggtitle("Sevilleta (2)") + ylab ("Soil N (%)") +
  xlab(bquote(Total~P~(mg~kg^-1))) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank() ) 

Sevilleta_2

summary(lm(aa$N_conc_percent ~ aa$total.P_conc_mg.kg))
# summary(lm(site$N_conc_percent ~ site$total.P_conc_mg.kg))

ggsave(plot=Sevilleta_2, filename="figures/totalP_site/Sevilleta_2.png", width = 3, height = 3)

#### Tapajos ##### 
aa<-subset(cores,dataset=="Tapajos")
site<-aggregate(cbind(N_conc_percent,total.P_conc_mg.kg)~site,mean, data=aa,na.rm=T)

Tapajos <-ggplot(data = aa, aes(total.P_conc_mg.kg,N_conc_percent,  fill = site) ) +
  geom_smooth(aes(group = 1), method = "lm", colour="black", se = TRUE, size=0.5, alpha=0.3, linetype = "dashed") +
  # geom_smooth(data = site, aes(total.P_conc_mg.kg,N_conc_percent),method = "lm", colour="blue", se = TRUE, fill = "blue", alpha=0.15) +
  geom_point(shape = 21, size = 3, color = "black", stroke = .5 ) +
  geom_point(data = site, aes(total.P_conc_mg.kg, N_conc_percent,  fill = site),shape = 21, size=7, color = "black", stroke = .5, alpha = 0.7 ) +
  ggtitle("Tapajos") + ylab ("Soil N (%)") +
  xlab(bquote(Total~P~(mg~kg^-1))) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank() ) 

Tapajos

summary(lm(aa$N_conc_percent ~ aa$total.P_conc_mg.kg))
summary(lm(site$N_conc_percent ~ site$total.P_conc_mg.kg))

ggsave(plot=Tapajos, filename="figures/totalP_site/Tapajos.png", width = 3, height = 3)


## ------------------------------------------ ##
# COMBINED Within site Total P versus Total N figures  -----
## ------------------------------------------ ##

WithinSiteTotal <- cowplot::plot_grid(BonanzaCreek_1,BonanzaCreek_2,BonanzaCreek_3,Brazil_AtlanticForest,Brazil_SouthernAmazon,Calhoun,CedarCreek,Coweeta,Hubbard_Brook,Jornada_1,Jornada_2,Luquillo_2,Luquillo_3,Niwot_1,Niwot_3,Niwot_4,Niwot_5,Sevilleta_1,Sevilleta_2,Tapajos)

WithinSiteTotal 

ggsave(plot=WithinSiteTotal, filename="figures/totalP_site/WithinSiteTotal.png", width = 13, height = 13)



#### Toolik_1 ##### NOT WORKING 
aa<-subset(cores,dataset=="Toolik_1")
plot<-aggregate(cbind(N_conc_percent,total.P_conc_mg.kg)~plot,mean, data=aa,na.rm=T)

Toolik_1 <-ggplot(data = aa, aes(total.P_conc_mg.kg,N_conc_percent,  fill = plot) ) +
  geom_smooth(aes(group = 1), method = "lm", colour="black", se = TRUE, size=0.5, alpha=0.3) +
  geom_smooth(data = plot, aes(total.P_conc_mg.kg,N_conc_percent),method = "lm", colour="blue", se = TRUE, fill = "blue", alpha=0.15) +
  geom_point(shape = 21, size = 2, color = "black", stroke = .5 ) +
  geom_point(data = plot, aes(total.P_conc_mg.kg, N_conc_percent,  fill = plot),shape = 21, size=4, color = "black", stroke = .5 ) +
  ggtitle("Toolik (1)") + ylab ("Soil N (%)") +
  xlab(bquote(Total~P~(mg~kg^-1))) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank() ) 

Toolik_1

summary(lm(aa$N_conc_percent ~ aa$total.P_conc_mg.kg))
summary(lm(plot$N_conc_percent ~ plot$total.P_conc_mg.kg))


#### Toolik_2 ##### NOT WORKING 
aa<-subset(cores,dataset=="Toolik_2")
plot<-aggregate(cbind(N_conc_percent,total.P_conc_mg.kg)~plot,mean, data=aa,na.rm=T)

Toolik_2 <-ggplot(data = aa, aes(total.P_conc_mg.kg,N_conc_percent,  fill = plot) ) +
  geom_smooth(aes(group = 1), method = "lm", colour="black", se = TRUE, size=0.5, alpha=0.3) +
  geom_smooth(data = plot, aes(total.P_conc_mg.kg,N_conc_percent),method = "lm", colour="blue", se = TRUE, fill = "blue", alpha=0.15) +
  geom_point(shape = 21, size = 2, color = "black", stroke = .5 ) +
  geom_point(data = plot, aes(total.P_conc_mg.kg, N_conc_percent,  fill = plot),shape = 21, size=4, color = "black", stroke = .5 ) +
  ggtitle("Toolik (2)") + ylab ("Soil N (%)") +
  xlab(bquote(Total~P~(mg~kg^-1))) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank() ) 

Toolik_2

summary(lm(aa$N_conc_percent ~ aa$total.P_conc_mg.kg))
summary(lm(plot$N_conc_percent ~ plot$total.P_conc_mg.kg))

#### Making cowplot grid figure ##### 

totalP_N_combined <- cowplot::plot_grid(BonanzaCreek_1,BonanzaCreek_2,BonanzaCreek_3,Brazil_AtlanticForest,Brazil_SouthernAmazon,Calhoun,CedarCreek,Coweeta,Hubbard_Brook,Jornada_2,Jornada_1,Luquillo_1,Luquillo_2,Luquillo_3,Niwot_1,Niwot_2,Niwot_3,Niwot_4,Niwot_5,Sevilleta_1,Sevilleta_2,Tapajos,Toolik_1,Toolik_2)
