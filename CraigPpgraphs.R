

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
cores$dataset <- ifelse(cores$dataset == "Konza_1", cores$site, cores$dataset )

cores <- cores %>% 
  mutate(PropSlowTotal = slow.P_conc_mg.kg/total.P_conc_mg.kg)

  
# Adding code to subset to only observations where both slow P and total N values exist for Niwot 1 
cores$slow.P_conc_mg.kg <- ifelse(is.na(cores$N_conc_percent) == TRUE, NA, cores$slow.P_conc_mg.kg)
cores$N_conc_percent <- ifelse(is.na(cores$slow.P_conc_mg.kg) == TRUE, NA, cores$N_conc_percent)

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
  group_by(dataset,site,block,plot) %>% 
  summarise(N_conc_percent = mean(N_conc_percent),
            total.P_conc_mg.kg = mean(total.P_conc_mg.kg),
            PropSlowTotal = mean(PropSlowTotal) )

## SLOW P
# plot_slowP<-aggregate(cbind(N_conc_percent,slow.P_conc_mg.kg,PropSlowTotal)~dataset+site+block+plot, mean, data=cores, na.rm = F)

plot_slowP <- cores %>% 
  group_by(dataset,site,block,plot) %>% 
  summarise(N_conc_percent = mean(N_conc_percent),
            slow.P_conc_mg.kg = mean(slow.P_conc_mg.kg),
            PropSlowTotal = mean(PropSlowTotal) )

# SITE LEVEL DATASETS

## TOTAL P 
site_totalP<-aggregate(cbind(N_conc_percent,total.P_conc_mg.kg,PropSlowTotal)~dataset+site,mean, data=plot_totalP,na.rm=T) # changing data to plot slow P - this means the site means is the mean of the plot means 

site_totalP <- plot_totalP %>% 
  group_by(dataset,site) %>% 
  summarise(N_conc_percent = mean(N_conc_percent),
            total.P_conc_mg.kg = mean(total.P_conc_mg.kg),
            PropSlowTotal = mean(PropSlowTotal) )

## SLOW P
site_slowP<-aggregate(cbind(N_conc_percent,slow.P_conc_mg.kg,PropSlowTotal)~dataset+site,mean, data=plot_slowP,na.rm=T)

site_slowP <- plot_slowP %>% 
  group_by(dataset,site) %>% 
  summarise(N_conc_percent = mean(N_conc_percent),
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
  filter(dataset %in% c("Calhoun","Coweeta","Hubbard Brook","Jornada_2","Konza_1","Luquillo_2","Niwot_1","Niwot_5","Sevilleta_1","Tapajos","Smokey Valley","Hays","Arikaree","Konza_2","CedarCreek_1"))

# MANUALLY CHANGING SEV 1 TOTAL N MEAN FOR NOW, NEED TO DISCUSS WITH ANNE FINAL SOLUTION
# Sev total N mean of grasslands and shrub sites from Anne's thesis = 0.055
dataset_means_slowP <- dataset_means_slowP %>% 
  mutate(mean_N = ifelse(dataset == "Sevilleta_1",0.055,mean_N))

# Grouping site Slow P dataset by SITE and adding columns for mean, standard deviation and standard error for N and P 
site_means_slowP <- site_slowP %>% 
  select(dataset,site,N_conc_percent,slow.P_conc_mg.kg) %>% 
  group_by(dataset,site) %>% 
  dplyr::summarise(mean_N = mean(N_conc_percent, na.rm = TRUE),mean_P = mean(slow.P_conc_mg.kg, na.rm = TRUE),sd_N = sd(N_conc_percent, na.rm = TRUE),sd_P = sd(slow.P_conc_mg.kg, na.rm = TRUE),se_N = plotrix::std.error(N_conc_percent, na.rm = TRUE),se_P = plotrix::std.error(slow.P_conc_mg.kg, na.rm = TRUE))

site_means_slowP <- site_means_slowP %>% 
  filter(dataset %in% c("Calhoun","Coweeta","Hubbard Brook","Jornada_2","Konza_1","Luquillo_2","Luquillo_2","Niwot_5","Sevilleta_1","Tapajos"))

site_means_slowP <- site_means_slowP %>% 
  mutate(mean_N = ifelse(dataset == "Sevilleta_1",0.055,mean_N))

## TOTAL P SUMMARIZED AND SITE SELECTED DATASETS 
dataset_means_totalP <- site_totalP %>% 
  dplyr::select(dataset, N_conc_percent, total.P_conc_mg.kg, PropSlowTotal) %>% 
  group_by(dataset) %>% 
  dplyr::summarise(mean_N = mean(N_conc_percent, na.rm = TRUE),
                   mean_P = mean(total.P_conc_mg.kg, na.rm = TRUE),
                   mean_ratio = mean(PropSlowTotal, na.rm = TRUE),
                   sd_N = sd(N_conc_percent, na.rm = TRUE),
                   sd_P = sd(total.P_conc_mg.kg, na.rm = TRUE),
                   sd_ratio = sd(PropSlowTotal, na.rm = TRUE),
                   se_N = plotrix::std.error(N_conc_percent, na.rm = TRUE),
                   se_P = plotrix::std.error(total.P_conc_mg.kg, na.rm = TRUE))


# selecting all sites with full total P info 
dataset_means_totalP <- dataset_means_totalP %>% 
  filter(dataset %in% c("Bonanza Creek_1","Bonanza Creek_2","Bonanza Creek_3","Brazil","Calhoun","CedarCreek_1","CedarCreek_2","Coweeta","Hubbard Brook","Jornada_1","Jornada_2","Konza_1","Konza_2","Luquillo_2","Luquillo_3","Niwot_1","Niwot_3","Niwot_4","Niwot_5","Sevilleta_1" ,"Sevilleta_2","Tapajos","Toolik_1","Toolik_2") )

# MANUALLY CHANGING SEV 1 TOTAL N MEAN FOR NOW, NEED TO DISCUSS WITH ANNE FINAL SOLUTION
# Sev total N mean of grasslands and shrub sites from Anne's thesis = 0.055
dataset_means_totalP <- dataset_means_totalP %>% 
  mutate(mean_N = ifelse(dataset == "Sevilleta_1",0.055,mean_N))

site_means_totalP <- site_totalP %>% 
  select(dataset,site,N_conc_percent,total.P_conc_mg.kg) %>% 
  group_by(dataset,site) %>% 
  dplyr::summarise(mean_N = mean(N_conc_percent, na.rm = TRUE),mean_P = mean(total.P_conc_mg.kg, na.rm = TRUE),sd_N = sd(N_conc_percent, na.rm = TRUE),sd_P = sd(total.P_conc_mg.kg, na.rm = TRUE),se_N = plotrix::std.error(N_conc_percent, na.rm = TRUE),se_P = plotrix::std.error(total.P_conc_mg.kg, na.rm = TRUE))

# selecting all sites with full total P info 
site_means_totalP <- site_means_totalP %>% 
  filter(dataset %in% c("Bonanza Creek_1","Bonanza Creek_2","Bonanza Creek_3","Brazil","Calhoun","CedarCreek_1","CedarCreek_2","Coweeta","Hubbard Brook","Jornada_1","Jornada_2","Konza_1","Konza_2","Luquillo_2","Luquillo_3","Niwot_1","Niwot_3","Niwot_4","Niwot_5","Sevilleta_1" ,"Sevilleta_2","Tapajos","Toolik_1","Toolik_2","FloridaCoastal") )

## ------------------------------------------ ##
# SIMPLE LINEAR REGRESSIONS AND EXPONENTIAL DECAY MODEL FOR CROSS-SITE ANALYSES FOR SLOW AND TOTAL P -----
## ------------------------------------------ ##

### SLOW P ANALYSES

## MAKING FIGURES 
library(MASS) # to access Animals data sets
library(scales) # to access break formatting functions

dataset_means_slowP <- dataset_means_slowP %>% 
  mutate(log_mean_N = log(mean_N) )

# Renaming datasets to remomve underscores 
dataset_means_slowP <- dataset_means_slowP %>% 
  mutate(dataset = recode(dataset, CedarCreek_1 = 'CedarCreek',
                           Jornada_2 = "Jornada (2)",
                           Luquillo_2 = "Luquillo (2)",
                           Niwot_1 = "Niwot (1)",
                           Sevilleta_1 = "Sevilleta (1)")) 

# %>% 
# rename('Ratio of Slow P over Total P' = mean_ratio) 

library(ggrepel)

TotalN_SlowPfig_dataset <- ggplot(data = dataset_means_slowP,
                           aes(x=mean_P, y=mean_N) ) +
  geom_point(aes(color = mean_ratio), size=3) + # removing se size for now 
  labs(title = "Slow P versus Total N",
       y = "Total N (%)") + 
  xlab(bquote(Slow~P~(mg~"*"~kg^-1))) +
  geom_label_repel(data = dataset_means_slowP, 
            aes(label = dataset), nudge_x=0.45, nudge_y=0.025,
            arrow=NULL) +
  stat_smooth(method = 'lm', se = TRUE, color = "black") +
  theme_bw()  +
  scale_color_gradientn(colours = rainbow(5)) +
  labs(color = "Ratio of Slow P over Total P") 

ggsave(plot = TotalN_SlowPfig_dataset, filename = "figures/TotalN_SlowPfig_dataset.png", width = 15, height = 10)


TotalN_TotalPfig_dataset <- ggplot(data = dataset_means_totalP,
                                  aes(x=mean_P, y=mean_N) ) +
  geom_point(aes(color = dataset), size=3) + # removing se size for now 
  labs(title = "Total P versus Total N",
       y = "Total N (%)") + 
  xlab(bquote(Total~P~(mg~"*"~kg^-1))) +
  geom_label_repel(data = dataset_means_totalP, 
                   aes(label = dataset), nudge_x=0.45, nudge_y=0.025,
                   arrow=NULL) +
  stat_smooth(method = 'lm', se = TRUE, color = "black") +
  theme_bw()  +
  scale_color_gradientn(colours = rainbow(5)) +
  labs(color = "Ratio of Slow P over Total P") 

ggsave(plot = TotalN_SlowPfig_dataset, filename = "figures/TotalN_SlowPfig_dataset.png", width = 15, height = 10)




SlowPfig_dataset2 <- ggplot(data = subset(dataset_means_slowP, dataset != "Niwot_5"),
                           aes(x=mean_ratio, y=mean_N) ) +
  geom_point(aes(color = dataset), ) + # removing se size for now 
  labs(title =ge "Slow P versus Total N by Dataset",
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
# Moving previous code down here that we don't need for now 5/12/24 EV  -----
## ------------------------------------------ ##

# SIMPLE LINEAR REGRESSIONS BY SITE OF TOTAL P VS TOTAL N

# This code produces a summary table with the slope and p-value for each dataset,doing a simple linear model
# Write 1-2 sentences in results section just summarizing this table (put table in appendix and reference)
sum_table<-data.frame(matrix(nrow=length(unique(cores$dataset)),ncol=3)) 
names(sum_table) <- c('dataset', 'Total_P.N_slope', 'Total_P.N_.pvalue')
cores_totalP<-cores
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
cores_SlowP<-subset(cores_SlowP,lter!="CDR")#remove CDR because only 1 observation
cores_SlowP<-subset(cores_SlowP,dataset!="Konza_2")#remove Konza_2 because only 1 observation
cores_SlowP<-subset(cores_SlowP,dataset!="Niwot_5")# Slow P same values for all 3 observations
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

#### CALHOUN #####
aa<-subset(cores,dataset=="Calhoun")
site<-aggregate(cbind(N_conc_percent,total.P_conc_mg.kg,slow.P_conc_mg.kg)~site,mean, data=aa,na.rm=T)

f<-ggplot(aa, aes(slow.P_conc_mg.kg,N_conc_percent,  colour = site)) +
  geom_point(size = 2 ) +
  geom_smooth(aes(group = 1),method = "lm", colour="black", se = FALSE) +
  geom_point(data = site, aes(slow.P_conc_mg.kg,N_conc_percent,  colour = site),size=4, alpha=0.5)+
  geom_smooth(data = site, aes(slow.P_conc_mg.kg,N_conc_percent),method = "lm", colour="red", se = FALSE)+
  ggtitle("Calhoun")+ xlab("Slowly Cycling P (mg/kg)") + ylab ("Soil N (%)")+
  theme_bw()
  f

#### COWEETA #####  
aa<-subset(cores,dataset=="Coweeta")
site<-aggregate(cbind(N_conc_percent,total.P_conc_mg.kg,slow.P_conc_mg.kg)~site,mean, data=aa,na.rm=T)

f<-ggplot(aa, aes(slow.P_conc_mg.kg,N_conc_percent,  colour = site)) +
  geom_point(size = 2 ) +
  geom_smooth(aes(group = 1),method = "lm", colour="black", se = FALSE) +
  geom_point(data = site, aes(slow.P_conc_mg.kg,N_conc_percent,  colour = site),size=4, alpha=0.5)+
  geom_smooth(data = site, aes(slow.P_conc_mg.kg,N_conc_percent),method = "lm", colour="red", se = FALSE)+
  ggtitle("Coweeta")+ xlab("Slowly Cycling P (mg/kg)") + ylab ("Soil N (%)")+
  theme_bw()
  f

#### FLO RIDA #####  
aa<-subset(cores,dataset=="FloridaCoastal")
site<-aggregate(cbind(N_conc_percent,total.P_conc_mg.kg,slow.P_conc_mg.kg)~site,mean, data=aa,na.rm=T)

f<-ggplot(aa, aes(slow.P_conc_mg.kg,N_conc_percent,  colour = site)) +
  geom_point(size = 2 ) +
  geom_smooth(aes(group = 1),method = "lm", colour="black", se = FALSE) +
  geom_point(data = site, aes(slow.P_conc_mg.kg,N_conc_percent,  colour = site),size=4, alpha=0.5)+
  geom_smooth(data = site, aes(slow.P_conc_mg.kg,N_conc_percent),method = "lm", colour="red", se = FALSE)+
  ggtitle("Everglades")+ xlab("Slowly Cycling P (mg/kg)") + ylab ("Soil N (%)")+
  theme_bw()
  f

#### HubbardBrook #####  
  aa<-subset(cores,dataset=="Hubbard Brook")
  site<-aggregate(cbind(N_conc_percent,total.P_conc_mg.kg,slow.P_conc_mg.kg)~site,mean, data=aa,na.rm=T)
  
f<-ggplot(aa, aes(slow.P_conc_mg.kg,N_conc_percent,  colour = site)) +
    geom_point(size = 2 ) +
    geom_smooth(aes(group = 1),method = "lm", colour="black", se = FALSE) +
    geom_point(data = site, aes(slow.P_conc_mg.kg,N_conc_percent,  colour = site),size=4, alpha=0.5)+
    geom_smooth(data = site, aes(slow.P_conc_mg.kg,N_conc_percent),method = "lm", colour="red", se = FALSE)+
    ggtitle("Hubbard Brook")+ xlab("Slowly Cycling P (mg/kg)") + ylab ("Soil N (%)")+
    theme_bw()
f  

#### Jornada #####  
aa<-subset(cores,dataset=="Jornada_2")
site<-aggregate(cbind(N_conc_percent,total.P_conc_mg.kg,slow.P_conc_mg.kg)~site,mean, data=aa,na.rm=T)

f<-ggplot(aa, aes(slow.P_conc_mg.kg,N_conc_percent,  colour = site)) +
  geom_point(size = 2 ) +
  geom_smooth(aes(group = 1),method = "lm", colour="black", se = FALSE) +
  geom_point(data = site, aes(slow.P_conc_mg.kg,N_conc_percent,  colour = site),size=4, alpha=0.5)+
  geom_smooth(data = site, aes(slow.P_conc_mg.kg,N_conc_percent),method = "lm", colour="red", se = FALSE)+
  ggtitle("Jornada")+ xlab("Slowly Cycling P (mg/kg)") + ylab ("Soil N (%)")+
  theme_bw()
f  

#### Luquillo ##### 
aa<-subset(cores,dataset=="Luquillo_2")
site<-aggregate(cbind(N_conc_percent,total.P_conc_mg.kg,slow.P_conc_mg.kg)~site,mean, data=aa,na.rm=T)

f<-ggplot(aa, aes(slow.P_conc_mg.kg,N_conc_percent,  colour = site)) +
  geom_point(size = 2 ) +
  geom_smooth(aes(group = 1),method = "lm", colour="black", se = FALSE) +
  geom_point(data = site, aes(slow.P_conc_mg.kg,N_conc_percent,  colour = site),size=4, alpha=0.5)+
  geom_smooth(data = site, aes(slow.P_conc_mg.kg,N_conc_percent),method = "lm", colour="red", se = FALSE)+
  ggtitle("Luquillo")+ xlab("Slowly Cycling P (mg/kg)") + ylab ("Soil N (%)")+
  theme_bw()
f  

#### Niwot ##### 
aa<-subset(cores,dataset=="Niwot_1")
site<-aggregate(cbind(N_conc_percent,total.P_conc_mg.kg,slow.P_conc_mg.kg)~site,mean, data=aa,na.rm=T)

f<-ggplot(aa, aes(slow.P_conc_mg.kg,N_conc_percent,  colour = site)) +
  geom_point(size = 2 ) +
  geom_smooth(aes(group = 1),method = "lm", colour="black", se = FALSE) +
  geom_point(data = site, aes(slow.P_conc_mg.kg,N_conc_percent,  colour = site),size=4, alpha=0.5)+
  geom_smooth(data = site, aes(slow.P_conc_mg.kg,N_conc_percent),method = "lm", colour="red", se = FALSE)+
  ggtitle("Niwot Ridge")+ xlab("Slowly Cycling P (mg/kg)") + ylab ("Soil N (%)")+
  theme_bw()
f  

#### Sevilleta ##### 
aa<-subset(cores,dataset=="Sevilleta_1")
plot<-aggregate(cbind(N_conc_percent,total.P_conc_mg.kg,slow.P_conc_mg.kg)~plot,mean, data=aa,na.rm=T)

f<-ggplot(aa, aes(slow.P_conc_mg.kg,N_conc_percent,  colour = plot)) +
  geom_point(size = 2 ) +
  geom_smooth(aes(group = 1),method = "lm", colour="black", se = FALSE) +
  geom_point(data = plot, aes(slow.P_conc_mg.kg,N_conc_percent,  colour = plot),size=4, alpha=0.5)+
  geom_smooth(data = plot, aes(slow.P_conc_mg.kg,N_conc_percent),method = "lm", colour="red", se = FALSE)+
  ggtitle("Sevilleta")+ xlab("Slowly Cycling P (mg/kg)") + ylab ("Soil N (%)")+
  theme_bw()
f  


#### Tapajos ##### 
aa<-subset(cores,dataset=="Tapajos")
site<-aggregate(cbind(N_conc_percent,total.P_conc_mg.kg,slow.P_conc_mg.kg)~site,mean, data=aa,na.rm=T)

f<-ggplot(aa, aes(slow.P_conc_mg.kg,N_conc_percent,  colour = site)) +
  geom_point(size = 2 ) +
  #geom_smooth(aes(group = 1),method = "lm", colour="black", se = FALSE) +
  geom_point(data = site, aes(slow.P_conc_mg.kg,N_conc_percent,  colour = site),size=4, alpha=0.5)+
  #geom_smooth(data = site, aes(slow.P_conc_mg.kg,N_conc_percent),method = "lm", colour="red", se = FALSE)+
  ggtitle("Tapajos")+ xlab("Slowly Cycling P (mg/kg)") + ylab ("Soil N (%)")+
  theme_bw()
f  


