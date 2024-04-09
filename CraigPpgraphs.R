

## ------------------------------------------ ##
# Within-site N and P relationships  ----
## ------------------------------------------ ##
# Load necessary libraries
library(Rmisc)
library(ggplot2)
library(dplyr)

## ------------------------------------------ ##
# Housekeeping -----
## ------------------------------------------ ##

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
cores <- read.csv(file = file.path("data", "tidy_data", 
                                    "sparc-soil-p_stats-ready_mineral_0-10.csv"))

#cores$set<-as.numeric(as.factor(cores$dataset))

## ------------------------------------------ ##
# Making summary datasets and running linear models -----
## ------------------------------------------ ##

#create a data frame that contains the number of rows with which we have sites
Final_table<-data.frame(matrix(nrow=length(unique(cores$dataset)),ncol=1))
names(Final_table) <- c('dataset')
Final_table$dataset<-unique(cores$dataset)

# SIMPLE LINEAR REGRESSIONS BY SITE OF TOTAL P VS TOTAL N
# 
sum_table<-data.frame(matrix(nrow=length(unique(cores$dataset)),ncol=3))
names(sum_table) <- c('dataset', 'Total_P.N_slope', 'Total_P.N_.pvalue')
cores_totalP<-cores
cores_totalP<-subset(cores_totalP,is.na(total.P_conc_mg.kg)==F)
cores_totalP<-subset(cores_totalP,is.na(N_conc_percent)==F)
cores_totalP<-subset(cores_totalP,lter!="CDR")#remove CDR because only 1 observation
cores_totalP<-subset(cores_totalP,dataset!="Konza_2")#remove Konza_2 because only 1 observation
cores_totalP<-subset(cores_totalP,dataset!="Toolik_1")# Total P showing up as zero but should be NA
cores_totalP$set<-as.numeric(as.factor(cores_totalP$dataset))
for(i in 1:max(cores_totalP$set)){
  a<-subset(cores_totalP,set==i)
  b<-summary(lm(N_conc_percent~total.P_conc_mg.kg,data = a))
    sum_table[i,1]<-first(a$dataset)
    sum_table[i,2]<-b$coefficients[2,1]
    sum_table[i,3]<-b$coefficients[2,4]
    }
Final_table<-merge(Final_table,sum_table,all.x = T)               


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

# PLOT LEVEL ANALYSES
plot_totalP<-aggregate(cbind(N_conc_percent,total.P_conc_mg.kg)~dataset+site+block+plot,mean, data=cores,na.rm=T)

sum_table<-data.frame(matrix(nrow=length(unique(cores$dataset)),ncol=3))
names(sum_table) <- c('dataset', 'Total_P.N_slope_Plot', 'Total_P.N_.pvalue_Plot')
plot_totalP<-cores
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


#PLOT LEVEL SLOW P
plot_slowP<-aggregate(cbind(N_conc_percent,slow.P_conc_mg.kg)~dataset+site+block+plot,mean, data=cores,na.rm=T)

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

#SITE LEVEL

site_totalP<-aggregate(cbind(N_conc_percent,total.P_conc_mg.kg)~dataset+site,mean, data=cores,na.rm=T)

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


#site LEVEL SLOW P
site_slowP<-aggregate(cbind(N_conc_percent,slow.P_conc_mg.kg)~dataset+site+block+site,mean, data=cores,na.rm=T)

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

# dataset LEVEL SLOW P
summary <- site_slowP %>% 
  select(dataset,N_conc_percent,slow.P_conc_mg.kg) %>% 
  group_by(dataset) %>% 
  dplyr::summarise(mean_N = mean(N_conc_percent, na.rm = TRUE),mean_P = mean(slow.P_conc_mg.kg, na.rm = TRUE),sd_N = sd(N_conc_percent, na.rm = TRUE),sd_P = sd(slow.P_conc_mg.kg, na.rm = TRUE),se_N = plotrix::std.error(N_conc_percent, na.rm = TRUE),se_P = plotrix::std.error(slow.P_conc_mg.kg, na.rm = TRUE))

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


