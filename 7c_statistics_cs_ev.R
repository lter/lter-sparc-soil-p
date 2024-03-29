## ------------------------------------------ ##
# SPARC Soil P -- Data Exploration and Statistics
## ------------------------------------------ ##
# Script author(s): Craig See & Ellery Vaughan 

# Purpose:
## Take data from end of script 5, which has been subset to top layer of mineral soil
## And run stats 

## ------------------------------------------ ##
# Housekeeping -----
## ------------------------------------------ ##

# Load necessary libraries
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive, supportR)

# Create necessary sub-folder(s)
dir.create(path = file.path("stat_results"), showWarnings = F)

# Clear environment
rm(list = ls())

# Identify needed tidy file(s)
tidy_drive <- googledrive::as_id("https://drive.google.com/drive/u/0/folders/1pjgN-wRlec65NDLBvryibifyx6k9Iqy9")

# Identify the archival data in that folder and download it
googledrive::drive_ls(path = tidy_drive) %>%
  dplyr::filter(name == "sparc-soil-p_stats-ready_mineral_0-10.csv") %>%
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "tidy_data", .$name))

# Read that file in
all_v1 <- read.csv(file = file.path("data", "tidy_data", 
                                    "sparc-soil-p_stats-ready_mineral_0-10.csv"))

avgs <- read.csv(file = file.path("data", "tidy_data", 
                                    "sparc-soil-p_site-avgs_mineral_0-10.csv"))

hist(all_v1$depth.start_cm)
hist(all_v1$depth.end_cm)

# First model attempt
library(nlme) # to run mixed effects models 
library(sjPlot) # to get tab model
library(lme4)
library(lmerTest)
library(tidyverse)

# Subsetting data to keep only sites we want to retain slow P data for for now
# keeping all sites we have slow P for
all_v1_slow_inc <- all_v1 %>% 
  filter(dataset %in% c("Calhoun","Coweeta","FloridaCoastal","Hubbard Brook","Jornada_2","Luquillo_1","Luquillo_2","Niwot_1","Niwot_5","Sevilleta_1","Tapajos"))

M1 <-lmer(N_conc_percent ~ slow.P_conc_mg.kg + (slow.P_conc_mg.kg | dataset/site/plot) , data=all_v1)

M2 <- lm(mean_N ~ mean_P, data=means)

anova(M2)
summary(M2)
tab_model(M2)

means <- avgs %>% 
  group_by(dataset) %>% 
  summarise(mean_P = mean(mean_slow.P_conc_mg.kg),
            mean_N = mean(mean_N_conc_percent))

plot(means$mean_P,means$mean_N)

ggplot(means, aes(x=mean_P,mean_N,color=dataset) ) +
  geom_point()

means <- means %>% 
  filter(!is.na(mean_P)) 

plot <- ggplot(data=avgs, aes(x=mean_slow.P_conc_mg.kg, y=mean_N_conc_percent) ) + 
  geom_point()

plot(avgs$mean_slow.P_conc_mg.kg,avgs$mean_N_conc_percent)

## ------------------------------------------ ##
# Within Site Slow P Models  -----
## ------------------------------------------ ##
# Starting 01/31/24

## Hubbard Brook (Slow P is cold nitric acid = "apatite" extraction)

HBR <- subset(all_v1, dataset_simp == "HBR") 

HBR_lm <- lm(N_conc_percent ~ slow.P_conc_mg.kg, data = HBR)
anova(HBR_lm)
summary(HBR_lm)
tab_model(HBR_lm)

# not letting slopes vary 
# singularity issue = convergence issues
# slope is the exact same between lm and lmer 
HBR_M1 <-lmer(N_conc_percent ~ slow.P_conc_mg.kg + (1 | site) , data = HBR)

anova(HBR_M1)
summary(HBR_M1)
tab_model(HBR_M1)

## Sevilleta 

SEV1 <- subset(all_v1, dataset_simp == "SEV_1") 

SEV1 <- SEV1 %>% 
  mutate(site = factor(site),
         plot = factor(plot),
         block = factor(block),
         core = factor(core))

SEV1_lm <- lm(N_conc_percent ~ slow.P_conc_mg.kg, data = SEV1)

anova(SEV1_lm)
summary(SEV1_lm)
tab_model(SEV1_lm)

# not letting slopes vary 
# singularity issue = convergence issues
# slope is the exact same between lm and lmer 
SEV1_M1 <-lmer(N_conc_percent ~ slow.P_conc_mg.kg + (slow.P_conc_mg.kg | site/block/plot) , data = SEV1)

anova(SEV1_M1)
summary(SEV1_M1)
tab_model(SEV1_M1)

## Coweeta

CWT <- subset(all_v1, dataset_simp == "CWT") 

CWT <- CWT %>% 
  mutate(site = factor(site),
         plot = factor(plot),
         block = factor(block),
         core = factor(core))

CWT_lm <- lm(N_conc_percent ~ slow.P_conc_mg.kg, data = CWT)

anova(CWT_lm)
summary(CWT_lm)
tab_model(CWT_lm)

# not letting slopes vary 
# singularity issue = convergence issues
# slope is the exact same between lm and lmer 
CWT_M1 <-lmer(N_conc_percent ~ slow.P_conc_mg.kg + (slow.P_conc_mg.kg | site) , data = CWT)

anova(CWT_M1)
summary(CWT_M1)
tab_model(CWT_M1)

## Florida Everglades

FCE <- subset(all_v1, dataset_simp == "FCE") 

FCE <- FCE %>% 
  mutate(site = factor(site),
         plot = factor(plot),
         block = factor(block),
         core = factor(core))

FCE_lm <- lm(N_conc_percent ~ slow.P_conc_mg.kg, data = FCE)

anova(FCE_lm)
summary(FCE_lm)
tab_model(FCE_lm)

# not letting slopes vary 
# singularity issue = convergence issues
# slope is the exact same between lm and lmer 
FCE_M1 <-lmer(N_conc_percent ~ slow.P_conc_mg.kg + (slow.P_conc_mg.kg | site) , data = FCE)

anova(FCE_M1)
summary(FCE_M1)
tab_model(FCE_M1)


