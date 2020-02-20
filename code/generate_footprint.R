library(sf)
library(rgdal)
library(dplyr)
library(tidyr)
library(lubridate)
source("./code/functions.R")
source("./code/FFP_R/calc_footprint_FFp_climatology_JB.R")
source("./code/tower_parameters.R")

################################################################################
########################## Read in cleaned data ################################
################################################################################

fmv.file <- readRDS("./data/combined_14-19_Fmv.rds")
cov.file <- readRDS("./data/combined_14-19_Cov.rds")

################################################################################
####################### Derive additional variables#############################
################################################################################

fmv.file <-
  fmv.file %>%
  na_if(-9999) %>%
  mutate(L = zreceptor / Z.over.L) %>%
  mutate(h = LayerHeight(L, U.star, as.numeric(coords[2]))) %>%
  select(DateTime, Yr, Doy, DecTim, Mean_Windsp, U.star, Wind.Direc, L, h, FileName)

cov.file <-
  cov.file %>%
  na_if(-9999) %>%
  mutate(SigmaV = sqrt(Vari_V.wind)) %>%
  select(DateTime, SigmaV, FileName)

################################################################################
######################## COMBINE Fmv and Cov files #############################
################################################################################

combined.file <- full_join(fmv.file, cov.file, 
                           by="DateTime")

combined.file <-
  combined.file %>%
  filter(minute(DateTime) == 0 | minute(DateTime) == 30) %>%
  rename(FileName.Fmv = FileName.x, FileName.Cov = FileName.y) %>%
  select(everything(), FileName.Fmv, FileName.Cov)

saveRDS(combined.file, "./data/combined_14-19_Fmv_Cov.rds")


