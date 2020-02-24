library(dplyr)
library(lubridate)
library(stringr)
library(readxl)
library(sf)
library(rgdal)
source("./code/functions.R")
source("./code/FFP_R/calc_footprint_FFP_climatology_JB.R")
source("./code/tower_parameters.R")

################################################################################
########################## Read in cleaned data ################################
################################################################################

CovFmv.file <- readRDS("./data/combined_14-19_Fmv_Cov.rds")

Guyaflux.full <- read_excel("./data/GX-METEO+EDDY-2004-2018E_JB.xlsx",
                            sheet=1, col_types="numeric")
names(Guyaflux.full)
names(Guyaflux.full) <- str_replace_all(names(Guyaflux.full), '[\\s]cm' , 'cm')
names(Guyaflux.full) <- str_replace_all(names(Guyaflux.full), '[\\s]\\(' , '(')
names(Guyaflux.full) <- str_replace_all(names(Guyaflux.full), '[\\s()/]' , '.')
names(Guyaflux.full) <- str_replace_all(names(Guyaflux.full), '-' , '_')
names(Guyaflux.full)

Guyaflux.clean <-
  Guyaflux.full %>%
    select(-1, everything()) %>%
    mutate(Hour = floor(Hour.min)) %>%
    mutate(Minute = Hour.min%%1 * 60) %>%
    mutate(Second = as.integer(0)) %>%
    mutate(Date = as_date(Julian.Day, paste0(Year - 1,"-12-31"))) %>%
    mutate(Day = day(Date)) %>%
    mutate(DateTime = make_datetime(Year, Month, Day, Hour, Minute, Second,
                                   tz = "America/Cayenne")) %>%
    select(DateTime, everything(),
           -TimeB, -Hour, -Minute, -Second, -Date, -Day)

combined.file <- full_join(Guyaflux.clean, CovFmv.file,  "DateTime")

# To investigate the completeness of the data
full.dates <- (seq(from=min(combined.file$DateTime), to=max(combined.file$DateTime), by="30 min"))

full.dates <- as_tibble(as_datetime(full.dates, tz = "America/Cayenne"))
names(full.dates) <- "DateTime"

missing_times <- dplyr::setdiff(full.dates,combined.file[1])
print(paste0(dim(missing_times)[1], " missing observations!"))

# To complete set of obervations (even though some will be empty)
combined.file <- left_join(full.dates, combined.file, "DateTime")

rm(CovFmv.file, Guyaflux.clean, Guyaflux.full, full.dates, missing_times)

#write.csv(combined.file, "./data/combined_xlsx_Fmv_Cov.csv")


#################### Investigate ustar entry alignment #########################
#dodgy.ustar.entries <- 
#  combined.file %>%
#    filter((U.star - Ustar)^2 > 0.001) %>%
#    rename(Ustar.xlsx=Ustar, Ustar.Fmv=U.star) %>%
#    select(DateTime, Year, Month, Julian.Day, Hour.min, Ustar.xlsx, Ustar.Fmv, FileName.Fmv)
#
#write.csv(dodgy.ustar.entries, "./data/Ustar_mismatch.csv")



################### Investigate Wind Direction offsets #########################
#combined.file2<-
#  combined.file %>%
#    mutate(wind_d_offset = Wind.Direc - Wind_D) %>%
#    mutate(wind_S_diff = Mean_Windsp - Wind_S)

############# Set dates to calculate flux tower footprint ######################

lower.date <- make_datetime(2015, 1, 1, tz = "America/Cayenne")
upper.limit <- make_datetime(2019, 1, 1, tz = "America/Cayenne")
interval.period <- ddays(92)
# Number of half hour slots available
obs.num <- as.numeric(interval.period)/(60*30) 

upper.date <- upper.limit - ddays(1)

out.dir <- "./outputs/hpc_test/"
dir.create(out.dir, recursive = TRUE)

while(upper.date < upper.limit){
  # upper.date <- make_datetime(2019, 12, 31, tz = "America/Cayenne")
  upper.date <- lower.date + interval.period
  flux.period <- interval(lower.date, upper.date)
  
  # Filter by time period
  input.flux <- 
    combined.file %>%
    #    drop_na() %>%
    filter(DateTime %within% flux.period)
  
  obs.num <- dim(input.flux)[1]
  
  # Filter out dodgy observations
  input.flux <- 
    input.flux %>%
    filter(h > 10) %>%
    filter(zm < h) %>%
    filter(!is.na(Wind_D)) %>%
    filter(Ustar > 0)
  #     sample_n(100) 
  
  #umeanl <- 1
  ftprint <- 
    calc_footprint_FFP_climatology(zm, z0=NA,
                                   umean = input.flux$Wind_S,
                                   h = input.flux$h,
                                   ol = input.flux$L,
                                   ustar = input.flux$U.star,
                                   sigmav = input.flux$SigmaV,
                                   wind_dir = input.flux$Wind_D,
                                   domain = c(-2000, 2000, -2000, 2000),
                                   r = seq(10,80,10),
                                   smooth_data = 0,
                                   fig = 0)
  
  # Include some measure of completeness
  obs.used <- dim(input.flux)[1] - ftprint$skip_count
  completeness <- obs.used/obs.num
  
  print(paste0(obs.used, " out of ",
               obs.num, " time periods used in calculation"))
  print(paste0(completeness*100, "% of time periods used in calculation"))
  
  num <- sum(!is.na(ftprint$xr))
  poly.list <- vector(mode = "list", length =num)
  
  for(i in 1:num){
    poly.list[[i]] <- list(cbind(x=ftprint$xr[[i]], y=ftprint$yr[[i]]))
  }
  
  for(i in 1:num){
    pc <- i * 10
    poly.name <- paste0(pc, "pc")
    if(all(complete.cases(poly.list[[i]]))){
      poly <- st_geometry(st_polygon(poly.list[[i]]) + st_point(FT.coords.UTM22N))
      flag <- 0
    }
    else{
      poly <- st_convex_hull(st_multipoint(poly.list[[i]][[1]][complete.cases(poly.list[[i]][[1]]),]))
      poly <- st_geometry(poly + st_point(FT.coords.UTM22N))
      print("FORCED GEOMETRY")
      flag <- 1
    }
    
    poly.list[[i]] <- st_sf(poly,
                            name = poly.name,
                            start.date = lower.date,
                            end.date = upper.date,
                            frc_gmt = as.integer(flag),
                            obs_used = as.integer(obs.used),
                            obs_num = as.integer(obs.num),
                            complete = completeness,
                            crs=CRS.local)
    print(paste0(i, "th polygon created"))
  }
  
  combined.poly <- do.call(rbind, poly.list)
  rm(poly.list)
  
  # Save outputs

  output.name <- paste0(tower.name,lower.date,"_", upper.date,"_FTPRNT.shp")
  st_write(combined.poly, paste0(out.dir, output.name))
  
  lower.date <- upper.date + ddays(1)
}

