################################################################################
################################################################################
###### READ AND CLEAN FLUX TOWER DATA IN PREPARATION FOR FOOTPRINTS ############
################################################################################
################################################################################
library(dplyr)
library(tidyr)
library(lubridate)
source("./code/functions.R")

#folder <- dir("./data/", pattern = paste0("RESU", year), full.names = TRUE)
folder <- dir("./data/", pattern = paste0("RESU"), full.names = TRUE)

################################################################################
########################### _Fmv data ##########################################
################################################################################
# These files contain umean, z/L, ustar, wind_dir

# Read data
fmv.file.names <- rev(list.files(folder, pattern = "*Fmv.csv",
                        full.names = TRUE, recursive = TRUE))

# Start with the last file (presumably the most up to date) and keep adding
# previous data
fmv.file <- read.fmv(fmv.file.names[1])
fmv.file <-
  fmv.file %>%
    mutate(Year = Yr + 2000) %>%
    mutate(Hour = floor(DecTim)) %>%
    mutate(Minute = DecTim%%1 * 60) %>%
    mutate(Second = as.integer(0)) %>%
    mutate(Date = as_date(Doy, paste0(Year - 1,"-12-31"))) %>%
    mutate(Day = day(Date)) %>%
    mutate(Month = month(Date)) %>%
    mutate(DateTime = make_datetime(Year, Month, Day, Hour, Minute, Second,
                                    tz = "America/Cayenne")) %>%
    mutate(FileName = fmv.file.names[1]) %>%
    select(DateTime, everything(), FileName,
           -Year, -Month, -Hour, -Minute, -Second, -Date, -Day)


for(i in 2:length(fmv.file.names)){
  print(i)
  #i <- 51 # For debugging
  print(fmv.file.names[i])
  if(file.size(fmv.file.names[i]) == 0){
    print("WARNING: EMPTY FILE")
    next
  } 
  
  fmv.prev <- read.fmv(fmv.file.names[i])
  fmv.prev <-
    fmv.prev %>%
      mutate(Year = Yr + 2000) %>%
      mutate(Hour = floor(DecTim)) %>%
      mutate(Minute = DecTim%%1 * 60) %>%
      mutate(Second = as.integer(0)) %>%
      mutate(Date = as_date(Doy, paste0(Year - 1,"-12-31"))) %>%
      mutate(Day = day(Date)) %>%
      mutate(Month = month(Date)) %>%
      mutate(DateTime = make_datetime(Year, Month, Day, Hour, Minute, Second,
                                      tz = "America/Cayenne")) %>%
      mutate(FileName = fmv.file.names[i]) %>%
      select(DateTime, everything(), FileName,
             -Year, -Month, -Hour, -Minute, -Second, -Date, -Day)
  
  # Extract time info from each file
  time.file <- select(fmv.file, DateTime)
  time.prev <- select(fmv.prev, DateTime)
  # Find out which dates (rows of data) should be added from previous file
  add.times <- dplyr::setdiff(time.prev, time.file)
  # Filter only those previous rows (dates) necessary to add
  #####NEED TO FIX THIS FILTERING
  add <- filter(fmv.prev, DateTime %in% add.times$DateTime)
  
  # Update file with additional data
  fmv.file <- bind_rows(add, fmv.file)
  rm(fmv.prev, time.file, time.prev, add.times, add)
}

# Those files that don't fit the standard timing pattern
bad.obs.fmv <- filter(fmv.file, DecTim%%0.25 != 0)

# Check for distinct rows

#distinct(fmv.file)


# Save a raw-ish copy of the full data before calculating new variables
write.csv(fmv.file, "./data/combined_14-19_Fmv.csv")
saveRDS(fmv.file, "./data/combined_14-19_Fmv.rds")

saveRDS(bad.obs.fmv, "./data/BAD_14-19_Fmv.rds")


########################### _Cov data ##########################################
# This data contains variance information useful for footprint calculations

cov.file.names <- rev(list.files(folder, pattern = "*Cov.csv",
                        full.names = TRUE, recursive = TRUE))

cov.file <- read.cov(cov.file.names[1])
cov.file <-
  cov.file %>%
    mutate(Year = Yr + 2000) %>%
    mutate(Hour = floor(DecTim)) %>%
    mutate(Minute = DecTim%%1 * 60) %>%
    mutate(Second = as.integer(0)) %>%
    mutate(Date = as_date(Doy, paste0(Year - 1,"-12-31"))) %>%
    mutate(Day = day(Date)) %>%
    mutate(Month = month(Date)) %>%
    mutate(DateTime = make_datetime(Year, Month, Day, Hour, Minute, Second,
                                    tz = "America/Cayenne")) %>%
    mutate(FileName = cov.file.names[1]) %>%
    select(DateTime, everything(), FileName,
           -Year, -Month, -Hour, -Minute, -Second, -Date, -Day)

for(i in 2:length(cov.file.names)){
  print(i)
  #i <- 2 # For debugging
  print(cov.file.names[i])
  if(file.size(cov.file.names[i]) == 0){
    print("WARNING: EMPTY FILE")
    next
  } 
  
  cov.prev <- read.cov(cov.file.names[i])
  cov.prev <-
    cov.prev %>%
      mutate(Year = Yr + 2000) %>%
      mutate(Hour = floor(DecTim)) %>%
      mutate(Minute = DecTim%%1 * 60) %>%
      mutate(Second = as.integer(0)) %>%
      mutate(Date = as_date(Doy, paste0(Year - 1,"-12-31"))) %>%
      mutate(Day = day(Date)) %>%
      mutate(Month = month(Date)) %>%
      mutate(DateTime = make_datetime(Year, Month, Day, Hour, Minute, Second,
                                      tz = "America/Cayenne")) %>%
      mutate(FileName = cov.file.names[i]) %>%
      select(DateTime, everything(), FileName,
             -Year, -Month, -Hour, -Minute, -Second, -Date, -Day)
  # Extract time info from each file
  time.file <- select(cov.file, DateTime)
  time.prev <- select(cov.prev, DateTime)
  # Find out which dates (rows of data) should be added from previous file
  add.times <- dplyr::setdiff(time.prev, time.file)
  # Filter only those previous rows (dates) necessary to add
  add <- filter(cov.prev, DateTime %in% add.times$DateTime)
  
  # Update file with additional data
  cov.file <- bind_rows(add, cov.file)
  rm(cov.prev, time.file, time.prev, add.times, add)
}


bad.obs.cov <- filter(cov.file, DecTim%%0.5 != 0)
distinct(cov.file)

write.csv(cov.file, "./data/combined_14-19_Cov.csv")
saveRDS(cov.file, "./data/combined_14-19_Cov.rds")

saveRDS(bad.obs.fmv, "./data/BAD_14-19_Fmv.csv")
saveRDS(bad.obs.cov, "./data/BAD_14-19_Cov.csv")


