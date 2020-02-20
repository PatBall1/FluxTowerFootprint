library(dplyr)
library(tidyr)

year <- 2017

folder <- dir("./data/", pattern = paste0("RESU",year), full.names = TRUE)


##### _Fmv data

# Read data

fmv.files <- list.files(folder, pattern = "*Fmv.csv",
                        full.names = TRUE, recursive = TRUE)

read.fmv <- function(fmv.path){
  fmv.names <- names(read.csv(fmv.path, skip = 0, nrows=1))
  fmv.file <- tibble::as_tibble(read.csv(fmv.path, header = FALSE, skip = 2))
  names(fmv.file) <- fmv.names
  return(fmv.file)
}

fmv.units <- as.data.frame(t(
  read.csv(fmv.files[1], header = FALSE, nrows = 2)
))
names(fmv.units) <- c("Quantity", "Unit")

# A list containing all fmv
fmv.list <- lapply(fmv.files, read.fmv)


#setClass("fmv", slots =list(data="data.frame", units="data.frame"))
#fmv.test <- new("fmv", data = fmv.list[[2]], units = fmv.units)


# To keep a record of units
fmv.units <- as.data.frame(t(
  read.csv(paste0("./data/RESU2017FMV/", fmv.17[i]), header = FALSE, nrows = 2)
  ))
names(fmv.units) <- c("Quantity", "Unit")

fmv.names <- names(
  read.csv(paste0("./data/RESU2017FMV/", fmv.17[i]), skip = 0, nrows=1)
)
fmv.file <- as_tibble(read.csv(paste0("./data/RESU2017FMV/",
                                      fmv.17[i]), header = FALSE, skip = 2))
names(fmv.file) <- fmv.names


# Set NAs and filter empty rows

fmv.file <- 
  fmv.file %>%
  na_if(-9999) %>%
  filter(DecTim %% 0.5 == 0)

fmv.file1 <- 
  fmv.file %>%
    drop_na(-1:-3)

# Filter out those rows that are all NA (except atmospheric pressure)
fmv.file1 <-
  fmv.file %>%
    filter(!(rowSums(is.na(fmv.file[,-1:-3])) == ncol(fmv.file[,-1:-3]) - 1))



# This can select those rows that aren't effectively empty
sum(!(rowSums(is.na(fmv.file[,-1:-3])) == ncol(fmv.file[,-1:-3]) - 1))

sum(rowSums(is.na(fmv.file[,-1:-3])) > 0)

##### _Cov data

cov.17 <- list.files("./data/RESU2017FMV", pattern = "*Cov.csv",
                     recursive = TRUE)


cov.names <- names(
  read.csv(paste0("./data/RESU2017FMV/", cov.17[i]), skip = 0, nrows=1)
)
cov.file <- as_tibble(read.csv(paste0("./data/RESU2017FMV/",
                                      cov.17[i]), header = FALSE, skip = 1))
names(cov.file) <- cov.names

head(cov.file)

# Set NAs and filter empty rows

cov.file <- 
  cov.file %>%
  na_if(-9999) %>%
  filter(DecTim %% 0.5 == 0) %>%
  filter(!is.na(Vari_U.wind))

!is.na(cov.file[1,]$Vari_Cp.CO2)

# For Sigma V (standard deviation of lateral velocity fluctuations)
# SD is sqrt of var 
sqrt(cov.file[1]$Vari_V.wind)

