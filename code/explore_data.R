library(dplyr)
library(lubridate)
################################################################################


#### Make sure data is read in to the proper resolution and not rounded
data <- tbl_df(read.csv("./data/GUYAFLUX_2004_2018.csv"))

data1 <- data



ob_lgth <- function(T, ustar, H)
{
# Calculate Obukov length
# Required parameters: air density, specific heat capacity, temperature,
# friction velocity, Von Karman Constant, acceleration due to gravity,
# sensible heat flux
}

################################################################################
############################## CLEAN DATA ######################################
################################################################################


data1 <- 
  data1 %>%
  select(-1) %>%
  mutate(Hour = as.integer(Hour.min)) %>%
  mutate(Minute = as.integer(ifelse(Hour.min%%1==0, 0, 30))) %>%
  mutate(Second = as.integer(0)) %>%
  select(Year, Month, Julian.Day, Hour, Minute, Second,
         everything(), -time) %>%
  mutate(Date = as_date(Julian.Day, paste0(Year - 1,"-12-31"))) %>%
  mutate(Day = day(Date)) %>%
  mutate(DateTime = make_datetime(Year, Month, Day, Hour, Minute, Second),
    tz = "America/Cayenne") %>%
  select(DateTime, everything(), Julian.Day)

data2 <-
  data1 %>% filter(hour(DateTime) == 12 & minute(DateTime) == 0)

################################################################################
######################## FILTER DATA FOR PERIOD ################################
################################################################################

data1 %>%
  filter(Year==2018, Month == 8)



