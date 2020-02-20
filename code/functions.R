# Function to read in Fmv files
read.fmv <- function(fmv.path){
  fmv.names <- names(read.csv(fmv.path, skip = 0, nrows=1))
  fmv.file <- tibble::as_tibble(read.csv(fmv.path, header = FALSE, skip = 2))
  names(fmv.file) <- fmv.names
  return(fmv.file)
}

# Function to read in Cov files
read.cov <- function(cov.path){
  cov.file <- tibble::as_tibble(read.csv(cov.path, header = TRUE))
  return(cov.file)
}

# A function to calculate the density of humid air.
AirDensity <- function(phi, T, p=101325){
  # A function to calculate the density of humid air.
  # Function inputs
  #   phi = relative humidity
  #   T = Temperature in degrees C
  #   P = Atmospheric pressure
  
  # The Buck equation. Buck (1996), Buck (1981)
  # https://journals.ametsoc.org/doi/pdf/10.1175/1520-0450%281981%29020%3C1527%3ANEFCVP%3E2.0.CO%3B2
  p_sat <- 1000 * 0.61121 * exp((18.678 - T / 234.5)*(T / (257.14 + T)))
  
  # The vapor pressure of water may be calculated from the saturation vapor
  # pressure and relative humidity. It is found by:
  p_v = phi  * p_sat
  
  # T in degrees C
  
  # To calculate the density 
  p_d <- p - p_v # parial pressure dry air
  M_d <- 0.0289564 #kg/mol  molar mass of dry air
  M_v <- 0.018016 #kg/mol  molar mass of water vapour
  R <- 8.314462 #J/(K.mol)  gas constant
  
  # Equation for air density of moist air
  rho <- (p_d * M_d + p_v * M_v)/(R * (T+273.15))  
  return(rho)
}


# Function to calculate Obukov Length
ObukovLength <- function(phi, T, p=101325, Cp= 1.005, ustar, H){
  # Air density
  rho <- AirDensity(phi, T, p=101325)
  # Von Karman constant
  k <- 0.41
  # Acceleration due to gravity
  g <- 9.81 #m/s
  L <- (-rho * Cp * T * ustar^3)/(k * g * H)
  return(L)
}


# Function to calculate boundary layer height
# As suggested by Kljun et al. (2015) Appendix B
LayerHeight <- function(L, ustar, lat){
  angvel <- 7.2921e-5 #rad/s  https://www.iers.org/
  f <- 2 * angvel * sin(lat *(pi/180))
  h <- (L/3.8)*(-1 + (1+2.28*(ustar/(f*L))^(1/2)))
  h[is.nan(h)] <- NA
  return(h)
}
