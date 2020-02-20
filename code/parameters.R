################################################################################
############################ SET PARAMETERS ####################################
################################################################################

#    zm       = Measurement height above displacement height (i.e. z-d) [m]
#               usually a scalar, but can also be a vector 
#    z0       = Roughness length [m] - enter [NaN] if not known 
#               usually a scalar, but can also be a vector 
#    umean    = Vector of mean wind speed at zm [ms-1] - enter [NaN] if not known 
#               Either z0 or umean is required. If both are given, z0 is selected to calculate the footprint
#    h        = Vector of boundary layer height [m]
#    ol       = Vector of Obukhov length [m]
#    sigmav   = Vector of standard deviation of lateral velocity fluctuations [ms-1]
#    ustar    = Vector of friction velocity [ms-1]
#    wind_dir = Vector of wind direction in degrees (of 360) for rotation of the footprint

# zm = zreceptor - zd
# zreceptor is the height of the receptor above ground and zd is the zero-plane displacement height.
zreceptor <- 55 # receptor height above ground is 55 m

# Zero plane = height at which mean velocity is zero
# Approximated as 0.7 * max tree height?
d <- 23.45 # Borrowed from previous calulation NEEDS CHECKING
zm <- zreceptor - d

#### Need to derive Obukhov length
# Von Karman constant
k <- 0.41

# Acceleration due to gravity
g <- 9.81 #m/s


# Specific heat capacity of air
Cp

# Density of air
# Needs to be calculated? Not that simple for humid air?
p <- 1.225

# we have relative humidity. To calculate saturation vapour pressure...

# The saturation vapor pressure of water at any given temperature is the vapor
# pressure when relative humidity is 100%. One formula used to find the
# saturation vapor pressure is:
# See https://wahiduddin.net/calc/density_algorithms.htm
# See https://en.wikipedia.org/wiki/Vapour_pressure_of_water
# Use the buck equation?

# p_vSat <- 100 * 6.102 * 10^((7.5*T)/(T+237.8))


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
  

# Obukov Length
# Require 
ObukovLength <- function(phi, T, p=101325, Cp, ustar){
  # Air density
  rho <- AirDensity(phi, T, p=101325)
  # Von Karman constant
  k <- 0.41
  # Acceleration due to gravity
  g <- 9.81 #m/s
  L <- (-rho * Cp * T * ustar^3)/(k * g * H)
  return(L)
}
    
# We have ustart, T, and H from flux tower    


# Need to derive boundary layer height
# See Appendix B of Kljun et al. (2015)
# Depends on stability
# Stability class unstable (z/L < -0.02), near neutral (- 0.02<z/L<0.02) and
# stable (z/L>0.02)

# For neutral to stable Nieuwstadt (1981)
# For positive L
L <- 2
ustar <- 5
lat <- 5.27877 # Guyaflux latitude
angvel <- 7.2921e-5 #rad/s  https://www.iers.org/
f <- 2 * angvel * sin(lat *(pi/180))
h <- (L/3.8)*(-1 + (1+2.28*(ustar/(f*L))^(1/2)))
h

LayerHeight <- function(L, ustar, lat){
  angvel <- 7.2921e-5 #rad/s  https://www.iers.org/
  f <- 2 * angvel * sin(lat *(pi/180))
  h <- (L/3.8)*(-1 + (1+2.28*(ustar/(f*L))^(1/2)))
  return(h)
}

# If z is negative, have to perform a calculation to work out a change in
# boundary layer height

dh 

