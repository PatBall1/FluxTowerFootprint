library(sf)
################################################################################
######################### Tower parameters #####################################
################################################################################

# Sonic ananometer Receptor height above ground is 58 m
zreceptor <- 58 #m

# canopy height
z_can <- 35 #m

# Zero plane displacment = height at which mean velocity is zero
# Approximated as 0.7 * max tree height? / canopy height
d <- 23.45                  # Borrowed from previous calulation NEEDS CHECKING

# Height above zero plane
zm <- zreceptor - d

# Roughness length
z0 <- 0.15 * z_can


# Location and name of Flux Tower
tower.name <- "Guyaflux"
coords <- c(Long =	-52.92486, Lat = 5.27877)

FT.loc <- st_sf(st_geometry(st_point(coords)), name=tower.name, crs = 4326)

# To shift footprints to be centred around the FluxTower, need local UTM CRS
CRS.local <- 32622      # EPSG code for WGS 84 / UTM zone 22N
FT.loc.UTM22N <- st_transform(FT.loc, crs=CRS.local)
FT.coords.UTM22N <- st_coordinates(FT.loc.UTM22N)



#Bonjour James, voici un graph avec les données de vent du sonic et de notre capteur météo. Il y a bien un décalage de 90°.
#
#La hauteur de canopée : 35 m
#
#La hauteur des capteurs : 58 m (pour le sonic et les analyseurs de gaz) et 57m pour les capteurs météo.
#
#Le DIS (Zero displacement heght) : 22 m
#
#Roughness length (0.15 * canopy heigth) : 5.25
#
#Je n’ai pas trouvé  de valeur pour la boundary layer height je cherche.
#
#Tu pourras nous renvoyer les données du footprint avec le fichier et les cartes ?
#
#A+
#  
#Benoit