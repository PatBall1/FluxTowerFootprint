library(maptools)
library(raster)
library(GISTools)
library(prettymapr)  
library(FREddyPro)

## Footprint estimation
EddyPro_Gx<-readEddyPro("eddypro_GUYAFLUX_Li7000_AdvModeFINAL_full_output_2017.csv", na=-9999)

ftp2.1.wet=calculateFootprint(EddyPro_Gx,displacement=23.45,stability=1,fetch=1000,grid=200,height=55,lowerDay=32,upperDay=120)
ftp2.1.dry=calculateFootprint(EddyPro_Gx,displacement=23.45,stability=1,fetch=1000,grid=200,height=55,lowerDay=213,upperDay=304)

for (i in c(10,20,30,40,50,60,70,80))
{

ftp2.1p=calculatePercentFootprint(ftp2.1.wet,percent=i)
ftp2_points=exportFootprintPoints(ftp2.1p, 286673, 583800)

ftp2_points2<-ftp2_points[is.na(ftp2_points$z)==F,]
ftp2_points2<-ftp2_points2[ftp2_points2$z>=i-10,]


x2 <- ftp2_points2[,1]  ##tells R which columns is which
y2 <- ftp2_points2[,2]
dat2<-cbind(x2,y2)
z2<-chull(x2,y2)       ##calculates the convex hull 

coords2 <- dat2[c(z2, z2[1]), ]
sp_poly2 <- SpatialPolygons(list(Polygons(list(Polygon(coords2)), ID=1)))
sp_poly_df2 <- SpatialPolygonsDataFrame(sp_poly2, data=data.frame(ID=1))
writeOGR(sp_poly_df2, "chull_wet", layer=paste("FootprintGx_",i), driver="ESRI Shapefile")
}

for (i in c(10,20,30,40,50,60,70,80))
{

ftp2.1p=calculatePercentFootprint(ftp2.1.dry,percent=i)
ftp2_points=exportFootprintPoints(ftp2.1p, 286673, 583800)

ftp2_points2<-ftp2_points[is.na(ftp2_points$z)==F,]
ftp2_points2<-ftp2_points2[ftp2_points2$z>=i-10,]



x2 <- ftp2_points2[,1]  ##tells R which columns is which
y2 <- ftp2_points2[,2]
dat2<-cbind(x2,y2)
z2<-chull(x2,y2)       ##calculates the convex hull 

coords2 <- dat2[c(z2, z2[1]), ]
sp_poly2 <- SpatialPolygons(list(Polygons(list(Polygon(coords2)), ID=1)))
sp_poly_df2 <- SpatialPolygonsDataFrame(sp_poly2, data=data.frame(ID=1))
writeOGR(sp_poly_df2, "chull_dry", layer=paste("FootprintGx_",i), driver="ESRI Shapefile")
}

