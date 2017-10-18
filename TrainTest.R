library("raster", lib.loc="~/R/win-library/3.4")
library("rasterVis", lib.loc="~/R/win-library/3.4")
library("rgdal", lib.loc="~/R/win-library/3.4")
library("dplyr", lib.loc="~/R/win-library/3.4")

r=raster("C:/Users/vprescott/Desktop/coastlines-split-4326/coastlines-split-4326/na5.tif")

plot(ED)

xy2<-ED[2:1]
spdf<-SpatialPoints(coords=xy2,
                  proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))
plot(r, add=TRUE)
plot(spdf)
#training data
r.clip<-crop(spdf, r)
plot(r.clip)
rclip<-as.data.frame(r.clip)
write.csv(rclip, file="C:/Users/vprescott/Desktop/Coordinates/Testing/test_Channa argus.csv", row.names=FALSE)

#testing data
diff<-setdiff(xy2,rclip)
head(diff)
write.csv(diff, file="C:/Users/vprescott/Desktop/Coordinates/Training/train_Procambarus marmorkrebs.csv", row.names=FALSE)

