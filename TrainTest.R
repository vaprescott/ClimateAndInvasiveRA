library("raster", lib.loc="~/R/win-library/3.4")
library("rasterVis", lib.loc="~/R/win-library/3.4")
library("rgdal", lib.loc="~/R/win-library/3.4")
library("dplyr", lib.loc="~/R/win-library/3.4")

#r=raster("C:/Users/vprescott/Desktop/coastlines-split-4326/coastlines-split-4326/na5.tif")

#plot(ED)

#xy2<-ED[2:1]
#spdf<-SpatialPoints(coords=xy2,
#                  proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))
#plot(r, add=TRUE)
#plot(spdf)

#training data
#r.clip<-crop(spdf, r)
#plot(r.clip)
#rclip<-as.data.frame(r.clip)
#write.csv(rclip, file="C:/Users/vprescott/Desktop/Coordinates/Testing/test_Channa argus.csv", row.names=FALSE)

#testing data
#diff<-setdiff(xy2,rclip)
#head(diff)
#write.csv(diff, file="C:/Users/vprescott/Desktop/Coordinates/Training/train_Procambarus marmorkrebs.csv", row.names=FALSE)



#background test points

species<-list.files(path="D:/BrokenHardDrive/postdoc/analysis_files/training/global_training",
                    pattern="train_", full.names=TRUE)

for(i in 1:length(species)){
  sp.coords.train<-read.csv(species[i], header=TRUE)
  filename<-sub(pattern = "(.*)\\..*$", replacement = "\\1",
                basename(species[i]))
  filename<-sapply(strsplit(filename, "train_"), "[[",-1)   
  filename.full<-sub(pattern=" ", replacement ="_", 
                     basename(filename))
  form.coords<-sprintf('D:/BrokenHardDrive/postdoc/analysis_files/sp_coords/corrected_coords/%s_corrected2.csv', 
                       filename.full)
  sp.coords.full<-read.csv(file=form.coords)
  sp.coords.full<-sp.coords.full[,c("longitude","latitude")]
  colnames(sp.coords.full)[1]<-"Longitude"
  colnames(sp.coords.full)[2]<- "Latitude"
  
  set.seed(10)
  form_backg=sprintf("D:/BrokenHardDrive/postdoc/analysis_files/background_raster/%s.tif", filename)
  background=raster(form_backg)
  ext=extent(background)
  crs(background)<-"+proj=longlat +datum=WGS84"
  
raster.random.points<-function(size, background, na.rm=TRUE){
  coords<-matrix(0, nrow=size, ncol=2)
  coords[,1]<-runif(size, xmin(background), xmax(background))
  coords[,2] <- runif(size, ymin(background), ymax(background))
  if (na.rm) {
    cells <- cellFromXY(background, coords)
    na.cnt <- length(which(is.na(background[cells])))
    while (na.cnt > 0){
      recs <- which(is.na(background[cells]))
      coords[recs,1] <- runif(length(recs), xmin(background), xmax(background))
      coords[recs,2] <- runif(length(recs), ymin(background), ymax(background))
      cells <- cellFromXY(background, coords)
      na.cnt <- length(which(is.na(background[cells])))
    }}
  return(coords)
}
# now call the function to generate random points
coords <- raster.random.points(nrow(sp.coords.full), background)

# plot the raster and the random points
#plot(background)
#points(coords, pch=19, cex=0.2)
backg<-coords
colnames(backg)=c('Longitude','Latitude')
group=kfold(backg,5)
backg_train=backg[group!=1,]
backg_test=backg[group==1,]
form_bg_test<-sprintf("D:/BrokenHardDrive/postdoc/analysis_files/test_background/%s.csv", filename)
write.csv(backg_test,
          file=form_bg_test)
form_bg_train<-sprintf("D:/BrokenHardDrive/postdoc/analysis_files/train_background/%s.csv", filename)
write.csv(backg_train,
          file=form_bg_train)
}
