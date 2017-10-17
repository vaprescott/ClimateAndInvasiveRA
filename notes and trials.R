species<-list.files(path="C:/Users/vprescott/Desktop/RAMP2/Full_coords",
                    pattern="csv", full.names=TRUE)
current<-raster("E:/postdoc/Bioclim/WorldClim/Current/tif_files/bio_1.tif")
plot(current)

for(i in 1:length(species)){
  sp.coords<-read.csv(species[i], header=TRUE)
  sp.coords<-read.csv("C:/Users/vprescott/Desktop/RAMP2/Full_coords/Silurus_glanis_full.csv",
                      header=TRUE)
  nrow=nrow(sp.coords)
  
  #filename<-species[i]
  filename<-sub(pattern = "(.*)\\..*$", replacement = "\\1",
                basename(species[i]))
  filename<-sub(pattern="_", replacement =" ", 
                basename(filename))
  filename<-sapply(strsplit(filename, "_full"), "[[",1)
  set.seed(0)
  group=kfold(sp.coords, 5)
  pres_train=sp.coords[group!=1,]
  pres_test=sp.coords[group==1,]
  set.seed(10)
  max.x<-max(sp.coords$Latitude)
  min.x<-min(sp.coords$Latitude)
  max.y<-max(sp.coords$Longitude)
  min.y<-min(sp.coords$Longitude)
  ext=extent(min.x,max.x,min.y,max.y)
  backg=randomPoints(current, n=nrow,ext=ext, extf=1.25)
  colnames(backg)=c('Longitude','Latitude')
  group=kfold(backg,5)
  backg_train=backg[group!=1,]
  backg_test=backg[group==1,]
  #plot(!is.na(current), 
  #     col=c('white','light grey'), 
  #     legend=FALSE,
  #     ext=ext)
  plot(sp.coords, main=c(filename, "current3"))
        
  #plot(!is.na(current), col=c('white','light grey'), legend=FALSE)
  #plot(ext, add=TRUE)
  #points(sp.coords, pch=".", cex=0.5, col='red')
  #points(backg_train, pch='-', cex=0.5, col='yellow')
  #points(backg_test, pch='+', cex=0.5, col='black')
  #points(pres_train, pch='+', col='green')
  #points(pres_test, pch='+', col='blue') 
}

for(i in 1:length(species)){
  sp.coords<-read.csv(species[i], header=TRUE)
  filename<-sub(pattern = "(.*)\\..*$", replacement = "\\1",
                basename(species[i]))
  filename<-sub(pattern="_", replacement =" ", 
                basename(filename))
  filename<-sapply(strsplit(filename, "_full"), "[[",1)
  plot(sp.coords, main=c(filename, "current"))
}

colfun<-colorRampPalette(
  c("blue","cyan","green","yellow","red"))
  
levelplot(rcp45.50.predict, 
          main=paste(filename, "RCP 4.5 2050"), 
          xlim=c(-95,-70), ylim=c(40,52),
          at=seq(0,1, length.out=120),
          col.regions=colfun)
          

f=raster(rcp45.50,1)
plot(!is.na(f), col=c('white','light grey'), legend=FALSE)
          

if_<- unique(if_[ , 1:2 ] )
nrows(mf)
View(mf)
write.csv(if_,
          file="E:/postdoc/analysis_files/sp_coords/full_coords/Carassius_carassius_full.csv")

#create testing and training data from north america
bias3<-readOGR("C:/Users/vprescott/Desktop/coastlines-split-4326/coastlines-split-4326/NorthAmerica.shp")
ext=extent(bias3)
xy1<-ED[1:2]
xy2<-ED2[2:1]
spdf[c('longitude','latitude')]
spdf<-SpatialPoints(coords=xy1, data=ED,
                             proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84"))
spdf<-SpatialPoints(coords=xy1,
                    proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84"))

pt<-sp::over(aa,spdf)

plot(spdf)
plot(aa, add=TRUE)
pres_train<-extract(spdf,bias3)
a<-spdf[aa]
plot(spdf, ext=extent(aa))
plot(aa)
plot(spdf, add=TRUE)
aa<-spTransform(bias3, crs("+proj=longlat +datum=WGS84 +ellps=WGS84"))
a3<-data.frame(a)
head(a3)
df <- data.frame(x=coordinates(a)[,1], y=coordinates(a)[,2], a@data)

write.csv(a3, 
          file="C:/Users/vprescott/Desktop/")

r.clip<-crop(spdf,aa)
plot(r.clip)
plot(aa, add=TRUE)
r.mask<-rasterize(r.clip)

ab<-gIntersects(spdf,aa)
