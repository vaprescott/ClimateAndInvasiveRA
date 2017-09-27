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
          