library("dismo")
library("gbm")
library("maps")
library("mapdata")
library("maptools")
library("rgdal")
library("raster")
library("rasterVis")


#bring in tiff files for climate data and put them into one rasterstack
current.list=list.files(path="E:/BrokenHardDrive/postdoc/Bioclim/WorldClim/Current/tif_files", 
                        pattern="tif$", full.names=TRUE )
current=stack(current.list)

#RCP 45 2050
rcp45.50.list=list.files(path="E:/BrokenHardDrive/postdoc/Bioclim/Mod_WorldClim/Modlayers_2050_45/mod_50_45_tiff_gl", 
                         pattern="tif$", full.names=TRUE )
rcp45.50=stack(rcp45.50.list)

#RCP 45 2070
rcp45.70.list=list.files(path="E:/BrokenHardDrive/postdoc/Bioclim/Mod_WorldClim/Modlayers_2070_45/mod_70_45_tiff_gl", 
                         pattern="tif$", full.names=TRUE )
rcp45.70=stack(rcp45.70.list)

#RCP85 2050
rcp85.50.list=list.files(path="E:/BrokenHardDrive/postdoc/Bioclim/Mod_WorldClim/Modlayers_2050_85/mod_50_85_tiff_gl", 
                         pattern="tif$", full.names=TRUE )
rcp85.50=stack(rcp85.50.list)

#RCP85 2070
rcp85.70.list=list.files(path="E:/BrokenHardDrive/postdoc/Bioclim/Mod_WorldClim/Modlayers_2070_85/mod_70_85_tiff_gl", 
                         pattern="tif$", full.names=TRUE )
rcp85.70=stack(rcp85.70.list)

#great lakes current
gl.current.list=list.files(path="E:/BrokenHardDrive/postdoc/Bioclim/WorldClim/Current/tiff_gl_old",
                           pattern="tif$", full.names=TRUE)
gl.current=stack(gl.current.list)
glb<-readOGR("E:/BrokenHardDrive/postdoc/glin_gl_mainlakes/gl_mainlakes.shp")


#bring in coordinates of species of interest
#species<-list.files(path="E:/BrokenHardDrive/postdoc/analysis_files/sp_coords/corrected_coords",
#                    pattern="_corrected2.csv", full.names=TRUE)

#sp.coords<-read.csv(species[i], header=TRUE)
#filename<-sub(pattern = "(.*)\\..*$", replacement = "\\1",
#              basename(species[i]))
#filename<-sub(pattern="_", replacement =" ", 
#              basename(filename))
#filename<-sapply(strsplit(filename, "_corrected2"), "[[",1)  
#sp.coords<-sp.coords[,c("longitude","latitude")]
#projection(sp.coords)<-"+proj=longlat +ellps=WGS84 +datum=WGS84"


species<-list.files(path="E:/BrokenHardDrive/postdoc/analysis_files/training/global_training",
                    pattern="train_", full.names=TRUE)
for(i in 28:29)
  sp.coords.train<-read.csv(species[2], header=TRUE)
  filename<-sub(pattern = "(.*)\\..*$", replacement = "\\1",
              basename(species[2]))
  filename<-sapply(strsplit(filename, "train_"), "[[",-1)     
  form1=sprintf('E:/BrokenHardDrive/postdoc/analysis_files/testing/global_test/test_%s.csv', filename)
  sp.coords.test<-read.csv(file=form1)
#filename<-sub(pattern="_", replacement =" ", 
#                basename(filename))

sp.coords.train<-sp.coords.train[,c("Longitude","Latitude")]
sp.coords.test<-sp.coords.test[,c("Longitude","Latitude")]

pres_train<-sp.coords.train
pres_test<-sp.coords.test


#create background training and test data (in lieu of absence data)
  set.seed(10)
  form_bg_name<-sprintf('E:/BrokenHardDrive/postdoc/analysis_files/background_raster/%s.tif', filename)
  form_bg_name<-sprintf("/usr/lib64/microsoft-r/rserver/o16n/9.1.0/rserve/workdir/Rserv9.1.0/conn31083/Cherax quadricarinatus.tif")
  background<-raster(form_bg_name)
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
  coords <- raster.random.points(10000, background)
backg<-coords
colnames(backg)=c('Longitude','Latitude')
group=kfold(backg,5)
backg_train=backg[group!=1,]
backg_test=backg[group==1,]


#create a raster of rasterstack, showing just first layer and plot to check data
r=raster(current,1)
plot(!is.na(r), col=c('white','light grey'), legend=FALSE)
points(backg_train, pch='-', cex=0.5, col='blue')
points(backg_test, pch='+', cex=0.5, col='black')
points(pres_train, pch='+', cex=0.5, col='green')
points(pres_test, pch='+', cex=0.5,col='purple')

#create model
xm<-maxent(current,p=pres_train,
           a=backg_train,
           path='E:/BrokenHardDrive/postdoc/analysis_files/Maxent',
           args=c(
             'removeduplicates=TRUE',
             'outputformat=cumulative',
             'outputfiletype=grd',
            'replicates=5',
             'replicatetype=crossvalidate'
             ))

#use model on test data
xm.avg<-read.csv("E:/BrokenHardDrive/postdoc/analysis_files/Maxent/species_avg.csv")

e<-evaluate(model=xm, p=pres_test, a=backg_test, x=current)
e


px<-predict(gl.current, xm)
png(paste0("E:/postdoc/analysis_files/png_files/4550/4550_", filename, ".png"))
writeRaster(px,
            filename='E:/postdoc/M.guruns/m.foss.current.tif')

colfun<-colorRampPalette(
  c("blue","cyan","green","yellow","red"))
current.plot<-levelplot(px, 
                        main=paste(filename, "current"), 
                        xlim=c(-95,-70), ylim=c(40,52),
                        at=seq(0,1, length.out=1000),
                        col.regions=colfun,
                        margin=F,
                        maxpixels=13000000) +
  layer(sp.polygons(glb))
print(current.plot)
dev.off()
tr<-threshold(e, 'equal_sens_spec')
sensitivity<-sum(pres>=tr)/length(pres) 
#use the desired threshold value from previous step
specificity<-sum(abs<tr)/length(abs)
TSS<-sensitivity + specificity - 1
TSS

#RCP4.5 2050
rcp45.50.predict<-predict(rcp45.50, xm,
                          path='E:/postdoc/M.guruns/4550')
png(paste0("E:/postdoc/analysis_files/png_files/4550/4550_", filename, ".png"))
writeRaster(rcp45.50.predict,
            filename=paste0('E:/postdoc/analysis_files/raster_files/4550_', filename, '.tif'))
plot.4550<-levelplot(rcp45.50.predict, 
                     main=paste(filename, "RCP 4.5 2050"), 
                     xlim=c(-95,-70), ylim=c(40,52),
                     at=seq(0,1, length.out=1000),
                     col.regions=colfun,
                     margin=F,
                     maxpixels=13000000) +
  layer(sp.polygons(glb))
print(plot.4550)
dev.off()

#2470
rcp45.70.predict<-predict(rcp45.70, xm)
png(paste0("E:/postdoc/analysis_files/png_files/4570/4570_", filename, ".png"))
writeRaster(rcp45.70.predict,
            filename=paste0('E:/postdoc/analysis_files/raster_files/4570_', filename, '.tif'))
plot.4570<-levelplot(rcp45.70.predict, 
                     main=paste(filename, "RCP 4.5 2070"), 
                     xlim=c(-95,-70), ylim=c(40,52),
                     at=seq(0,1, length.out=1000),
                     col.regions=colfun,
                     margin=F,
                     maxpixels=15000000) +
  layer(sp.polygons(glb))
print(plot.4570)
dev.off()

#Predict for RCP 4.5. 2050
rcp85.50.predict<-predict(rcp85.50, xm,
                          path='E:/postdoc/M.guruns/output/8550')
png(paste0("E:/postdoc/analysis_files/png_files/8550/8550_2", filename, ".png"))
writeRaster(rcp85.50.predict,
            filename=paste0('E:/postdoc/analysis_files/raster_files/', filename, '.tif'))
plot.8550<-levelplot(rcp85.50.predict, 
                     main=paste(filename, "RCP 8.5 2050"), 
                     xlim=c(-95,-70), ylim=c(40,52),
                     at=seq(0,1, length.out=1000),
                     col.regions=colfun,
                     margin=F,
                     maxpixels=13000000) +
  layer(sp.polygons(glb))
print(plot.8550)
dev.off()
dev.off()



