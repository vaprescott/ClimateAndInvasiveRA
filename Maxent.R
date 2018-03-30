library("dismo")
library("gbm")
library("maps")
library("mapdata")
library("maptools")
library("rgdal")
library("raster")
library("rasterVis")
library("ClusterR")


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

species<-list.files(path="E:/BrokenHardDrive/postdoc/analysis_files/training/global_training",
                    pattern="train_", full.names=TRUE)

for(i in 26:27){
  sp.coords.train<-read.csv(species[30], header=TRUE)
  filename<-sub(pattern = "(.*)\\..*$", replacement = "\\1",
                basename(species[15]))
  filename<-sapply(strsplit(filename, "train_"), "[[",-1)        
  form1=sprintf('E:/BrokenHardDrive/postdoc/analysis_files/testing/global_test/test_%s.csv', filename)
  sp.coords.test<-read.csv(file=form1)


  sp.coords.train<-sp.coords.train[,c("Longitude","Latitude")]
  sp.coords.test<-sp.coords.test[,c("Longitude","Latitude")]

  coordinates(sp.coords.train)<-~Longitude + Latitude
  coordinates(sp.coords.test)<-~Longitude + Latitude

  pres_train<-sp.coords.train
  pres_test<-sp.coords.test
  crs(pres_train)<-"+proj=longlat +datum=WGS84"
  crs(pres_test)<-"+proj=longlat +datum=WGS84"  

  form_backg=sprintf("E:/BrokenHardDrive/postdoc/analysis_files/background_raster/%s.tif", filename)
  background<-raster(form_backg)
  crs(background)<-"+proj=longlat +datum=WGS84"
  
  sp_backg<-as(background, 'SpatialPointsDataFrame')
  coords<-spsample(sp_backg, n=1000000, "random")
  coords_extract<-extract(background, coords)
  df2_coords<-as.data.frame(coords)
  coord_test<-data.frame(cbind(coords_extract, df2_coords))
  coords_final<-na.omit(coord_test)
  colnames(coords_final)[2]<-"Longitude"
  colnames(coords_final)[3]<-"Latitude"
  coords_final2<-coords_final[,-1]
  cf<-coords_final2[sample(nrow(coords_final2),10000) , ]
  
  backg<-cf
  group=kfold(backg,5)
  backg_train=backg[group!=1,]
  backg_test=backg[group==1,]

#create a raster of rasterstack, showing just first layer and plot to check data
#r=raster(current,1)
#plot(!is.na(r), col=c('white','light grey'), legend=FALSE)
#points(backg_train, pch='-', cex=0.5, col='blue')
#points(backg_test, pch='+', cex=0.5, col='black')
#points(pres_train, pch='+', cex=0.5, col='green')
#points(pres_test, pch='+', cex=0.5,col='purple')

  path=sprintf("E:/BrokenHardDrive/postdoc/analysis_files/Maxent/%s", filename)
  
  #create model
  xm<-maxent(current, p=pres_train,
             a=backg_train,
             path=path,
             agrs='outputformat=cloglog')

  backg_test_current<-extract(current, backg_test)
  pres_test_current<-extract(current, sp.coords.test)
  pres_test_current2<-na.omit(pres_test_current)
  pvtest<-predict(xm, pres_test_current2)
  bgtest<-predict(xm, backg_test_current)
  e2<-evaluate(p=pvtest, a=bgtest)

  tr<-threshold(e2, "equal_sens_spec")
  sensitivity<-sum(pvtest>=tr)/length(pvtest) 
  #use the desired threshold value from previous step
  specificity<-sum(bgtest<tr)/length(bgtest)
  TSS<-sensitivity + specificity - 1
  
  TSS<-data.frame(TSS)
  TSS$sp<-filename
  write.table(TSS,
              file="E:/BrokenHardDrive/postdoc/analysis_files/Maxent_TSS/TSS.csv",
              append=T, sep=",", row.names=F, col.names = F)
  sensitivity<-data.frame(sensitivity)
  sensitivity$sp<-filename
  write.table(sensitivity,
              file="E:/BrokenHardDrive/postdoc/analysis_files/Maxent_sensitivity/sensitivity.csv",
              append=T, sep=",", row.names=F, col.names = F)
  specificity<-data.frame(specificity)
  specificity$sp<-filename
  write.table(specificity,
              file="E:/BrokenHardDrive/postdoc/analysis_files/Maxent_specificity/Specificity.csv",
              append=T, sep=",", row.names=F, col.names = F)
  tr<-data.frame(tr)
  tr$sp<-filename
  write.table(tr,
              file="E:/BrokenHardDrive/postdoc/analysis_files/Maxent_threshold/Threshold.csv",
              append=T, sep=",", row.names=F, col.names = F)
  auc<-data.frame(e2@auc)
  auc$sp<-filename
  write.table(auc,
              file="E:/BrokenHardDrive/postdoc/analysis_files/Maxent_auc/auc.csv",
              append=T, sep=",", row.names=F, col.names = F)
  

 #current.predict<-predict(gl.current, xm)
  beginCluster(2)
  system.time(current.predict<-clusterR(gl.current, predict, args=list(model=xm), progress='text'))
  endCluster()
  colfun<-colorRampPalette(
    c("blue","cyan","green","yellow","red"))
 png(paste0("E:/BrokenHardDrive/postdoc/analysis_files/png_files/Maxent/current/current_", filename, ".png"))
 current.plot<-levelplot(current.predict, 
                         main=paste(filename, "current"), 
                         xlim=c(-95,-70), ylim=c(40,52),
                         at=seq(0,1, length.out=1000),
                         col.regions=colfun,
                         margin=F,
                         maxpixels=15000000) +
  layer(sp.polygons(glb))
 print(current.plot)
 dev.off() 

#RCP4.5 2050
 #rcp45.50.predict<-predict(rcp45.50, xm, path=path)
 beginCluster(2)
 system.time(rcp45.50.predict<-clusterR(rcp45.50, predict, args=list(model=xm), progress='text'))
 endCluster()
 png(paste0("E:/BrokenHardDrive/postdoc/analysis_files/png_files/Maxent/4550/4550_", filename, ".png"))
 plot.4550<-levelplot(rcp45.50.predict, 
                      main=paste(filename, "RCP 4.5 2050"), 
                      xlim=c(-95,-70), ylim=c(40,52),
                      at=seq(0,1, length.out=1000),
                      col.regions=colfun,
                      margin=F,
                      maxpixels=15000000) +
   layer(sp.polygons(glb))
 print(plot.4550)
 dev.off()
 
 remove(current.predict,rcp45.50.predict, current.plot, plot.4550)
 removeTmpFiles(h=.5)
 gc()
 
 #4570
# rcp45.70.predict<-predict(rcp45.70, xm, path=path)
 beginCluster(2)
 system.time(rcp45.70.predict<-clusterR(rcp45.70, predict, args=list(model=xm), progress='text'))
 endCluster()
 png(paste0("E:/BrokenHardDrive/postdoc/analysis_files/png_files/Maxent/4570/4570_", filename, ".png"))
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
 
 remove(rcp45.70.predict, plot.4570)
 removeTmpFiles(h=.5)
 gc()
 
 #8550
 #rcp85.50.predict<-predict(rcp85.50, xm, path=path)
 beginCluster(2)
 system.time(rcp85.50.predict<-clusterR(rcp85.50, predict, args=list(model=xm), progress='text'))
 endCluster()
 png(paste0("E:/BrokenHardDrive/postdoc/analysis_files/png_files/Maxent/8550/8550_", filename, ".png"))
 plot.8550<-levelplot(rcp85.50.predict, 
                      main=paste(filename, "RCP 8.5 2050"), 
                      xlim=c(-95,-70), ylim=c(40,52),
                      at=seq(0,1, length.out=1000),
                      col.regions=colfun,
                      margin=F,
                      maxpixels=15000000) +
   layer(sp.polygons(glb))
 print(plot.8550)
 dev.off()
 
 #8570
 #rcp85.70.predict<-predict(rcp85.70, xm)
 beginCluster(2)
 system.time(rcp85.70.predict<-clusterR(rcp85.70, predict, args=list(model=xm), progress='text'))
 endCluster()
 png(paste0("E:/BrokenHardDrive/postdoc/analysis_files/png_files/Maxent/8570/8570_", filename, ".png"))
 plot.8570<-levelplot(rcp85.70.predict, 
                      main=paste(filename, "RCP 8.5 2070"), 
                      xlim=c(-95,-70), ylim=c(40,52),
                      at=seq(0,1, length.out=1000),
                      col.regions=colfun,
                      margin=F,
                      maxpixels=15000000) +
   layer(sp.polygons(glb))
 print(plot.8570)
 dev.off()
 
 remove(rcp85.70.predict, rcp85.50.predict, plot.8550, plot.8570)
 removeTmpFiles(h=.5)
 gc()
}


#create background training and test data (in lieu of absence data)
#set.seed(10)
#form_bg_name<-sprintf('E:/BrokenHardDrive/postdoc/analysis_files/background_raster/%s.tif', filename)
#form_bg_name<-sprintf("/usr/lib64/microsoft-r/rserver/o16n/9.1.0/rserve/workdir/Rserv9.1.0/conn31083/Cherax quadricarinatus.tif")
#background<-raster(form_bg_name)
#ext=extent(background)
#crs(background)<-"+proj=longlat +datum=WGS84"
#raster.random.points<-function(size, background, na.rm=TRUE){
#  coords<-matrix(0, nrow=size, ncol=2)
#  coords[,1]<-runif(size, xmin(background), xmax(background))
##  coords[,2] <- runif(size, ymin(background), ymax(background))
#  if (na.rm) {
#    cells <- cellFromXY(background, coords)
#    na.cnt <- length(which(is.na(background[cells])))
#    while (na.cnt > 0){
#      recs <- which(is.na(background[cells]))
#      coords[recs,1] <- runif(length(recs), xmin(background), xmax(background))
#      coords[recs,2] <- runif(length(recs), ymin(background), ymax(background))
##      cells <- cellFromXY(background, coords)
#      na.cnt <- length(which(is.na(background[cells])))
 #   }}
#  return(coords)
# now call the function to generate random points
#coords <- raster.random.points(10000, background)
#backg<-coords


