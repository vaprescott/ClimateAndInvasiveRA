library("dismo", lib.loc="~/R/win-library/3.4")
library("gbm", lib.loc="~/R/win-library/3.4")
library("maps", lib.loc="~/R/win-library/3.4")
library("mapdata", lib.loc="~/R/win-library/3.4")
library("maptools", lib.loc="~/R/win-library/3.4")
library("rgdal", lib.loc="~/R/win-library/3.4")
#library("RPostgreSQL", lib.loc="~/R/win-library/3.4")
library("raster", lib.loc="~/R/win-library/3.4")
library("rasterVis", lib.loc="~/R/win-library/3.4")

#bring in tiff files for climate data and put them into one rasterstack
current.list=list.files(path="E:/postdoc/Bioclim/WorldClim/Current/tif_files", 
                        pattern="tif$", full.names=TRUE )
current=stack(current.list)
current<-is.raster(current)
#RCP 45 2050
rcp45.50.list=list.files(path="E:/postdoc/Bioclim/Mod_WorldClim/Modlayers_2050_45/mod_50_45_tiff_gl", 
                         pattern="tif$", full.names=TRUE )
rcp45.50=stack(rcp45.50.list)
#RCP 45 2070
rcp45.70.list=list.files(path="E:/postdoc/Bioclim/Mod_WorldClim/Modlayers_2070_45/mod_70_45_tiff_gl", 
                         pattern="tif$", full.names=TRUE )
rcp45.70=stack(rcp45.70.list)
#RCP85 2050
rcp85.50.list=list.files(path="E:/postdoc/Bioclim/Mod_WorldClim/Modlayers_2050_85/mod_50_85_tiff_gl", 
                         pattern="tif$", full.names=TRUE )
rcp85.50=stack(rcp85.50.list)
#RCP85 2070
rcp85.70.list=list.files(path="E:/postdoc/Bioclim/Mod_WorldClim/Modlayers_2070_85/mod_70_85_tiff_gl", 
                         pattern="tif$", full.names=TRUE )
rcp85.70=stack(rcp45.70.list)
#great lakes region
gl.current.list=list.files(path="E:/postdoc/Bioclim/WorldClim/Current/tiff_gl",
                           pattern="tif$", full.names=TRUE)
gl.current=stack(gl.current.list)
gl.current<-is.raster(gl.current)
glb<-readOGR("E:/postdoc/glin_gl_mainlakes/gl_mainlakes.shp")

#bring in coordinates of species of interest
species<-list.files(path="C:/Users/vprescott/Desktop/RAMP2/Full_coords",
                    pattern="csv", full.names=TRUE)

sp.coords<-read.csv(species[i], header=TRUE)
filename<-sub(pattern = "(.*)\\..*$", replacement = "\\1",
              basename(species[i]))
filename<-sub(pattern="_", replacement =" ", 
              basename(filename))
filename<-sapply(strsplit(filename, "_full2"), "[[",1)  
sp.coords<-sp.coords[,c("longitude","latitude")]
projection(sp.coords)<-"+proj=longlat +ellps=WGS84 +datum=WGS84"

#create presence training and test data
set.seed(0)
group=kfold(sp.coords, 5)
pres_train=sp.coords[group!=1,]
pres_test=sp.coords[group==1,]


#create background training and test data (in lieu of absence data)
set.seed(10)
bias3<-readOGR("E:/postdoc/analysis_files/background/misgu3_MCP/mis_poly_1.shp")
bias4<-raster('E:/postdoc/analysis_files/background/misgu3_MCP/misguruns_fossilis2.tif')
ext=extent(bias3)
backg=randomPoints(current, 
                   n=nrow(sp.coords),
                   ext=ext,
                   extf=1.25)
colnames(backg)=c('Longitude','Latitude')
group=kfold(backg,5)
#backg_train=backg[group!=1,]
backg_test=backg[group==1,]
View(backg_test)

back_points=randomPoints(current, 
                   n=10000,
                   ext=ext,
                   extf=1)


#create a raster of rasterstack, showing just first layer and plot to check data
r=raster(current,1)
plot(!is.na(r), col=c('white','light grey'), legend=FALSE)
points(backg_train, pch='-', cex=0.5, col='blue')
points(backg_test, pch='+', cex=0.5, col='black')
points(pres_train, pch='+', cex=0.5, col='green')
points(pres_test, pch='+', cex=0.5,col='purple')

#create model
xm<-maxent(current,p=pres_train,
           a=back_points,
           path='E:/postdoc/M.guruns/output',
           args=c(
             'removeduplicates=TRUE',
             'outputformat=cumulative',
             'outputfiletype=grd',
            'replicates=5',
             'replicatetype=crossvalidate'
             ))
 #use model on test data
xm.avg<-read.csv("E:/postdoc/M.guruns/output/species_avg.csv")

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



