remoteLogin("http://10.17.28.21:12800", session=TRUE, diff=TRUE, commandline = TRUE, username="admin", password = "Web4fun@luc")

install.packages(c("gbm","maps","mapdata","maptools","rgdal","raster","rasterVis"))
install.packages("dismo")

library("dismo")
library("gbm")
library("maps")
library("mapdata")
library("maptools")
library("rgdal")
library("raster")
library("rasterVis")

#bring in tiff files for climate data and put them into one rasterstack
current.list=list.files(path="C:/Program Files/Microsoft/ML Server/R_SERVER/o16n/rserve/workdir/conn5664/tif_files", 
                        pattern="tif$", full.names=TRUE )
current=stack(current.list)

#RCP 45 2050
rcp45.50.list=list.files(path="C:/Program Files/Microsoft/ML Server/R_SERVER/o16n/rserve/workdir/conn5664/mod_50_45_tiff_gl", 
                         pattern="tif$", full.names=TRUE )
rcp45.50=stack(rcp45.50.list)

#RCP 45 2070
rcp45.70.list=list.files(path="C:/Program Files/Microsoft/ML Server/R_SERVER/o16n/rserve/workdir/conn5664/mod_70_45_tiff_gl", 
                         pattern="tif$", full.names=TRUE )
rcp45.70=stack(rcp45.70.list)

#RCP85 2050
rcp85.50.list=list.files(path="C:/Program Files/Microsoft/ML Server/R_SERVER/o16n/rserve/workdir/conn5664/mod_50_85_tiff_gl", 
                         pattern="tif$", full.names=TRUE )
rcp85.50=stack(rcp85.50.list)

#RCP85 2070
rcp85.70.list=list.files(path="C:/Program Files/Microsoft/ML Server/R_SERVER/o16n/rserve/workdir/conn5664/mod_70_85_tiff_gl", 
                         pattern="tif$", full.names=TRUE )
rcp85.70=stack(rcp85.70.list)

#great lakes current
gl.current.list=list.files(path="C:/Program Files/Microsoft/ML Server/R_SERVER/o16n/rserve/workdir/conn5664/tiff_gl_old",
                           pattern="tif$", full.names=TRUE)

gl.current=stack(gl.current.list)
glb<-readOGR("C:/Program Files/Microsoft/ML Server/R_SERVER/o16n/rserve/workdir/conn5664/glin_gl_mainlakes/gl_mainlakes.shp")


#bring in coordinates of species of interest
species<-list.files(path="C:/Program Files/Microsoft/ML Server/R_SERVER/o16n/rserve/workdir/conn5664/global_training",
                    pattern="train_", full.names=TRUE)

for(i in 28:29)
  sp.coords.train<-read.csv(species[2], header=TRUE)
  filename<-sub(pattern = "(.*)\\..*$", replacement = "\\1",
              basename(species[2]))
  filename<-sapply(strsplit(filename, "train_"), "[[",-1)     
  form1=sprintf('C:/Program Files/Microsoft/ML Server/R_SERVER/o16n/rserve/workdir/conn5664/global_test/test_%s.csv', filename)
  sp.coords.test<-read.csv(file=form1)


  sp.coords.train<-sp.coords.train[,c("Longitude","Latitude")]
  sp.coords.test<-sp.coords.test[,c("Longitude","Latitude")]

  coordinates(sp.coords.train)<-~Longitude + Latitude
  coordinates(sp.coords.test)<-~Longitude + Latitude

  pres_train<-sp.coords.train
  pres_test<-sp.coords.test
  crs(pres_train)<-"+proj=longlat +datum=WGS84"
  crs(pres_test)<-"+proj=longlat +datum=WGS84"  
  
  set.seed(10)
  form_backg=sprintf("C:/Program Files/Microsoft/ML Server/R_SERVER/o16n/rserve/workdir/conn5664/background_raster/%s.tif", filename)
  #form_bg_name<-sprintf("/usr/lib64/microsoft-r/rserver/o16n/9.1.0/rserve/workdir/Rserv9.1.0/conn31083/Cherax quadricarinatus.tif")
  background<-raster(form_backg)
  ext=extent(background)
  crs(background)<-"+proj=longlat +datum=WGS84"

  
  background<-background>-Inf
  system.time(pp<-rasterToPolygons(background, dissolve=TRUE))
  
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
  system.time(coords <- raster.random.points(100, background))

