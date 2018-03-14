remoteLogin("http://10.17.28.21:12800", session=TRUE, diff=TRUE, commandline = TRUE, username="admin", password = "Web4fun@luc")

install.packages(c("rJava", "gbm","maps","mapdata","maptools","rgdal","raster","rasterVis", "sp"))
install.packages("dismo")

library("dismo")
library("gbm")
library("maps")
library("mapdata")
library("maptools")
library("rgdal")
library("raster")
library("rasterVis")
library("sp")
library("rJava")

#bring in tiff files for climate data and put them into one rasterstack
current.list=list.files(path="C:/Program Files/Microsoft/ML Server/R_SERVER/o16n/rserve/workdir/conn3964/tif_files", 
                        pattern="tif$", full.names=TRUE )
current=stack(current.list)

#RCP 45 2050
rcp45.50.list=list.files(path="C:/Program Files/Microsoft/ML Server/R_SERVER/o16n/rserve/workdir/conn3964/mod_50_45_tiff_gl", 
                         pattern="tif$", full.names=TRUE )
rcp45.50=stack(rcp45.50.list)

#RCP 45 2070
rcp45.70.list=list.files(path="C:/Program Files/Microsoft/ML Server/R_SERVER/o16n/rserve/workdir/conn3964/mod_70_45_tiff_gl", 
                         pattern="tif$", full.names=TRUE )
rcp45.70=stack(rcp45.70.list)

#RCP85 2050
rcp85.50.list=list.files(path="C:/Program Files/Microsoft/ML Server/R_SERVER/o16n/rserve/workdir/conn3964/mod_50_85_tiff_gl", 
                         pattern="tif$", full.names=TRUE )
rcp85.50=stack(rcp85.50.list)

#RCP85 2070
rcp85.70.list=list.files(path="C:/Program Files/Microsoft/ML Server/R_SERVER/o16n/rserve/workdir/conn3964/mod_70_85_tiff_gl", 
                         pattern="tif$", full.names=TRUE )
rcp85.70=stack(rcp85.70.list)

#great lakes current
gl.current.list=list.files(path="C:/Program Files/Microsoft/ML Server/R_SERVER/o16n/rserve/workdir/conn3964/tiff_gl_old",
                           pattern="tif$", full.names=TRUE)

gl.current=stack(gl.current.list)
glb<-readOGR("C:/Program Files/Microsoft/ML Server/R_SERVER/o16n/rserve/workdir/conn3964/glin_gl_mainlakes/gl_mainlakes.shp")


#bring in coordinates of species of interest
species<-list.files(path="C:/Program Files/Microsoft/ML Server/R_SERVER/o16n/rserve/workdir/conn3964/global_training",
                    pattern="train_", full.names=TRUE)

for(i in 28:29)
  sp.coords.train<-read.csv(species[2], header=TRUE)
  filename<-sub(pattern = "(.*)\\..*$", replacement = "\\1",
              basename(species[2]))
  filename<-sapply(strsplit(filename, "train_"), "[[",-1)     
  form1=sprintf('C:/Program Files/Microsoft/ML Server/R_SERVER/o16n/rserve/workdir/conn3964/global_test/test_%s.csv', filename)
  sp.coords.test<-read.csv(file=form1)


  sp.coords.train<-sp.coords.train[,c("Longitude","Latitude")]
  sp.coords.test<-sp.coords.test[,c("Longitude","Latitude")]

  coordinates(sp.coords.train)<-~Longitude + Latitude
  coordinates(sp.coords.test)<-~Longitude + Latitude

  pres_train<-sp.coords.train
  pres_test<-sp.coords.test
  crs(pres_train)<-"+proj=longlat +datum=WGS84"
  crs(pres_test)<-"+proj=longlat +datum=WGS84"  
  
  #set.seed(10)
  form_backg=sprintf("C:/Program Files/Microsoft/ML Server/R_SERVER/o16n/rserve/workdir/conn3964/background_raster/%s.tif", filename)
  #form_bg_name<-sprintf("/usr/lib64/microsoft-r/rserver/o16n/9.1.0/rserve/workdir/Rserv9.1.0/conn31083/Cherax quadricarinatus.tif")
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
  coordinates(backg_train)<-~Longitude + Latitude
  coordinates(backg_test)<-~Longitude +Latitude
  
  #backg_test_current<-extract(current, backg_test)
  #pres_test_current<-extract(current, sp.coords.test)
  #test<-rbind(pres_test_current, backg_test_current)
  path=sprintf("C:/Program Files/Microsoft/ML Server/R_SERVER/o16n/rserve/workdir/conn3964/Maxent/%s", filename)
  
  
  
  backg_train_current<-extract(current, backg_train)
  pres_train_current<-extract(current, sp.coords.train)
  pres_backg_train<-c(rep(1,nrow(pres_train_current)), 
                      rep(0, nrow(backg_train_current)))
  train<-rbind(pres_train_current, backg_train_current)
  envtrain<-data.frame(cbind(pa=pres_backg_train, train))
  
  
  xm_swd<-maxent(current, p=pres_train_current,
             a=backg_train_current,
             path=path,
             agrs='outputformat=cloglog')
  

  
 e<-evaluate(model=xm, p=pres_test, a=backg_test, x=current)
 e
 threshold(e)
 tr<-threshold(e, "equal_sens_spec")
 
 sensitivity<-sum(pres>=tr)/length(pres) 
 #use the desired threshold value from previous step
 specificity<-sum(abs<tr)/length(abs)
 
 TSS<-sensitivity + specificity - 1
 
 
 TSS<-data.frame(TSS)
 TSS$sp<-filename
 write.table(TSS,
             file="E:/BrokenHardDrive/postdoc/analysis_files/BRT_TSS/TSS.csv",
             append=T, sep=",", row.names=F, col.names = F)
 sensitivity<-data.frame(sensitivity)
 sensitivity$sp<-filename
 write.table(sensitivity,
             file="E:/BrokenHardDrive/postdoc/analysis_files/BRT_sensitivity/sensitivity.csv",
             append=T, sep=",", row.names=F, col.names = F)
 specificity<-data.frame(specificity)
 specificity$sp<-filename
 write.table(specificity,
             file="E:/BrokenHardDrive/postdoc/analysis_files/BRT_specificity/specificity.csv",
             append=T, sep=",", row.names=F, col.names = F)
 tr<-data.frame(tr)
 tr$sp<-filename
 write.table(tr,
             file="E:/BrokenHardDrive/postdoc/analysis_files/BRT_threshold/threshold.csv",
             append=T, sep=",", row.names=F, col.names = F)
 auc<-data.frame(e@auc)
 auc$sp<-filename
 
