remoteLogin("http://10.17.28.21:12800", session=TRUE, diff=TRUE, commandline = TRUE, username="admin", password = "Web4fun@luc")

install.packages(c("gbm","maps","mapdata","maptools","rgdal","raster","rasterVis"))
install.packages("dismo")
install.packages("ClusterR")

library("dismo")
library("gbm")
library("maps")
library("mapdata")
library("maptools")
library("rgdal")
library("raster")
library("rasterVis")
library("ClusterR")
#pause()

#library("dismo")
#library("gbm")
#library("maps")
#library("mapdata")
#library("maptools")
#library("rgdal")
#library("raster")
#library("rasterVis")

#bring in tiff files for climate data and put them into one rasterstack
current.list=list.files(path="C:/Program Files/Microsoft/ML Server/R_SERVER/o16n/rserve/workdir/conn7684/tif_files", 
                        pattern="tif$", full.names=TRUE )
current=stack(current.list)

#RCP 45 2050
rcp45.50.list=list.files(path="C:/Program Files/Microsoft/ML Server/R_SERVER/o16n/rserve/workdir/conn7684/mod_50_45_tiff_gl", 
                         pattern="tif$", full.names=TRUE )
rcp45.50=stack(rcp45.50.list)

#RCP 45 2070
rcp45.70.list=list.files(path="C:/Program Files/Microsoft/ML Server/R_SERVER/o16n/rserve/workdir/conn7684/mod_70_45_tiff_gl", 
                         pattern="tif$", full.names=TRUE )
rcp45.70=stack(rcp45.70.list)

#RCP85 2050
rcp85.50.list=list.files(path="C:/Program Files/Microsoft/ML Server/R_SERVER/o16n/rserve/workdir/conn7684/mod_50_85_tiff_gl", 
                         pattern="tif$", full.names=TRUE )
rcp85.50=stack(rcp85.50.list)

#RCP85 2070
rcp85.70.list=list.files(path="C:/Program Files/Microsoft/ML Server/R_SERVER/o16n/rserve/workdir/conn7684/mod_70_85_tiff_gl", 
                         pattern="tif$", full.names=TRUE )
rcp85.70=stack(rcp85.70.list)

#great lakes current
gl.current.list=list.files(path="C:/Program Files/Microsoft/ML Server/R_SERVER/o16n/rserve/workdir/conn7684/tiff_gl_old",
                          pattern="tif$", full.names=TRUE)

gl.current=stack(gl.current.list)
glb<-readOGR("C:/Program Files/Microsoft/ML Server/R_SERVER/o16n/rserve/workdir/conn7684/glin_gl_mainlakes/gl_mainlakes.shp")


#bring in coordinates of species of interest
species<-list.files(path="C:/Program Files/Microsoft/ML Server/R_SERVER/o16n/rserve/workdir/conn7684/global_training",
                    pattern="train_", full.names=TRUE)

for(i in 1:length(species)){
  sp.coords.train<-read.csv(species[15], header=TRUE)
  filename<-sub(pattern = "(.*)\\..*$", replacement = "\\1",
                basename(species[15]))
  filename<-sapply(strsplit(filename, "train_"), "[[",-1)  
  #form1=sprintf('/usr/lib64/microsoft-r/rserver/o16n/9.1.0/rserve/workdir/Rserv9.1.0/conn28221/global_test/test_%s.csv', filename)
  form1=sprintf('C:/Program Files/Microsoft/ML Server/R_SERVER/o16n/rserve/workdir/conn7684/global_test/test_%s.csv', filename)
  sp.coords.test<-read.csv(file=form1)
  #filename<-sub(pattern="_", replacement =" ", 
   #               basename(filename))
  
  sp.coords.train<-sp.coords.train[,c("Longitude","Latitude")]
  sp.coords.test<-sp.coords.test[,c("Longitude","Latitude")]
  
  #get background training and testing data points
  #form_bg_train<-sprintf("/usr/lib64/microsoft-r/rserver/o16n/9.1.0/rserve/workdir/Rserv9.1.0/conn28221/train_background/%s.csv", filename)
  #backg_train<-read.csv(file=form_bg_train)
  #backg_train<-backg_train[,c("Longitude","Latitude")]
  #form_bg_test<-sprintf("/usr/lib64/microsoft-r/rserver/o16n/9.1.0/rserve/workdir/Rserv9.1.0/conn28221/test_background/%s.csv", filename)
  #backg_test<-read.csv(file=form_bg_test)
  #backg_test<-backg_test[,c("Longitude","Latitude")]
  
  form_bg_train<-sprintf("C:/Program Files/Microsoft/ML Server/R_SERVER/o16n/rserve/workdir/conn7684/train_background/%s.csv", filename)
  backg_train<-read.csv(file=form_bg_train)
  backg_train<-backg_train[,c("Longitude","Latitude")]
  form_bg_test<-sprintf("C:/Program Files/Microsoft/ML Server/R_SERVER/o16n/rserve/workdir/conn7684/test_background/%s.csv", filename)
  backg_test<-read.csv(file=form_bg_test)
  backg_test<-backg_test[,c("Longitude","Latitude")]
  
  
  #create data frame with presence and background training scores and environmental data
  #filename<-sub(pattern = "(.*)\\..*$", replacement = "\\1",
   #             basename(species[2]))
  #filename<-sapply(strsplit(filename, "train_"), "[[",-1) 
  backg_train_current<-extract(current, backg_train)
  #getRemoteObject("backg_train_current")
  pres_train_current<-extract(current, sp.coords.train)
  #getRemoteObject("pres_train_current")
  pres_backg_train<-c(rep(1,nrow(pres_train_current)), 
                      rep(0, nrow(backg_train_current)))
  train<-rbind(pres_train_current, backg_train_current)
  envtrain<-data.frame(cbind(pa=pres_backg_train, train))
  
  #do the same for test data
  backg_test_current<-extract(current, backg_test)
  pres_test_current<-extract(current, sp.coords.test)
  #getRemoteObject(c("backg_test_current", "pres_test_current"))
  pres_backg_test<-c(rep(1,nrow(pres_test_current)), 
                     rep(0, nrow(backg_test_current)))
  test<-rbind(pres_test_current, backg_test_current)
  envtest<-data.frame(cbind(pa=pres_backg_test, test))
  
  #run a model, lower the learning rate if you get the algorithm warning
  sp.tc5.lr01.train<-gbm.step(data=envtrain, gbm.x=2:20, gbm.y=1,
                              family='bernoulli', tree.complexity = 5,
                              learning.rate=0.01, bag.fraction=0.5,
                              n.folds=5)
 
  ##TRY TO PLOT SP.TC5.LR01.TRAIN
  
  #determine best number of trees
  if (sp.tc5.lr01.train$gbm.call$best.trees > 1000) {
    
  } else {
    sp.tc5.lr01.train<-gbm.step(data=envtrain, gbm.x=2:20, gbm.y=1,
                                family="bernoulli", tree.complexity = 5,
                                learning.rate=0.005, bag.fraction=0.5,
                                n.folds=5)}
  
  if (sp.tc5.lr01.train$gbm.call$best.trees < 3000) {
    
  } else {
    sp.tc5.lr01.train<-gbm.step(data=envtrain, gbm.x=2:20, gbm.y=1,
                                family="bernoulli", tree.complexity = 5,
                                learning.rate=0.02, bag.fraction=0.5,
                                n.folds=5)}
  

 predict_test<-predict(sp.tc5.lr01.train,envtest, 
                       n.trees=sp.tc5.lr01.train$gbm.call$best.trees,
                       type='response')

  
  #calc.deviance(obs=envtest$pa, pred=predict_test, calc.mean = TRUE)
  d<-cbind(envtest$pa, predict_test)
  pres<-d[d[,1]==1,2]
  abs<-d[d[,1]==0,2]
  e<-evaluate(p=pres, a=abs)
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
              file="C:/Program Files/Microsoft/ML Server/R_SERVER/o16n/rserve/workdir/conn7684/TSS.csv",
              append=T, sep=",", row.names=F, col.names = F)
  sensitivity<-data.frame(sensitivity)
  sensitivity$sp<-filename
  write.table(sensitivity,
              file="C:/Program Files/Microsoft/ML Server/R_SERVER/o16n/rserve/workdir/conn7684/sensitivity.csv",
              append=T, sep=",", row.names=F, col.names = F)
  specificity<-data.frame(specificity)
  specificity$sp<-filename
  write.table(specificity,
              file="C:/Program Files/Microsoft/ML Server/R_SERVER/o16n/rserve/workdir/conn7684/specificity.csv",
              append=T, sep=",", row.names=F, col.names = F)
  tr<-data.frame(tr)
  tr$sp<-filename
  write.table(tr,
              file="C:/Program Files/Microsoft/ML Server/R_SERVER/o16n/rserve/workdir/conn7684/threshold.csv",
              append=T, sep=",", row.names=F, col.names = F)
  auc<-data.frame(e@auc)
  auc$sp<-filename
  write.table(auc,
              file="C:/Program Files/Microsoft/ML Server/R_SERVER/o16n/rserve/workdir/conn7684/auc.csv",
              append=T, sep=",", row.names=F, col.names = F)
  trees<-sp.tc5.lr01.train$gbm.call$best.trees 
  trees$sp<-filename
  write.table(trees,
              file="C:/Program Files/Microsoft/ML Server/R_SERVER/o16n/rserve/workdir/conn7684/trees.csv",
              append=T, sep=",", row.names=F, col.names = F)
 

#save a png of the plot
  beginCluster(2)
  current.predict<-clusterR(gl.current, raster::predict, args=list(model=sp.tc5.lr01.train, n.trees=sp.tc5.lr01.train$gbm.call$best.trees,
                                                                   type='response'))
  endCluster()
  current.predict<-predict(gl.current, sp.tc5.lr01.train,
                           n.trees=sp.tc5.lr01.train$gbm.call$best.trees,
                           type='response')
                
  colfun<-colorRampPalette(
  c("blue","cyan","green","yellow","red"))
  png(paste0("C:/Program Files/Microsoft/ML Server/R_SERVER/o16n/rserve/workdir/conn4868/current/current_", filename, ".png"))
  current.plot<-levelplot(current.predict, 
                          main=paste(filename, "current"), 
                          xlim=c(-95,-70), ylim=c(40,52),
                          at=seq(0,1, length.out=1100),
                          col.regions=colfun,
                          margin=F,
                          maxpixels=13000000) +
    layer(sp.polygons(glb))
  print(current.plot)
  dev.off()
  
  #Once again, the next step takes hours to run
  rcp45.50.predict<-predict(rcp45.50,sp.tc5.lr01.train,
                            n.trees=sp.tc5.lr01.train$gbm.call$best.trees,
                            type='response')
  
  png(paste0("C:/Program Files/Microsoft/ML Server/R_SERVER/o16n/rserve/workdir/conn4868/png_files/4550/4550_", filename, ".png"))
  plot.4550<-levelplot(rcp45.50.predict, 
                       main=paste(filename, "RCP 4.5 2050"), 
                       xlim=c(-95,-70), ylim=c(40,52),
                       at=seq(0,1, length.out=1100),
                       col.regions=colfun,
                       margin=F,
                       maxpixels=15000000) +
    layer(sp.polygons(glb))
  print(plot.4550)
  dev.off()
  
  
  rcp45.70.predict<-predict(rcp45.70, sp.tc5.lr01.train,
                            n.trees=sp.tc5.lr01.train$gbm.call$best.trees,
                            type="response")
  
  png(paste0("C:/Program Files/Microsoft/ML Server/R_SERVER/o16n/rserve/workdir/conn4868/png_files/4570/4570_", filename, ".png"))
  plot.4570<-levelplot(rcp45.70.predict, 
                       main=paste(filename, "RCP 4.5 2070"), 
                       xlim=c(-95,-70), ylim=c(40,52),
                       at=seq(0,1, length.out=1100),
                       col.regions=colfun,
                       margin=F,
                       maxpixels=15000000) +
    layer(sp.polygons(glb))
  print(plot.4570)
  dev.off()
  
  
  rcp85.50.predict<-predict(rcp85.50, sp.tc5.lr01.train,
                            n.trees=sp.tc5.lr01.train$gbm.call$best.trees,
                            type="response")
  
  png(paste0("C:/Program Files/Microsoft/ML Server/R_SERVER/o16n/rserve/workdir/conn4868/png_files/8550/8550_", filename, ".png"))
  plot.8550<-levelplot(rcp85.50.predict, 
                       main=paste(filename, "RCP 8.5 2050"), 
                       xlim=c(-95,-70), ylim=c(40,52),
                       at=seq(0,1, length.out=1100),
                       col.regions=colfun,
                       margin=F,
                       maxpixels=15000000) +
    layer(sp.polygons(glb))
  print(plot.8550)
  dev.off()
  
  
  rcp85.70.predict<-predict(rcp85.70, sp.tc5.lr01.train,
                            n.trees=sp.tc5.lr01.train$gbm.call$best.trees,
                            type="response")
  
  png(paste0("C:/Program Files/Microsoft/ML Server/R_SERVER/o16n/rserve/workdir/conn4868/png_files/8570/8570_", filename, ".png"))
  plot.8570<-levelplot(rcp85.70.predict, 
                       main=paste(filename, "RCP 8.5 2070"), 
                       xlim=c(-95,-70), ylim=c(40,52),
                       at=seq(0,1, length.out=1100),
                       col.regions=colfun,
                       margin=F,
                       maxpixels=15000000) +
    layer(sp.polygons(glb))
  print(plot.8570)
  dev.off()
}

#create animation of 2050 and 2070 
test<-stack(current.predict,rcp45.50.predict, rcp85.50.predict, rcp85.70.predict)
animation<-animate(test, pause=2, n=100, 
                   xlim=c(-95,-70), 
                   ylim=c(40,52),
                   col=colors,
                   breaks=breakpoints,
                   maxpixels=15000000)
