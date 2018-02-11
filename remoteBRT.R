remoteLogin("http://10.17.28.65:12800", session=TRUE, diff=TRUE, commandline = TRUE, username="admin", password = "Web4fun@luc") 

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
pause()

library("dismo")
library("gbm")
library("maps")
library("mapdata")
library("maptools")
library("rgdal")
library("raster")
library("rasterVis")

#bring in tiff files for climate data and put them into one rasterstack
current.list=list.files(path="E:/BrokenHardDrive/postdoc/Bioclim/WorldClim/Current/wc2.0_2.5m_bio", 
                        pattern="tif$", full.names=TRUE )
current=stack(current.list)

resume()
#bring in tiff files for climate data and put them into one rasterstack
current.list=list.files(path="/usr/lib64/microsoft-r/rserver/o16n/9.1.0/rserve/workdir/Rserv9.1.0/conn31083/wc2.0_2.5m_bio", 
                        pattern="tif$", full.names=TRUE )
current=stack(current.list)

#RCP 45 2050
#rcp45.50.list=list.files(path="/usr/lib64/microsoft-r/rserver/o16n/9.1.0/rserve/workdir/Rserv9.1.0/conn41297/mod_50_45_tiff_gl", 
#                         pattern="tif$", full.names=TRUE )
#rcp45.50=stack(rcp45.50.list)

#RCP 45 2070
#rcp45.70.list=list.files(path="/usr/lib64/microsoft-r/rserver/o16n/9.1.0/rserve/workdir/Rserv9.1.0/conn41297/mod_70_45_tiff_gl", 
#                         pattern="tif$", full.names=TRUE )
#rcp45.70=stack(rcp45.70.list)

#RCP85 2050
#rcp85.50.list=list.files(path="/usr/lib64/microsoft-r/rserver/o16n/9.1.0/rserve/workdir/Rserv9.1.0/conn41297/mod_50_85_tiff_gl", 
#                         pattern="tif$", full.names=TRUE )
#rcp85.50=stack(rcp85.50.list)

#RCP85 2070
#rcp85.70.list=list.files(path="/usr/lib64/microsoft-r/rserver/o16n/9.1.0/rserve/workdir/Rserv9.1.0/conn41297/mod_70_85_tiff_gl", 
#                         pattern="tif$", full.names=TRUE )
#rcp85.70=stack(rcp85.70.list)

#great lakes current
gl.current.list=list.files(path="/usr/lib64/microsoft-r/rserver/o16n/9.1.0/rserve/workdir/Rserv9.1.0/conn31083/tiff_gl_old",
                          pattern="tif$", full.names=TRUE)

gl.current=stack(gl.current.list)
glb<-readOGR("/usr/lib64/microsoft-r/rserver/o16n/9.1.0/rserve/workdir/Rserv9.1.0/conn31083/glin_gl_mainlakes/gl_mainlakes.shp")


#bring in coordinates of species of interest
#species<-list.files(path="/usr/lib64/microsoft-r/rserver/o16n/9.1.0/rserve/workdir/Rserv9.1.0/conn31083/global_training",
#                    pattern="train_", full.names=TRUE)


#my.data<-lapply(species, read.csv, header=TRUE)
#for(i in 1:length(species)){
 # sp.coords.train<-read.csv(species[29], header=TRUE)
  #my.name<-names(sp.coords.train)[1]
#  filename<-sub(pattern = "(.*)\\..*$", replacement = "\\1",
#                basename(species[29]))
#  filename<-sapply(strsplit(filename, "train_"), "[[",-1) 
#  sp.coords.train<-sp.coords.train[,c("Longitude","Latitude")]
#  assign(paste(filename),sp.coords.train)
#  form1=sprintf('/usr/lib64/microsoft-r/rserver/o16n/9.1.0/rserve/workdir/Rserv9.1.0/conn28221/global_test/test_%s.csv', filename)
#  sp.coords.test<-read.csv(file=form1)
#  sp.coords.test<-sp.coords.test[,c("Longitude","Latitude")]
#  assign(paste(filename, "_test"),sp.coords.test)
#  form_bg_train<-sprintf("/usr/lib64/microsoft-r/rserver/o16n/9.1.0/rserve/workdir/Rserv9.1.0/conn28221/train_background/%s.csv", filename)
#  backg_train<-read.csv(file=form_bg_train)
##  backg_train<-backg_train[,c("Longitude","Latitude")]
#  assign(paste(filename,"_bg_train"), backg_train)
#  form_bg_test<-sprintf("/usr/lib64/microsoft-r/rserver/o16n/9.1.0/rserve/workdir/Rserv9.1.0/conn28221/test_background/%s.csv", filename)
#  backg_test<-read.csv(file=form_bg_test)
#  backg_test<-backg_test[,c("Longitude","Latitude")]
#  assign(paste(filename,"_bg_test"), backg_test)
#  }

putLocalWorkspace()
for(i in 1:length(species)){
  sp.coords.train<-read.csv(species[29], header=TRUE)
  filename<-sub(pattern = "(.*)\\..*$", replacement = "\\1",
                basename(species[29]))
  filename<-sapply(strsplit(filename, "train_"), "[[",-1)  
  #form1=sprintf('/usr/lib64/microsoft-r/rserver/o16n/9.1.0/rserve/workdir/Rserv9.1.0/conn28221/global_test/test_%s.csv', filename)
  form1=sprintf('E:/BrokenHardDrive/postdoc/analysis_files/testing/global_test/test_%s.csv', filename)
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
  
  form_bg_train<-sprintf("E:/BrokenHardDrive/postdoc/analysis_files/train_background/%s.csv", filename)
  backg_train<-read.csv(file=form_bg_train)
  backg_train<-backg_train[,c("Longitude","Latitude")]
  form_bg_test<-sprintf("E:/BrokenHardDrive/postdoc/analysis_files/test_background/%s.csv", filename)
  backg_test<-read.csv(file=form_bg_test)
  backg_test<-backg_test[,c("Longitude","Latitude")]
  
  
  #create data frame with presence and background training scores and environmental data
  #filename<-sub(pattern = "(.*)\\..*$", replacement = "\\1",
   #             basename(species[2]))
  #filename<-sapply(strsplit(filename, "train_"), "[[",-1) 
  remoteExecute("backg_train_current<-extract(current, backg_train)")
  getRemoteObject("backg_train_current")
  remoteExecute("pres_train_current<-extract(current, sp.coords.train)")
  getRemoteObject("pres_train_current")
  pres_backg_train<-c(rep(1,nrow(pres_train_current)), 
                      rep(0, nrow(backg_train_current)))
  train<-rbind(pres_train_current, backg_train_current)
  envtrain<-data.frame(cbind(pa=pres_backg_train, train))
  
  #do the same for test data
  remoteExecute("backg_test_current<-extract(current, backg_test)
  pres_test_current<-extract(current, sp.coords.test)")
  getRemoteObject(c("backg_test_current", "pres_test_current"))
  pres_backg_test<-c(rep(1,nrow(pres_test_current)), 
                     rep(0, nrow(backg_test_current)))
  test<-rbind(pres_test_current, backg_test_current)
  envtest<-data.frame(cbind(pa=pres_backg_test, test))
  
  #run a model, lower the learning rate if you get the algorithm warning
  sp.tc5.lr01.train<-gbm.step(data=envtrain, gbm.x=2:20, gbm.y=1,
                              family='bernoulli', tree.complexity = 5,
                              learning.rate=0.015, bag.fraction=0.5,
                              n.folds=5)
 
  ##TRY TO PLOT SP.TC5.LR01.TRAIN
  
  #determine best number of trees
  if (sp.tc5.lr01.train$gbm.call$best.trees > 1000) {
    
  } else {
    sp.tc5.lr01.train<-gbm.step(data=envtrain, gbm.x=2:20, gbm.y=1,
                                family="bernoulli", tree.complexity = 5,
                                learning.rate=0.005, bag.fraction=0.5,
                                n.folds=5)}
  getRemoteObject("sp.tc5.lr01.train")
  #determine how well the test data does with this model
#  putLocalObject("sp.tc5.lr01.train")
#  putLocalObject("envtest")
 #getRemoteObject("sp.tc5.lr01.train") 
  remoteExecute("predict_test<-predict(sp.tc5.lr01.train,envtest, 
                       n.trees=sp.tc5.lr01.train$gbm.call$best.trees, type='response'
                )", writePlots=TRUE, displayPlots=TRUE, recPlots=TRUE)
#predict_test=rxPredict(sp.tc5.lr01.train,envtest, 
 #                      n.trees=sp.tc5.lr01.train$gbm.call$best.trees, 
  #                     type='response')
  #colfun<-colorRampPalette(
   # c("blue","cyan","green","yellow","red"))
  #png(paste0("E:/BrokenHardDrive/postdoc/analysis_files/png_files/current_global/current_global_", filename, ".png"))
  #current.plot<-levelplot(sp.tc5.lr01.train, 
  ##                        main=paste(filename, "global model"), 
  #                        xlim=c(-95,-70), ylim=c(40,52),
  #                        at=seq(0,1, length.out=1000),
  #                        col.regions=colfun,
  #                        margin=F,
  #                        maxpixels=13000000)
  
  #print(current.plot)
  #dev.off()
  
  ##TRY TO PLOT PREDICT_TEST
  getRemoteObject("predict_test")
  calc.deviance(obs=envtest$pa, pred=predict_test, calc.mean = TRUE)
  d<-cbind(envtest$pa, predict_test)
  pres<-d[d[,1]==1,2]
  abs<-d[d[,1]==0,2]
  e<-evaluate(p=pres, a=abs)
  e
  threshold(e)
  tr<-threshold(e, "equal_sens_spec")
  #form=sprintf('E:/postdoc/analysis_files/threshold/threshold_%s.csv',filename)
  #write.csv(tr,
  #          file=form)
  sensitivity<-sum(pres>=tr)/length(pres) 
  #use the desired threshold value from previous step
  specificity<-sum(abs<tr)/length(abs)
  #form1=sprintf('E:/postdoc/sensitivity/sensitivity_%s.csv',filename)
  #write.csv(sensitivity,
  #          file=form1 )
  #form2<-sprintf('E:/postdoc/analysis_files/specificity/specificity_%s.csv',filename)
  #write.csv(specificity,
  #          file=form2)
  TSS<-sensitivity + specificity - 1
  #form3<-sprintf('E:/postdoc/analysis_files/TSS/TSS_%s.csv', filename)
  #write.csv(TSS,
  #          file=form3)
  
  TSS<-data.frame(TSS)
  TSS$sp<-filename
  write.table(TSS,
              file="/usr/lib64/microsoft-r/rserver/o16n/9.1.0/rserve/workdir/Rserv9.1.0/BRT_TSS/TSS.csv",
              append=T, sep=",", row.names=F, col.names = F)
  sensitivity<-data.frame(sensitivity)
  sensitivity$sp<-filename
  write.table(sensitivity,
              file="/usr/lib64/microsoft-r/rserver/o16n/9.1.0/rserve/workdir/Rserv9.1.0/conn41297/BRT_sensitivity/sensitivity.csv",
              append=T, sep=",", row.names=F, col.names = F)
  specificity<-data.frame(specificity)
  specificity$sp<-filename
  write.table(specificity,
              file="/usr/lib64/microsoft-r/rserver/o16n/9.1.0/rserve/workdir/Rserv9.1.0/conn41297/BRT_specificity/specificity.csv",
              append=T, sep=",", row.names=F, col.names = F)
  tr<-data.frame(tr)
  tr$sp<-filename
  write.table(tr,
              file="/usr/lib64/microsoft-r/rserver/o16n/9.1.0/rserve/workdir/Rserv9.1.0/conn41297/BRT_threshold/threshold.csv",
              append=T, sep=",", row.names=F, col.names = F)
  auc<-data.frame(e@auc)
  auc$sp<-filename
  write.table(auc,
              file="/usr/lib64/microsoft-r/rserver/o16n/9.1.0/rserve/workdir/Rserv9.1.0/conn41297//BRT_auc/auc.csv",
              append=T, sep=",", row.names=F, col.names = F)
  trees<-sp.tc5.lr01.train$gbm.call$best.trees 
  trees$sp<-filename
  write.table(trees,
              file="/usr/lib64/microsoft-r/rserver/o16n/9.1.0/rserve/workdir/Rserv9.1.0/conn41297/BRT_trees/trees.csv",
              append=T, sep=",", row.names=F, col.names = F)
 
  # compare climate at gps points to the great lakes
  #Takes about 5 hours to run
  #save a png of the plot
  remoteExecute("current.predict<-rxPredict(gl.current, sp.tc5.lr01.train,
                           n.trees=sp.tc5.lr01.train$gbm.call$best.trees,
                           type='response')", 
                writePlots=TRUE, displayPlots=TRUE, recPlots = TRUE)
                
    colfun<-colorRampPalette(
    c("blue","cyan","green","yellow","red"))
  png(paste0("E:/BrokenHardDrive/postdoc/analysis_files/png_files/current/current_", filename, ".png"))
  current.plot<-levelplot(current.predict, 
                          main=paste(filename, "current"), 
                          xlim=c(-95,-70), ylim=c(40,52),
                          at=seq(0,1, length.out=1000),
                          col.regions=colfun,
                          margin=F,
                          maxpixels=13000000) +
    layer(sp.polygons(glb))
  print(current.plot)
  dev.off()
  
  #Once again, the next step takes hours to run
  remoteExecute("rcp45.50.predict<-predict(rcp45.50,sp.tc5.lr01.train,
                            n.trees=sp.tc5.lr01.train$gbm.call$best.trees,
                            type='response')", writePlots=TRUE, displayPlots=TRUE)
  
  png(paste0("E:/BrokenHardDrive/postdoc/analysis_files/png_files/current/current_", filename, ".png"))
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
  
  png(paste0("/usr/lib64/microsoft-r/rserver/o16n/9.1.0/rserve/workdir/Rserv9.1.0/conn41297/png_files/4570/4570_", filename, ".png"))
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
  
  
  rcp85.50.predict<-predict(rcp85.50, sp.tc5.lr01.train,
                            n.trees=sp.tc5.lr01.train$gbm.call$best.trees,
                            type="response")
  
  png(paste0("/usr/lib64/microsoft-r/rserver/o16n/9.1.0/rserve/workdir/Rserv9.1.0/conn41297/png_files/8550/8550_", filename, ".png"))
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
  
  png(paste0("/usr/lib64/microsoft-r/rserver/o16n/9.1.0/rserve/workdir/Rserv9.1.0/conn41297/png_files/8570/8570_", filename, ".png"))
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
