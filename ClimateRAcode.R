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
species<-list.files(path="E:/BrokenHardDrive/postdoc/analysis_files/training/global_training",
                    pattern="train_", full.names=TRUE)

for(i in 28:29){
 sp.coords.train<-read.csv(species[i], header=TRUE)
  filename<-sub(pattern = "(.*)\\..*$", replacement = "\\1",
                basename(species[i]))
  filename<-sapply(strsplit(filename, "train_"), "[[",-1)  
  form1=sprintf('E:/BrokenHardDrive/postdoc/analysis_files/testing/global_test/test_%s.csv', filename)
  sp.coords.test<-read.csv(file=form1)
#filename<-sub(pattern="_", replacement =" ", 
#                basename(filename))

sp.coords.train<-sp.coords.train[,c("Longitude","Latitude")]
sp.coords.test<-sp.coords.test[,c("Longitude","Latitude")]

#get background training and testing data points
form_bg_train<-sprintf("E:/BrokenHardDrive/postdoc/analysis_files/train_background/%s.csv", filename)
backg_train<-read.csv(file=form_bg_train)
backg_train<-backg_train[,c("Longitude","Latitude")]
form_bg_test<-sprintf("E:/BrokenHardDrive/postdoc/analysis_files/test_background/%s.csv", filename)
backg_test<-read.csv(file=form_bg_test)
backg_test<-backg_test[,c("Longitude","Latitude")]


#create data frame with presence and background training scores and environmental data
backg_train_current<-extract(current, backg_train)
pres_train_current<-extract(current, sp.coords.train)
pres_backg_train<-c(rep(1,nrow(pres_train_current)), 
                    rep(0, nrow(backg_train_current)))
train<-rbind(pres_train_current, backg_train_current)
envtrain<-data.frame(cbind(pa=pres_backg_train, train))

#do the same for test data
backg_test_current<-extract(current, backg_test)
pres_test_current<-extract(current, sp.coords.test)
pres_backg_test<-c(rep(1,nrow(pres_test_current)), 
                   rep(0, nrow(backg_test_current)))
test<-rbind(pres_test_current, backg_test_current)
envtest<-data.frame(cbind(pa=pres_backg_test, test))

#run a model, lower the learning rate if you get the algorithm warning
sp.tc5.lr01.train<-gbm.step(data=envtrain, gbm.x=2:20, gbm.y=1,
                                  family="bernoulli", tree.complexity = 5,
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


#determine how well the test data does with this model
predict_test=predict(sp.tc5.lr01.train,envtest, 
                     n.trees=sp.tc5.lr01.train$gbm.call$best.trees,
                     type="response")

#png(paste0("E:/BrokenHardDrive/postdoc/analysis_files/png_files/current_global/current_global_", filename, ".png"))
#current.plot<-levelplot(sp.tc5.lr01.train, 
#                        main=paste(filename, "global model"), 
#                        xlim=c(-95,-70), ylim=c(40,52),
#                        at=seq(0,1, length.out=1000),
#                        col.regions=colfun,
#                        margin=F,
#                        maxpixels=13000000)

#print(current.plot)
#dev.off()

##TRY TO PLOT PREDICT_TEST
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
write.table(auc,
            file="E:/BrokenHardDrive/postdoc/analysis_files/BRT_auc/auc.csv",
            append=T, sep=",", row.names=F, col.names = F)
trees<-sp.tc5.lr01.train$gbm.call$best.trees 
trees$sp<-filename
write.table(trees,
            file="E:/BrokenHardDrive/postdoc/analysis_files/BRT_trees/trees.csv",
            append=T, sep=",", row.names=F, col.names = F)

# compare climate at gps points to the great lakes
#Takes about 5 hours to run
#save a png of the plot
current.predict<-predict(gl.current, sp.tc5.lr01.train,
           n.trees=sp.tc5.lr01.train$gbm.call$best.trees,
           type="response")

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
rcp45.50.predict<-predict(rcp45.50,sp.tc5.lr01.train,
            n.trees=sp.tc5.lr01.train$gbm.call$best.trees,
            type="response")

png(paste0("E:/BrokenHardDrive/postdoc/analysis_files/png_files/4550/4550_", filename, ".png"))
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

png(paste0("E:/BrokenHardDrive/postdoc/analysis_files/png_files/4570/4570_", filename, ".png"))
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

png(paste0("E:/BrokenHardDrive/postdoc/analysis_files/png_files/8550/8550_", filename, ".png"))
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

png(paste0("E:/BrokenHardDrive/postdoc/analysis_files/png_files/8570/8570_", filename, ".png"))
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



#get full set of coords to get number of random background points
#filename.full<-sub(pattern=" ", replacement ="_", 
#                            basename(filename))
#form.coords<-sprintf('D:/BrokenHardDrive/postdoc/analysis_files/sp_coords/corrected_coords/%s_corrected2.csv', filename.full)
#sp.coords.full<-read.csv(file=form.coords)
#head(sp.coords.full)

#create presence training and test data
#set.seed(0)
#group=kfold(sp.coords, 5)
#pres_train=sp.coords[group!=1,]
#pres_test=sp.coords[group==1,]

#save training and testing data for RAMP
#write.csv(pres_train,
# file="C:/Users/vprescott/Desktop/RAMP2/training/Pisidium_henslowanum.csv")
#write.csv(pres_test, 
# file="C:/Users/vprescott/Desktop/RAMP2/test/Pisidium_henslowanum.csv")

#create background training and test data (in lieu of absence data)
#set.seed(10)
#form_backg=sprintf("D:/BrokenHardDrive/postdoc/analysis_files/background_raster/%s.tif", filename)
#background=raster(form_backg)
#ext=extent(background)
#crs(background)<-"+proj=longlat +datum=WGS84"

#raster.random.points<-function(size, background, na.rm=TRUE){
#  coords<-matrix(0, nrow=size, ncol=2)
#  coords[,1]<-runif(size, xmin(background), xmax(background))
#  coords[,2] <- runif(size, ymin(background), ymax(background))
#  if (na.rm) {
#    cells <- cellFromXY(background, coords)
#    na.cnt <- length(which(is.na(background[cells])))
#    while (na.cnt > 0){
#      recs <- which(is.na(background[cells]))
#      coords[recs,1] <- runif(length(recs), xmin(background), xmax(background))
#      coords[recs,2] <- runif(length(recs), ymin(background), ymax(background))
#      cells <- cellFromXY(background, coords)
#      na.cnt <- length(which(is.na(background[cells])))
#    }}
#  return(coords)
#}
# now call the function to generate random points
#coords <- raster.random.points(nrow(sp.coords.full), background)

# plot the raster and the random points
#plot(background)
#points(coords, pch=19, cex=0.2)
#backg<-coords
#colnames(backg)=c('Longitude','Latitude')
#backg<-backg[,c("Longitude","Latitude")]
#group=kfold(backg,5)
#backg_train=backg[group!=1,]
#backg_test=backg[group==1,]
#form_bg_test<-sprintf("D:/BrokenHardDrive/postdoc/analysis_files/RAMP_test_background/%s.csv", filename)
#write.csv(backg_test,
#          file=form_bg_test)

#create a raster of rasterstack, showing just first layer and plot to check data
#r=raster(current,1)
#plot(!is.na(r), col=c('white','light grey'), legend=FALSE)
#points(backg_train, pch='-', cex=0.5, col='blue')
#points(backg_test, pch='+', cex=0.5, col='black')
#points(sp.coords.train, pch='+', cex=0.5, col='yellow')
#points(sp.coords.test, pch='+', cex=0.5,col='purple')

