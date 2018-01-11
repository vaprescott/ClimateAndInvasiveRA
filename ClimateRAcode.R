library("dismo", lib.loc="~/R/win-library/3.4")
library("gbm", lib.loc="~/R/win-library/3.4")
library("maps", lib.loc="~/R/win-library/3.4")
library("mapdata", lib.loc="~/R/win-library/3.4")
library("maptools", lib.loc="~/R/win-library/3.4")
library("rgdal", lib.loc="~/R/win-library/3.4")
#library("RPostgreSQL", lib.loc="~/R/win-library/3.4")
library("raster", lib.loc="~/R/win-library/3.4")
library("rasterVis", lib.loc="~/R/win-library/3.4")

#trying to bring in tiff files from postgre
#con<- dbConnect(PostgreSQL(), 
#                host='localhost', 
#                user='postgres',
#                password='ApplePeachPear', 
#                dbname='ClimateInvasiveRA')

#ras<- readGDAL(con)


#bring in tiff files for climate data and put them into one rasterstack
current.list=list.files(path="D:/BrokenHardDrive/postdoc/Bioclim/WorldClim/Current/wc2.0_2.5m_bio", 
  pattern="tif$", full.names=TRUE )
current=stack(current.list)
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
#great lakes current
gl.current.list=list.files(path="E:/postdoc/Bioclim/WorldClim/Current/tiff_gl",
          pattern="tif$", full.names=TRUE)
gl.current=stack(gl.current.list)
glb<-readOGR("E:/postdoc/glin_gl_mainlakes/gl_mainlakes.shp")

#bring in coordinates of species of interest
species<-list.files(path="D:/BrokenHardDrive/postdoc/analysis_files/training/global_training",
                    pattern="train_", full.names=TRUE)
full.species<-list.files(path="D:/BrokenHardDrive/postdoc/analysis_files/sp_coords/corrected_coords",
                         pattern="_corrected2.csv",full.names=TRUE)
for(i in 1:length(species)){
  sp.coords.train<-read.csv(species[i], header=TRUE)
filename<-sub(pattern = "(.*)\\..*$", replacement = "\\1",
                basename(species[i]))
filename<-sapply(strsplit(filename, "train_"), "[[",-1)  
form1=sprintf('D:/BrokenHardDrive/postdoc/analysis_files/testing/global_test/test_%s.csv', filename)
sp.coords.test<-read.csv(file=form1)
#filename<-sub(pattern="_", replacement =" ", 
#                basename(filename))

sp.coords.train<-sp.coords.train[,c("Longitude","Latitude")]
sp.coords.test<-sp.coords.test[,c("Longitude","Latitude")]
head(sp.coords.test)

#get full set of coords to get number of random background points
filename.full<-sub(pattern=" ", replacement ="_", 
                            basename(filename))
form.coords<-sprintf('D:/BrokenHardDrive/postdoc/analysis_files/sp_coords/corrected_coords/%s_corrected2.csv', filename.full)
sp.coords.full<-read.csv(file=form.coords)
head(sp.coords.full)

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
set.seed(10)
form_backg=sprintf("D:/BrokenHardDrive/postdoc/analysis_files/background_raster/%s.tif", filename)
background=raster(form_backg)
#ext=extent(90.0000078231,-180,180.000018775,-60)
#ext=extent(background)
#crs(background)<-"+proj=longlat +datum=WGS84"
#r.spgrd = background[!is.na(background[[1]]),]
#bias3<-raster("E:/postdoc/analysis_files/background/misgu3_MCP/mis_poly_1.shp")
#backg=randomPoints(current, 
#                   n=nrow(sp.coords.full),
#                   ext=ext,
#                   extf=1)
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
coords <- raster.random.points(nrow(sp.coords.full), background)
# plot the raster and the random points
plot(background)
points(coords, pch=19, cex=0.2)
backg<-coords
# extract the cell values associated with the random points
#cells <- cellFromXY(background, coords)
#vals <- background[cells]
colnames(backg)=c('Longitude','Latitude')
#backg<-backg[,c("Longitude","Latitude")]
group=kfold(backg,5)
backg_train=backg[group!=1,]
backg_test=backg[group==1,]

#create a raster of rasterstack, showing just first layer and plot to check data
r=raster(current,1)
plot(!is.na(r), col=c('white','light grey'), legend=FALSE)
points(backg_train, pch='-', cex=0.5, col='blue')
points(backg_test, pch='+', cex=0.5, col='black')
points(sp.coords.train, pch='+', cex=0.5, col='yellow')
points(sp.coords.test, pch='+', cex=0.5,col='purple')

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
sp.tc5.lr01.train$gbm.call$best.trees

#determine how well the test data does with this model
predict_test=predict(sp.tc5.lr01.train,envtest, 
           n.trees=sp.tc5.lr01.train$gbm.call$best.trees,
           type="response")
##TRY TO PLOT PREDICT_TEST
calc.deviance(obs=envtest$pa, pred=predict_test, calc.mean = TRUE)
d<-cbind(envtest$pa, predict_test)
pres<-d[d[,1]==1,2]
abs<-d[d[,1]==0,2]
e<-evaluate(p=pres, a=abs)
e
threshold(e)
tr<-threshold(e, "equal_sens_spec")
form=sprintf('E:/postdoc/analysis_files/threshold/threshold_%s.csv',filename)
write.csv(tr,
          file=form)
sensitivity<-sum(pres>=tr)/length(pres) 
  #use the desired threshold value from previous step
specificity<-sum(abs<tr)/length(abs)
form1=sprintf('E:/postdoc/sensitivity/sensitivity_%s.csv',filename)
write.csv(sensitivity,
          file=form1 )
form2<-sprintf('E:/postdoc/analysis_files/specificity/specificity_%s.csv',filename)
write.csv(specificity,
          file=form2)
TSS<-sensitivity + specificity - 1
form3<-sprintf('E:/postdoc/analysis_files/TSS/TSS_%s.csv', filename)
write.csv(TSS,
          file=form3)

# compare climate at gps points to the great lakes
#Takes about 5 hours to run
#save a png of the plot
current.predict<-predict(gl.current, sp.tc5.lr01.train,
           n.trees=sp.tc5.lr01.train$gbm.call$best.trees,
           type="response")
colfun<-colorRampPalette(
  c("blue","cyan","green","yellow","red"))
png(paste0("E:/postdoc/analysis_files/png_files/current/current_", filename, ".png"))
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
png(paste0("E:/postdoc/analysis_files/png_files/4550/4550_", filename, ".png"))
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
png(paste0("E:/postdoc/analysis_files/png_files/4570/4570_", filename, ".png"))
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
png(paste0("E:/postdoc/analysis_files/png_files/8550/8550_", filename, ".png"))
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
png(paste0("E:/postdoc/analysis_files/png_files/8570/8570_", filename, ".png"))
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

#create animation of 2050 and 2070 
#test<-stack(current.predict,rcp45.50.predict, rcp85.50.predict, rcp85.70.predict)
#animation<-animate(test, pause=2, n=100, 
#                   xlim=c(-95,-70), 
#                   ylim=c(40,52),
#                   col=colors,
#                   breaks=breakpoints,
#                   maxpixels=7000000)

}


