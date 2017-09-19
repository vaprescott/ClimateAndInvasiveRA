library("dismo", lib.loc="~/R/win-library/3.4")
library("gbm", lib.loc="~/R/win-library/3.4")
library("maps", lib.loc="~/R/win-library/3.4")
library("mapdata", lib.loc="~/R/win-library/3.4")
library("maptools", lib.loc="~/R/win-library/3.4")
library("rgdal", lib.loc="~/R/win-library/3.4")
#library("RPostgreSQL", lib.loc="~/R/win-library/3.4")
library("raster", lib.loc="~/R/win-library/3.4")

#trying to bring in tiff files from postgre
#con<- dbConnect(PostgreSQL(), 
#                host='localhost', 
#                user='postgres',
#                password='ApplePeachPear', 
#                dbname='ClimateInvasiveRA')

#ras<- readGDAL(con)


#bring in tiff files for climate data and put them into one rasterstack
current.list=list.files(path="E:/postdoc/Bioclim/WorldClim/Current/tif_files", 
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
species<-list.files(path="C:/Users/vprescott/Desktop/RAMP2/Full_coords",
                    pattern="csv", full.names=TRUE)
for(i in 1:length(species)){
  sp.coords<-read.csv(species[i], header=TRUE)
filename<-sub(pattern = "(.*)\\..*$", replacement = "\\1",
                basename(species[i]))
filename<-sub(pattern="_", replacement =" ", 
                basename(filename))
filename<-sapply(strsplit(filename, "_full"), "[[",1)  
sp.coords<-sp.coords[,c("Longitude","Latitude")]

#create presence training and test data
set.seed(0)
group=kfold(sp.coords, 5)
pres_train=sp.coords[group!=1,]
pres_test=sp.coords[group==1,]

#create background training and test data (in lieu of absence data)
set.seed(10)
ext=extent(bias3)
bias3<-readOGR("E:/postdoc/analysis_files/background/misgu3_MCP/mis_poly_1.shp")
backg=randomPoints(current, 
                   n=nrow(sp.coords),
                   ext=ext,
                   extf=1.25)
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
points(pres_train, pch='+', cex=0.5, col='green')
points(pres_test, pch='+', cex=0.5,col='purple')

#create data frame with presence and background training scores and environmental data
backg_train_current<-extract(current, backg_train)
pres_train_current<-extract(current, pres_train)
pres_backg_train<-c(rep(1,nrow(pres_train_current)), 
                    rep(0, nrow(backg_train_current)))
train<-rbind(pres_train_current, backg_train_current)
envtrain<-data.frame(cbind(pa=pres_backg_train, train))

#do the same for test data
backg_test_current<-extract(current, backg_test)
pres_test_current<-extract(current, pres_test)
pres_backg_test<-c(rep(1,nrow(pres_test_current)), 
                   rep(0, nrow(backg_test_current)))
test<-rbind(pres_test_current, backg_test_current)
envtest<-data.frame(cbind(pa=pres_backg_test, test))

#run a model, lower the learning rate if you get the algorithm warning
sp.tc5.lr01.train<-gbm.step(data=envtrain, gbm.x=2:20, gbm.y=1,
                                  family="bernoulli", tree.complexity = 5,
                                  learning.rate=0.01, bag.fraction=0.5)

#determine best number of trees
sp.tc5.lr01.train$gbm.call$best.trees

#determine how well the test data does with this model
predict_test=predict(sp.tc5.lr01.train,envtest, 
           n.trees=sp.tc5.lr01.train$gbm.call$best.trees,
           type="response")
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

#run the model across the world (compare climate at gps points to the great lakes)
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


