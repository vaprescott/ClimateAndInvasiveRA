library("dismo", lib.loc="~/R/win-library/3.4")
library("gbm", lib.loc="~/R/win-library/3.4")
library("maps", lib.loc="~/R/win-library/3.4")
library("mapdata", lib.loc="~/R/win-library/3.4")
library("maptools", lib.loc="~/R/win-library/3.4")
library("rgdal", lib.loc="~/R/win-library/3.4")
library("RPostgreSQL", lib.loc="~/R/win-library/3.4")
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
gl.current.list=list.files(path="E:/postdoc/Bioclim/WorldClim/Current/tiff_gl",
          pattern="tif$", full.names=TRUE)
gl.current=stack(gl.current.list)
glb<-readOGR("E:/postdoc/glin_gl_mainlakes/gl_mainlakes.shp")

#bring in coordinates of species of interest, and switch columns so that latitude is first
species<-list.files(path="C:/Users/vprescott/Desktop/RAMP2/Full_coords",
                    pattern="csv", full.names=TRUE)
for(i in 1:length(species)){
  sp.coords<-read.csv(species[i], header=TRUE)
filename<-sub(pattern = "(.*)\\..*$", replacement = "\\1",
                basename(species[i]))
filename<-sub(pattern="_", replacement =" ", 
                basename(filename))
filename<-sapply(strsplit(filename, "_full"), "[[",1)  
#sp.coords<-sp.coords[c("Latitude","Longitude")]


#create presence training and test data
set.seed(0)
group=kfold(sp.coords, 5)
pres_train=sp.coords[group!=1,]
pres_test=sp.coords[group==1,]

#create background training and test data (in lieu of absence data)
set.seed(10)
max.x<-max(sp.coords$Latitude)
min.x<-min(sp.coords$Latitude)
max.y<-max(sp.coords$Longitude)
min.y<-min(sp.coords$Longitude)
ext=extent(min.x,max.x,min.y,max.y)
backg=randomPoints(current, n=500,ext=ext, extf=1.25)
colnames(backg)=c('Longitude','Latitude')
group=kfold(backg,5)
backg_train=backg[group!=1,]
backg_test=backg[group==1,]

#create a raster of rasterstack, showing just first layer and plot to check data
r=raster(current,1)
plot(!is.na(r), col=c('white','light grey'), legend=FALSE)
points(backg_train, pch='-', cex=0.5, col='red')
points(backg_test, pch='+', cex=0.5, col='black')
points(pres_train, pch='+', col='green')
points(pres_test, pch='+', col='blue')

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
sensitivity<-sum(pres>=tr)/length(pres) 
  #use the desired threshold value from previous step
specificity<-sum(abs<tr)/length(abs)
sensitivity
specificity

#run the model across the world (compare climate at gps points to the great lakes)
#Takes about 5 hours to run
predict_current<-predict(gl.current, sp.tc5.lr01.train,
           n.trees=sp.tc5.lr01.train$gbm.call$best.trees,
           type="response")

#plot map
colfun<-colorRampPalette(c("blue","cyan","green","yellow","red"))
plot(predict_current, main=c(filename, ", current"), 
     xlim=c(-95,-70), ylim=c(40,52),
      col=colfun(50))
plot(glb, add=TRUE)


#to crop to just North America
#cropped<-raster("E:/postdoc/Mod_WorldClim/ModLayers_2050_45/mod_rasters/mod_bio_1.tif")
#current_NA<-crop(predict_current_mask, cropped)
#current_NA<-mask(current_NA,cropped)
#plot(current_NA,
#     main="ADD TITLE")

#use future projection RCP 45 2050
rcp45.50.list=list.files(path="E:/postdoc/Bioclim/Mod_WorldClim/Modlayers_2050_45/mod_50_45_tiff_gl", 
                         pattern="tif$", full.names=TRUE )
rcp45.50=stack(rcp45.50.list)
#Once again, the next step takes hours to run
predict_RCP45_50<-predict(rcp45.50,sp.tc5.lr01.train,
            n.trees=sp.tc5.lr01.train$gbm.call$best.trees,
            type="response")
#predict_RCP45_50_cropped<-crop(predict_RCP45_50,cropped)
#predict_RCP45_50_mask<-mask(predict_RCP45_50_cropped,cropped)
plot(predict_RCP45_50, 
     main="P. henslowanum, RCP 4.5 2050", 
     xlim=c(-95,-70), ylim=c(40,52),
     col=colfun(50))
plot(glb, add=TRUE)
           
#do the same for the next climate horizon
rcp45.70.list=list.files(path="E:/postdoc/WorldClim/2070/GF-RCP45/gf45bi70/", 
                         pattern="tif$", full.names=TRUE )
rcp45.70=stack(rcp45.70.list)
rcp45.7.predict<-predict(rcp45.70, sp.tc5.lr01.train,
                         n.trees=sp.tc5.lr01.train$gbm.call$best.trees,
                         type="response")
#rcp45.70_crop=crop(rcp45.7.predict,cropped)
#rcp45.70_mask<-mask(rcp45.70_crop,cropped)
#plot(rcp45.70_mask, xlim=c(-170,-20), ylim=c(10,90),
#     main="A. torrentium RCP 4.5 2070", cex.main=1.25, 
#     breaks=breakpoints,col=colors)
#map(database="state", col="black",fill=FALSE, add=TRUE)
#text(cex.main=1.25,add=TURE)

#create animation of 2050 and 2070 
test<-stack(predict_current, predict_RCP45_50)
animation<-animate(test, pause=1, n=100, 
                   xlim=c(-95,-70), 
                   ylim=c(40,52),
                   col=colfun(50),
                   maxpixels=7000000)


}


