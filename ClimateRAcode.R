library("dismo", lib.loc="~/R/library")
library("gbm", lib.loc="~/R/library")
library("maps", lib.loc="~/R/library")
library("mapdata", lib.loc="~/R/library")
library("maptools", lib.loc="~/R/library")
library("rgdal", lib.loc="~/R/library")
#bring in tiff files for climate data and put them into one rasterstack

current.list=list.files(path="E:/postdoc/WorldClim/Current/tif_files", 
  pattern="tif$", full.names=TRUE )
current=stack(current.list)

#bring in coordinates of species of interest, and switch columns so that latitude is first
a.torren.coords <- read.csv("C:/Users/vprescott/Desktop/RAMP2/Species/Austropotamobius_torrentium/Austropotamobius torrentium GBIF Locations.csv")
a.torren.coords<-a.torren.coords[c("Latitude","Longitude")]

#create presence training and test data
set.seed(0)
group=kfold(a.torren.coords, 5)
pres_train=a.torren.coords[group!=1,]
pres_test=a.torren.coords[group==1,]

#create background training and test data (in lieu of absence data)
set.seed(10)
ext=extent(6,16,46,50)
backg=randomPoints(current, n=500,ext=ext, extf=1.25)
colnames(backg)=c('Longitude','Latitude')
group=kfold(backg,5)
backg_train=backg[group!=1,]
backg_test=backg[group==1,]

#create a raster of rasterstack, showing just first layer and plot to check data
r=raster(current,1)
plot(!is.na(r), col=c('white','light grey'), legend=FALSE)
points(backg_train, pch='-', cex=0.5, col='yellow')
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
a.torren.tc5.lr001.train<-gbm.step(data=envtrain, gbm.x=2:20, gbm.y=1,
                                  family="bernoulli", tree.complexity = 5,
                                  learning.rate=0.001, bag.fraction=0.5)

#determine how well the test data does with this model
pr=predict(a.torren.tc5.lr01.train,envtest, 
           n.trees=a.torren.tc5.lr01$gbm.call$best.trees,
           type="response")
calc.deviance(obs=envtest$pa, pred=pr, calc.mean = TRUE)
d<-cbind(envtest$pa, pr)
pres<-d[d[,1]==1,2]
abs<-d[d[,1]==0,2]
e<-evaluate(p=pres, a=abs)
e

#run the model across the world (compare climate at gps points to the entire world)
#will eventually have to crop this to the great lakes region

p<-predict(current, a.torren.tc5.lr01.train,
           n.trees=a.torren.tc5.lr01.train$gbm.call$best.trees,
           type="response")

#plot map
#this is where I will try to mask with a map of great lakes/NA
p=mask(p, raster(current,1))
plot(p, main="A. torren-BRT prediction")