library("dismo", lib.loc="~/R/win-library/3.4")
library("gbm", lib.loc="~/R/win-library/3.4")
library("maps", lib.loc="~/R/win-library/3.4")
library("mapdata", lib.loc="~/R/win-library/3.4")
library("maptools", lib.loc="~/R/win-library/3.4")
library("rgdal", lib.loc="~/R/win-library/3.4")
#library("RPostgreSQL", lib.loc="~/R/win-library/3.4")
library("raster", lib.loc="~/R/win-library/3.4")

#bring in tiff files for climate data and put them into one rasterstack
current.list=list.files(path="E:/postdoc/Bioclim/WorldClim/Current/tif_files", 
                        pattern="tif$", full.names=TRUE )
current=stack(current.list)

gl.current.list=list.files(path="E:/postdoc/Bioclim/WorldClim/Current/tiff_gl",
                           pattern="tif$", full.names=TRUE)
gl.current=stack(gl.current.list)
glb<-readOGR("E:/postdoc/glin_gl_mainlakes/gl_mainlakes.shp")

#bring in coordinates of species of interest
species<-list.files(path="C:/Users/vprescott/Desktop/RAMP2/Full_coords",
                    pattern="csv", full.names=TRUE)

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

xm<-maxent(current, pres_train,
           removeDuplicates=TRUE,
           path='E:/postdoc/M.guruns',
           args=c(
             'maximumbackground=nrow(sp.coords)',
             'defaultprevalence=1.00',
             'betamultiplier=0.5',
             'pictures=true',
             'randomtestpoints=30',
             'linear=true',
             'quadratic=true',
             'product=true',
             'threshold=true',
             'hinge=true',
             'threads=2',
             'responsecurves=false',
             'jackknife=false',
             'askoverwrite=false'))

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

