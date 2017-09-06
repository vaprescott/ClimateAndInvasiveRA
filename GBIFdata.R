#get gbif data that I don't have to clean up
library("rgbif", lib.loc="~/R/win-library/3.4")
library("raster", lib.loc="~/R/win-library/3.4")

#determin which species you want
key<-name_suggest(q='Orconectes propinquus', rank='species')$key[1]

#get occurrence data from GBIF
occ<-occ_search(taxonKey=key, 
                limit=4500, 
                fields=c('decimalLatitude', 'decimalLongitude'),
                return='data')
head(occ)
#filter out replicates and double check
occ2<-unique(occ[c('decimalLatitude', 'decimalLongitude')])
head(occ2)

#remove 'decimal' from column name
names(occ2)[1:2]<-c("Latitude","Longitude")

#plot to double check distribution
current<-raster("E:/postdoc/Bioclim/WorldClim/Current/tif_files/bio_1.tif")
plot(!is.na(current), 
     col=c('white','light grey'), 
     legend=FALSE)
points(occ2, pch='-', col='red')

#save file name
#make sure to remane file!
write.csv(occ2,
          file="C:/Users/vprescott/Desktop/RAMP2/Orconectes_propinquus.csv")

