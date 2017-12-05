#get gbif data that I don't have to clean up
library("rgbif", lib.loc="~/R/win-library/3.4")
library("raster", lib.loc="~/R/win-library/3.4")

#determin which species you want
key<-name_suggest(q='Misgurnus fossilis', rank='species')$key[1]
 
#get occurrence data from GBIF
occ<-occ_search(taxonKey=key, 
                limit=4500, 
                fields=c('decimalLatitude', 'decimalLongitude'),
                return='data')

#plot to double check distribution
current<-raster("D:/BrokenHardDrive/postdoc/Bioclim/WorldClim/Current/tif_files/bio_1.tif")
plot(!is.na(current), 
     col=c('white','light grey'), 
     legend=FALSE)
plot(!is.na(current), col=c('white','light grey'), legend=FALSE)
points(occ, pch='-', col='red')

#switch column order and get unique values
occ2<-unique(occ[c('decimalLongitude','decimalLatitude')])
#head(occ2)

#remove 'decimal' from column name
names(occ2)[1:2]<-c("longitude","latitude")
occ2 <- unique(occ2[ , 1:2 ] )

#save file name
#make sure to remane file!
write.csv(occ2,
          file="D:/BrokenHardDrive/postdoc/analysis_files/sp_coords/raw_coords/Misguruns_fossilis_raw.csv")

#R likes to  have longitude first in analysis, but RAMP like latitude first
