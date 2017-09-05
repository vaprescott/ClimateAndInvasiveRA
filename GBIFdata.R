#get gbif data that I don't have to clean up
library("rgbif", lib.loc="~/R/win-library/3.4")
key<-name_suggest(q='Morone saxtilis', rank='species')$key[1]
occ<-occ_search(taxonKey=key, limit=4500, 
                fields=c('decimalLatitude', 'decimalLongitude'),
                return='data')
occ2<-unique(occ[c('decimalLongitude','decimalLatitude')])
names(occ2)[1:2]<-c("Longitude","Latitude")
refcols<-c("Latitude","Longitude")
occ2<-occ2[,c(refcols, setdiff(names(occ2), refcols))]
head(occ2)
write.csv(occ2,
          file="C:/Users/vprescott/Desktop/RAMP2/Morone_saxtilis4.csv")
