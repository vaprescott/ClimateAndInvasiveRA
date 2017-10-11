library("plyr", lib.loc="~/R/win-library/3.4")

#get training and test data for RAMP
species<-list.files(path="E:/postdoc/analysis_files/sp_coords/full_coords",
                    pattern="csv", full.names=TRUE)
for(i in 1:length(species)){
  sp.coords<-read.csv(species[i], header=TRUE)
filename<-sub(pattern = "(.*)\\..*$", replacement = "\\1",
                basename(species[i]))
filename<-sub(pattern="_", replacement =" ", 
                basename(filename))
filename<-sapply(strsplit(filename, "_full2"), "[[",1)  
sp.coords<-sp.coords[,c("latitude","longitude")]
rename(sp.coords, c("latitude"="Latitude","longitude"="Longitude"))

#create training and test data
set.seed(0)
group=kfold(sp.coords, 5)
pres_train=sp.coords[group!=1,]
pres_test=sp.coords[group==1,]

formTrain<-sprintf('C:/Users/vprescott/Desktop/RAMP2/training/train_%s.csv', filename)
write.csv(pres_train,
          file=formTrain,
          row.names=FALSE)

formTest<-sprintf('C:/Users/vprescott/Desktop/RAMP2/test/test_%s.csv', filename)
write.csv(pres_test,
          file=formTest,
        row.names=FALSE)


}
