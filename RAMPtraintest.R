library("plyr", lib.loc="~/R/win-library/3.4")

#get training and test data for RAMP
species<-list.files(path="D:/BrokenHardDrive/postdoc/analysis_files/sp_coords/corrected_coords",
                    pattern="_corrected2.csv", full.names=TRUE)
for(i in 1:length(species)){
  sp.coords<-read.csv(species[i], header=TRUE)
filename<-sub(pattern = "(.*)\\..*$", replacement = "\\1",
                basename(species[i]))
filename<-sub(pattern="_", replacement =" ", 
                basename(filename))
filename<-sapply(strsplit(filename, "_corrected2"), "[[",1)  
sp.coords<-sp.coords[,c("latitude","longitude")]
rename(sp.coords, c("latitude"="Latitude","longitude"="Longitude"))

#create training and test data
set.seed(0)
group=kfold(sp.coords, 5)
pres_train=sp.coords[group!=1,]
pres_test=sp.coords[group==1,]

formTrain<-sprintf('D:/BrokenHardDrive/postdoc/analysis_files/training/global_training/train_%s.csv', filename)
write.csv(pres_train,
          file=formTrain,
          row.names=FALSE)

formTest<-sprintf('D:/BrokenHardDrive/postdoc/analysis_files/testing/global_test/test_%s.csv', filename)
write.csv(pres_test,
          file=formTest,
        row.names=FALSE)


}
