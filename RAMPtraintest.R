#get training and test data for RAMP
mt<-read.csv(file="C:/Users/vprescott/Desktop/RAMP2/Full_coords/Pisidiu_henslowanum_full.csv")

#created unique points BEFORE saving
#mt<- unique(mt)

#create training and test data
set.seed(0)
group=kfold(mt, 5)
pres_train=mt[group!=1,]
pres_test=mt[group==1,]

write.csv(pres_train,
          file="C:/Users/vprescott/Desktop/RAMP2/training/Pisidium_henslowanum.csv")
write.csv(pres_test, 
          file="C:/Users/vprescott/Desktop/RAMP2/test/Pisidium_henslowanum.csv")
