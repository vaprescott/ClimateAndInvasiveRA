#get training and test data for RAMP

#create presence training and test data
set.seed(0)
group=kfold(a.torren.coords, 5)
pres_train=a.torren.coords[group!=1,]
pres_test=a.torren.coords[group==1,]