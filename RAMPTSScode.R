#what you need:
library("dismo", lib.loc="~/R/win-library/3.4")

species<-list.files(path="C:/Users/vprescott/Desktop/RAMP2/global_test_scores/global_test_nearest_scores",
                    pattern="_test_nearest_scores", full.names=TRUE)

for(i in 1:length(species)){
  test<-read.csv(species[i], header=TRUE)
  filename<-sub(pattern = "(.*)\\..*$", replacement = "\\1",
                basename(species[i]))
  filename<-sapply(strsplit(filename, "_test_nearest_scores"), "[[",1)  
  form1=sprintf('C:/Users/vprescott/Desktop/RAMP2/global_test_background/global_bg_nearest_scores/%s_bg_nearest_scores.csv', filename)
  bg<-read.csv(file=form1)

pres_backg_test<-c(rep(1,nrow(test)), 
                   rep(0, nrow(bg)))
testing<-rbind(test, bg)
envtest<-data.frame(cbind(pa=pres_backg_test, testing))
envtest<-envtest[,-2]


#calc.deviance(obs=envtest$pa, pred=envtest$Score, calc.mean = TRUE)
d<-envtest
pres<-d[d[,1]==1,2]
abs<-d[d[,1]==0,2]
e<-evaluate(p=pres, a=abs)
e
threshold(e)
tr<-threshold(e, "spec_sens")

sensitivity<-sum(pres>=tr)/length(pres) 

specificity<-sum(abs<tr)/length(abs)
TSS<-sensitivity + specificity - 1
TSS<-data.frame(TSS)
TSS$sp<-filename
write.table(TSS,
            file="D:/BrokenHardDrive/postdoc/analysis_files/TSS/TSS.csv",
            append=T, sep=",", row.names=F, col.names = F)
sensitivity<-data.frame(sensitivity)
sensitivity$sp<-filename
write.table(sensitivity,
            file="D:/BrokenHardDrive/postdoc/analysis_files/sensitivity/sensitivity.csv",
            append=T, sep=",", row.names=F, col.names = F)
specificity<-data.frame(specificity)
specificity$sp<-filename
write.table(specificity,
            file="D:/BrokenHardDrive/postdoc/analysis_files/specificity/specificity.csv",
            append=T, sep=",", row.names=F, col.names = F)
tr<-data.frame(tr)
tr$sp<-filename
write.table(tr,
            file="D:/BrokenHardDrive/postdoc/analysis_files/threshold/threshold.csv",
            append=T, sep=",", row.names=F, col.names = F)

}


#form=sprintf('D:/BrokenHardDrive/postdoc/analysis_files/threshold/threshold_%s.csv',filename)
#write.csv(tr,
#          file=form)
#form1=sprintf('E:/postdoc/sensitivity/sensitivity_%s.csv',filename)
#write.csv(sensitivity,
#          file=form1 )
#form2<-sprintf('E:/postdoc/analysis_files/specificity/specificity_%s.csv',filename)
#write.csv(specificity,
#          file=form2)

#form3<-sprintf('D:/BrokenHardDrive/postdoc/analysis_files/TSS', filename)
#write.csv(TSS,
#          file=form3)


#test data that includes RAMP spreadsheet (we labeled as pr)
#pr<-read.csv(file="C:/Users/vprescott/Desktop/RAMP2/test_scores/current_scores/Procambarus_clarkii_test_scores.csv")
#pr2<-read.csv(file="C:/Users/vprescott/Desktop/RAMP2/training_scores/current_scores/Procambarus_clarkii_train_scores.csv")

#create presence/absence for test data, let's say that a score of 6 or higher
#means presence, while a score less than 6 means lower
#pr$pa_test<- ifelse(as.numeric(pr$Score)>5,1,0)
#pr2$pa_train<-ifelse(as.numeric(pr2$Score)>5,1,0)

#calc.deviance(obs=pr2$pa_train, pred=pr$pa_test, calc.mean = TRUE)

#combined that column with the dataset
#d<-cbind(pr$pa_test,pr2$pa_train)
#head(d)
#label the 1/0 as presence/absence
#pres<-d[d[,1]==1,2]
#abs<-d[d[,1]==0,2]
#length(pres)
#evaluate
#e<-evaluate(p=pr$pa_test, a=pr2$pa_train)
#e
#threshold(e)
#sensitivity<-sum(pres>=40757.43)/length(pres) 
#specificity<-sum(abs<40757.43)/length(abs)
#sensitivity
#specificity
