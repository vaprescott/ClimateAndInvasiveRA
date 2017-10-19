#what you need:
library("dismo", lib.loc="~/R/win-library/3.4")

#test data that includes RAMP spreadsheet (we labeled as pr)
pr<-read.csv(file="C:/Users/vprescott/Desktop/RAMP2/test_scores/Pisidium_henslowanum_test_scores.csv")
pr2<-read.csv(file="C:/Users/vprescott/Desktop/RAMP2/training_scores/Pisidium_henslowanum_train_scores.csv")

#create presence/absence for test data, let's say that a score of 6 or higher
#means presence, while a score less than 6 means lower
pr$pa_test<- ifelse(as.numeric(pr$Score)>5,1,0)
pr2$pa_train<-ifelse(as.numeric(pr2$Score)>5,1,0)
head(pr)
View(pr2)
calc.deviance(obs=pr2$pa_train, pred=pr$pa_test, calc.mean = TRUE)

#combined that column with the dataset
d<-cbind(pr$pa_test,pr2$pa_train)
View(d)
#label the 1/0 as presence/absence
pres<-d[d[,1]==1,2]
View(pres)
abs<-d[d[,1]==0,2]

#evaluate
e<-evaluate(p=pr$pa_test, a=pr2$pa_train)
e
threshold(e)
sensitivity<-sum(pres>=40757.43)/length(pres) 
specificity<-sum(abs<40757.43)/length(abs)
sensitivity
specificity


#what you need:
library("dismo", lib.loc="~/R/win-library/3.4")

#test data that includes RAMP spreadsheet (we labeled as pr)
pr<-read.csv(file="C:/Users/vprescott/Desktop/RAMP2/test_scores/current_scores/Procambarus_clarkii_test_scores.csv")
pr2<-read.csv(file="C:/Users/vprescott/Desktop/RAMP2/training_scores/current_scores/Procambarus_clarkii_train_scores.csv")

#create presence/absence for test data, let's say that a score of 6 or higher
#means presence, while a score less than 6 means lower
pr$pa_test<- ifelse(as.numeric(pr$Score)>5,1,0)
pr2$pa_train<-ifelse(as.numeric(pr2$Score)>5,1,0)

calc.deviance(obs=pr2$pa_train, pred=pr$pa_test, calc.mean = TRUE)

#combined that column with the dataset
d<-cbind(pr$pa_test,pr2$pa_train)
head(d)
#label the 1/0 as presence/absence
pres<-d[d[,1]==1,2]
abs<-d[d[,1]==0,2]
length(pres)
#evaluate
e<-evaluate(p=pr$pa_test, a=pr2$pa_train)
e
threshold(e)
sensitivity<-sum(pres>=40757.43)/length(pres) 
specificity<-sum(abs<40757.43)/length(abs)
sensitivity
specificity
