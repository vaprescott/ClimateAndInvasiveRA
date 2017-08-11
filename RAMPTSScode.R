#what you need:
#test data that includes RAMP spreadsheet (we labeled as pr)

#create presence/absence for test data, let's say that a score of 6 or higher
#means presence, while a score less than 6 means lower
pr$pa_test<- ifelse(as.numeric(pr$Score)>5,1,0)

#combined that column with the dataset
d<-cbind(pr$pa_test,pr)

#label the 1/0 as presence/absence
pres<-d[d[,1]==1,2]
abs<-d[d[,1]==0,2]

#evaluate
e<-evaluate(p=pres, a=abs)
e
threshold(e)
sensitivity<-sum(pres>=736.6559)/length(pres) 
specificity<-sum(abs<736.6559)/length(abs)
