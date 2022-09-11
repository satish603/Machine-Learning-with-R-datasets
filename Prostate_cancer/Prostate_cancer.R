pcancer<-read.csv("Prostate_cancer.csv")
summary(pcancer)
View(pcancer)
pcancer<-pcancer[-11,-1]
View(pcancer)
str(pcancer)
table(pcancer$diagnosis_result)
pcancer$diagnosis_result<-factor(pcancer$diagnosis_result,levels = c("B","M"),labels=c("Benign","Malignant"))
round(prop.table(table(pcancer$diagnosis_result))*100,digits = 1)
summary(pcancer[-1])
normalize<- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
pcancer_n<-as.data.frame(lapply(pcancer[2:9], normalize))
summary(pcancer_n)
View(pcancer_n)
train_pcancer<-pcancer_n[1:70, ]
test_pcancer<-pcancer_n[70:99,]
train_lable<-pcancer[1:70,1]
test_lable<-pcancer[70:99,1]
train_lable
train_pcancer

library("class")
pcancer_pred_test<-knn(train = train_pcancer,test = test_pcancer,cl=train_lable,k=70)
library(gmodels)
CrossTable(x= test_lable,y= pcancer_pred_test,prop.chisq = FALSE)
