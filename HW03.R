setwd("~/Documents/Class/STAT-983")
library(Hmisc)
library(lattice)
library(broom)
library(tidyverse)
library(leaps)
library(MASS)
library(lars)
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(reshape2)
library(ggcorrplot)
library(class)
library(caret)


Auto1 <- read.table(file = "Auto.csv", sep = ",", header=T)
mpg01 = I(Auto1$mpg >= median(Auto1$mpg))*1
Auto  = data.frame(mpg01, Auto1[,-1]) ## replace column "mpg" by "mpg01"
origins <- c('USA', 'Europe', 'Japan')
Auto$origin <- factor(Auto$origin, labels = origins)

Auto$mpg01<-as.factor(Auto$mpg01)
Auto$cylinders<-as.factor(Auto$cylinders)
Auto$year<-as.factor(Auto$year)

set.seed(33)
tstIndex=sample(1:dim(Auto)[1],round(0.2*dim(Auto)[1]),replace = F)
Autotrain=Auto[-tstIndex,]
Autotest=Auto[tstIndex,]


summary(Auto)
par(mfrow = c(1,4))
for (i in 3:6) boxplot(Autotrain[,i], xlab=names(Auto)[i])
dev.off()

#correlation
cor1=round(cor(Auto[,3:6]),2)
write.csv(cor1, file = "corrA.csv")

ggplot(data = Auto, aes( origin)) +
  geom_bar( stat='count') +
  xlab('Region of Origin') +
  ylab('Count') +
  ggtitle('Frequency: Origin') 

p1=ggplot(data = Auto, aes(x = origin, y = weight)) +
  geom_boxplot() +
  xlab('Region of Origin') +
  ylab('Weight') +
  ggtitle('Weight Vs. Region of Origin') 

p2=ggplot(data = Auto, aes(x = cylinders, y = weight)) +
  geom_boxplot() +
  xlab('Number of Cylinders') +
  ylab('Weight') +
  ggtitle('Weight Vs. Number of Cylinders') 

pixelFigure=ggarrange(plotlist=list(p1,p2),ncol = 2,nrow = 1)
pixelFigure


ggplot(data = Auto, aes(x = mpg01, y = weight)) +
  geom_boxplot() +
  xlab('MPG') +
  ylab('Weight') +
  ggtitle('MPG by Weight') 


ggplot(data = Auto, aes(x =  cylinders, fill = origin)) +
  geom_bar() +
  xlab('Number of Cylinders') +
  ylab('Count') +
  ggtitle('Cars from Each Region by Number of Cylinders')

ggplot(data = Auto, aes(x =  mpg01, fill = origin)) +
  geom_bar() +
  xlab('Number of Cylinders') +
  ylab('Count') +
  ggtitle('Cars from Each Region by Number of Cylinders')

ggplot(data = Auto, aes(x = year, fill = cylinders)) +
  geom_bar() +
  facet_wrap(~ origin, ncol = 1) +
  xlab('Model Year') +
  ylab('Count') +
  ggtitle('Origin Vs. Products over time')

ggplot(data = Auto, aes(x = year, fill = mpg01)) +
  geom_bar() +
  facet_wrap(~ origin, ncol = 1) +
  xlab('Model Year') +
  ylab('Count') +
  ggtitle('MPG Vs. Product Mix Over Time')

p3=ggplot(data = Auto, aes(x = mpg01, y = weight)) +
  geom_boxplot() +
  xlab('MPG') +
  ylab('Weight') +
  ggtitle('MPG by Weight') 

p4=ggplot(data = Auto, aes(x = cylinders, fill = mpg01)) +
  geom_bar() +
  xlab('Number of Cylinders') +
  ylab('Count') +
  ggtitle('MPG by Number of Cylinders')
pixelFigure2=ggarrange(plotlist=list(p3,p4),ncol = 2,nrow = 1)
pixelFigure2



###########################################################################################
Auto1 <- read.table(file = "Auto.csv", sep = ",", header=T)
mpg01 <- I(Auto1$mpg >= median(Auto1$mpg))*1
Auto  <- data.frame(mpg01, Auto1[,-1])
Auto$mpg01<-as.factor(Auto$mpg01)

set.seed(33)
tstIndex=sample(1:dim(Auto)[1],round(0.2*dim(Auto)[1]),replace = F)
Autotrain=Auto[-tstIndex,]
Autotest=Auto[tstIndex,]

# origins <- c('USA', 'Europe', 'Japan')
# Auto$origin <- factor(Auto$origin, labels = origins)
# Auto$mpg01<-as.factor(Auto$mpg01)
# Auto$cylinders<-as.factor(Auto$cylinders)
# Auto$year<-as.factor(Auto$year)

### Method A1: LDA
library(MASS)
fit1 <- lda(mpg01 ~ ., data= Autotrain)
pred1 <- predict(fit1,Autotrain)$class
mean(pred1  != Autotrain$mpg01) 
mean(predict(fit1,Autotest)$class !=Autotest$mpg01) 

### Method A2: QDA
library(MASS)
fit2 <- qda(mpg01 ~ ., data= Autotrain)
pred2 <- predict(fit2,Autotrain)$class
mean(pred2  != Autotrain$mpg01) 
mean(predict(fit2,Autotest)$class !=Autotest$mpg01) 

### Method A3: Naive Bayes
library(e1071)
fit3 <- naiveBayes(mpg01 ~ ., data= Autotrain)
pred3 <- predict(fit3, Autotrain)
mean( pred3 != Autotrain$mpg01) 
mean( predict(fit3,Autotest) !=Autotest$mpg01) 


fit4 <- glm(mpg01 ~ ., data= Autotrain, family = "binomial")
summary(fit4)
pred4 <- predict(fit4, Autotrain,type = 'response')
round(coef(fit4),4)
1-mean(ifelse(pred4 > 0.5, 1, 0)) 
mean( ifelse(predict(fit4,Autotest,type = 'response') > 0.5, 1, 0) !=Autotest$mpg01) 


fit5  <- step(fit4)
round(coef(fit5),3)
summary(fit5)
round(coef(fit5),4)
confint(fit5)
pred5 <- predict(fit5, Autotrain,type = 'response')
1-mean(ifelse(pred5 > 0.5, 1, 0)) 
mean( ifelse(predict(fit5,Autotest,type = 'response') > 0.5, 1, 0) !=Autotest$mpg01) 


### Method A5: SVM
library(e1071)

fit6 <- svm(mpg01 ~., data=Autotrain) 
summary(fit6)
pred6 <- predict(fit6,  Autotrain)
mean( pred6 != Autotrain$mpg01)
mean(predict(fit6,Autotest) != Autotest$mpg01) 

### Method A6: RF
fitControl <- trainControl(
  method = "repeatedcv",
  number = 3,
  repeats = 3)

rfGrid <-  expand.grid(mtry=c(1,2,3,4,5,6))
nrow(rfGrid)
set.seed(824)
rfFit <- train(mpg01 ~., data = Autotrain, 
               method = "rf", 
               trControl = fitControl, 
               verbose = FALSE,
               tuneGrid = rfGrid)

1-max(rfFit$results$Accuracy)

prerf<-predict(rfFit$finalModel,Autotest)
mean(prerf != Autotest$mpg01) 

sum(rfFit$finalModel$importance)

dfrf=data.frame(pct=round(as.vector(rfFit$finalModel$importance/1.554934),2),var=row.names(rfFit$finalModel$importance))
#row.names(dfrf)<-NULL
ggplot(data = dfrf, aes(x=var,y=pct,fill=var)) +
  geom_bar(stat = "identity") +
  #scale_y_continuous(labels=scales::percent) + 
  geom_text(data=dfrf, aes(label=paste0(pct,"%"),y=pct+1), size=4)+
  xlab('Features') +
  ylab('Percentage') +
  ggtitle('Random forest Relative feature importance')+
  theme(legend.position = "none")

### Method A7: KNN
fit7<-knn(Autotrain[,-1], Autotrain[,-1],Autotrain[,1], k = 5)
mean( fit7 != Autotrain[,1])

fit8<-knn(Autotrain[,-1], Autotest[,-1],Autotrain[,1], k = 5)
mean( fit8 != Autotest[,1])

trainKnnErrors<-c()
testKnnErrors<-c()
for (i in c(1, 3, 5, 7, 15)){
  ypred2.train <- knn(Autotrain[,-1], Autotrain[,-1], Autotrain[,1], k=i)
  ypred2.test <- knn(Autotrain[,-1], Autotest[,-1], Autotrain[,1], k=i)
  trainKnnErrors<-c(trainKnnErrors,mean( ypred2.train != Autotrain[,1]))
  testKnnErrors<-c(testKnnErrors,mean( ypred2.test != Autotest[,1]))
  print(mean( ypred2.train != Autotrain[,1]))
  print(mean( ypred2.test != Autotest[,1]))
}
trainKnnErrors
testKnnErrors

TTErrors<-data.frame(y=c(trainKnnErrors,testKnnErrors),x=c(1,3,5,7,15),Error=c(rep("Train",5),rep("Test",5)))
ggplot(data =TTErrors )+
  geom_point(aes(x=x,y=y,color=Error))+
  geom_line(aes(x=x,y=y,color=Error))+
  labs(y="Errors",x="K ",title = "KNN Errors")


### Cross-validation for muhat 
fit1Train=fit2Train=fit3Train=fit1Test=fit2Test=fit3Test=fit4Test=fit4Train<-c()
fit5Train=fit6Train=fit7Train=fit5Test=fit6Test=fit7Test=fit8Test=fit8Train<-c()
for (i in 1:10){
  #set.seed(33)
  tstIndex=sample(1:dim(Auto)[1],round(0.2*dim(Auto)[1]),replace = F)
  Autotrain=Auto[-tstIndex,]
  Autotest=Auto[tstIndex,]
  
  fit1 <- lda(mpg01 ~ ., data= Autotrain)
  pred1 <- predict(fit1,Autotrain)$class
  fit1Train<-c(fit1Train,mean(pred1  != Autotrain$mpg01))
  fit1Test<-c(fit1Test,mean(predict(fit1,Autotest)$class !=Autotest$mpg01)) 
  
  fit2 <- qda(mpg01 ~ ., data= Autotrain)
  pred2 <- predict(fit2,Autotrain)$class
  fit2Train<-c(fit2Train,mean(pred2  != Autotrain$mpg01))
  fit2Test<-c(fit2Test,mean(predict(fit2,Autotest)$class !=Autotest$mpg01)) 
  
  fit3 <- naiveBayes(mpg01 ~ ., data= Autotrain)
  pred3 <- predict(fit3, Autotrain)
  fit3Train<-c(fit3Train,mean( pred3 != Autotrain$mpg01))
  fit3Test<-c(fit3Test,mean( predict(fit3,Autotest) !=Autotest$mpg01))
  
  fit4 <- glm(mpg01 ~ ., data= Autotrain, family = "binomial")
  pred4 <- predict(fit4, Autotrain,type = 'response')
  fit4Test<-c(fit4Test,1-mean(ifelse(pred4 > 0.5, 1, 0))) 
  fit4Train<-c(fit4Train,mean( ifelse(predict(fit4,Autotest,type = 'response') > 0.5, 1, 0) !=Autotest$mpg01)) 
  
  fit5  <- step(fit4,trace = -1)
  pred5 <- predict(fit5, Autotrain,type = 'response')
  fit5Train<-c(fit5Train,1-mean(ifelse(pred5 > 0.5, 1, 0)))
  fit5Test<-c(fit5Test,mean( ifelse(predict(fit5,Autotest,type = 'response') > 0.5, 1, 0) !=Autotest$mpg01)) 
  
  fit6 <- svm(mpg01 ~., data=Autotrain) 
  pred6 <- predict(fit6,  Autotrain)
  fit6Train<-c(fit6Train,mean( pred6 != Autotrain$mpg01))
  fit6Test<-c(fit6Test,mean(predict(fit6,Autotest) != Autotest$mpg01)) 
  
  rfFit <- train(mpg01 ~., data = Autotrain, 
                 method = "rf", 
                 trControl = fitControl, 
                 verbose = FALSE,
                 tuneGrid = rfGrid)
  
  fit7Train<-c(fit7Train,1-max(rfFit$results$Accuracy))
  prerf<-predict(rfFit$finalModel,Autotest)
  fit7Test<-c(fit7Test,mean(prerf != Autotest$mpg01))
  
  fit7<-knn(Autotrain[,-1], Autotrain[,-1],Autotrain[,1], k = 5)
  fit8Train<-c(fit8Train,mean( fit7 != Autotrain[,1]))
  
  fit8<-knn(Autotrain[,-1], Autotest[,-1],Autotrain[,1], k = 5)
  fit8Test<-c(fit8Test,mean( fit8 != Autotest[,1]))
}
mean(fit7Test)
var(fit7Test)



