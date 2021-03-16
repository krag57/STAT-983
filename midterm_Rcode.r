library(kernlab)
library(caret)
library(randomForest)
library(e1071)
library(partykit)

### Read Training Data
midterm<- read.table(file = "midtermtrain.csv", sep=",");

testIndex=sample(1:2911,size = 200,replace = F)

### Train and test split
midtermtrain<-midterm[-testIndex,]
midtermtest<-midterm[testIndex,]
dim(midtermtrain);

###  exploratory variable
X1 <- midtermtrain[,1];
X2 <- midtermtrain[,2];

#X1 <- midtermtest[,1];
#X2 <- midtermtest[,2];


## note that muhat = E(Y) and Vhat = 100*Var(Y)
muhat <- apply(midtermtrain[,3:202], 1, mean);
Vhat  <- 100*apply(midtermtrain[,3:202], 1, var);

## we can plot 4 graphs in a single plot
par(mfrow = c(2, 2));
plot(X1, muhat); 
plot(X2, muhat); 
plot(X1, Vhat); 
plot(X2, Vhat);
dev.off()

### Train and Test set
train<-data.frame(muhat,Vhat,X1,X2)
test<-data.frame(muhat,Vhat,X1,X2)

### Models for muhat
fit1<-lm(muhat~X1+X2,data = train)
summary(fit1)
sqrt(mean((resid(fit1))^2))
RMSE(predict(fit1,test),test$muhat)

fit2<-lm(muhat~poly(X1,2)+poly(X2,2),data = train)
summary(fit2)
sqrt(mean((resid(fit2))^2))
RMSE(predict(fit2,test),test$muhat)

fit3<-lm(muhat~poly(X1,3)+poly(X2,3),data = train)
summary(fit3)
sqrt(mean((resid(fit3))^2))
RMSE(predict(fit3,test),test$muhat)


fitControl <- trainControl(
  method = "repeatedcv",
  number = 2,
  repeats = 2)

rfGrid <-  expand.grid(mtry=c(1,2))
nrow(rfGrid)
set.seed(824)
rfFit <- train(muhat ~ X1+X2, data = train, 
               method = "rf", 
               trControl = fitControl, 
               verbose = FALSE,
               tuneGrid = rfGrid)

prerf<-predict(rfFit$finalModel,test[,3:4])
RMSE(prerf,test$muhat)
rfFit$results$Rsquared
rfFit$results$RMSE

### Models for Vhat
fita<-lm(Vhat~X1+X2,data = train)
summary(fita)
sqrt(mean((resid(fita))^2))
RMSE(predict(fita,test),test$Vhat)

fitb<-lm(Vhat~poly(X1,2)+poly(X2,2),data = train)
summary(fitb)
sqrt(mean((resid(fitb))^2))
RMSE(predict(fitb,test),test$Vhat)

fitc<-lm(Vhat~poly(X1,3)+poly(X2,3),data = train)
summary(fitc)
sqrt(mean((resid(fitc))^2))
RMSE(predict(fitc,test),test$Vhat)


set.seed(823)
rfFit2 <- train(Vhat ~ X1+X2, data = , 
               method = "rf", 
               trControl = fitControl, 
               verbose = FALSE, 
               ## Now specify the exact models 
               ## to evaluate:
               tuneGrid = rfGrid)

prerf2<-predict(rfFit2$finalModel,test[,3:4])
RMSE(prerf2,test$Vhat)
rfFit2$results$Rsquared
rfFit2$results$RMSE


### Testing Data

midtermtest  <- read.table(file = "midtermtest.csv", sep=",");
dim(midtermtest)

### Cross-validation for muhat 
fit1Train=fit2Train=fit3Train=fit1Test=fit2Test=fit3Test=fit4Test=fit4Train<-c()

for (i in 1:10){
  X1 <- midterm[,1];
  X2 <- midterm[,2];
  
  muhat <- apply(midterm[,3:202], 1, mean);
  Vhat  <- 100*apply(midterm[,3:202], 1, var);
  boot<-data.frame(muhat,Vhat,X1,X2)
  
  bootindex<-sample(1:2911,size = 300,replace = F)
  bootTrian<-boot[-bootindex,]
  bootTest<-boot[bootindex,]
  
  
  fit1<-lm(muhat~X1+X2,data = bootTrian)
  summary(fit1)
  fit1Train<-c(fit1Train,sqrt(mean((resid(fit1))^2)))
  fit1Test<-c(fit1Test,RMSE(predict(fit1,bootTest),bootTest$muhat))
  
  fit2<-lm(muhat~poly(X1,2)+poly(X2,2),data = bootTrian)
  summary(fit2)
  fit2Train<-c(fit2Train,sqrt(mean((resid(fit2))^2)))
  fit2Test<-c(fit2Test,RMSE(predict(fitb,bootTest),bootTest$muhat))
  
  fit3<-lm(muhat~poly(X1,3)+poly(X2,3),data = bootTrian)
  summary(fit3)
  fit3Train<-c(fit3Train,sqrt(mean((resid(fit3))^2)))
  fit3Test<-c(fit3Test,RMSE(predict(fitc,bootTest),bootTest$muhat))
  
  rfFitb <- train(muhat ~ X1+X2, data = train, 
                 method = "rf", 
                 trControl = fitControl, 
                 verbose = FALSE,
                 tuneGrid = rfGrid)
  
  prerfb<-predict(rfFitb$finalModel,bootTest[,3:4])
  fit4Test<-c(fit4Test,RMSE(prerfb,bootTest$muhat))
  fit4Train<-c(fit4Train,rfFitb$results$RMSE)
}
mean(fit4Train)

### Cross-validation for Vhat 
fitaTrain=fitbTrain=fitcTrain=fitaTest=fitbTest=fitcTest=fitdTest=fitdTrain<-c()

for (i in 1:10){
  X1 <- midterm[,1];
  X2 <- midterm[,2];
  
  muhat <- apply(midterm[,3:202], 1, mean);
  Vhat  <- 100*apply(midterm[,3:202], 1, var);
  boot<-data.frame(muhat,Vhat,X1,X2)
  
  bootindex<-sample(1:2911,size = 300,replace = F)
  bootTrian<-boot[-bootindex,]
  bootTest<-boot[bootindex,]
  
  
  fita<-lm(Vhat~X1+X2,data = bootTrian)
  summary(fita)
  fitaTrain<-c(fitaTrain,sqrt(mean((resid(fita))^2)))
  fitaTest<-c(fitaTest,RMSE(predict(fita,bootTest),bootTest$Vhat))
  
  fitb<-lm(Vhat~poly(X1,2)+poly(X2,2),data = bootTrian)
  summary(fitb)
  fitbTrain<-c(fitbTrain,sqrt(mean((resid(fitb))^2)))
  fitbTest<-c(fitbTest,RMSE(predict(fitb,bootTest),bootTest$Vhat))
  
  fitc<-lm(Vhat~poly(X1,3)+poly(X2,3),data = bootTrian)
  summary(fitc)
  fitcTrain<-c(fitcTrain,sqrt(mean((resid(fitc))^2)))
  fitcTest<-c(fitcTest,RMSE(predict(fitc,bootTest),bootTest$Vhat))
  
  rfFitb <- train(Vhat ~ X1+X2, data = train, 
                  method = "rf", 
                  trControl = fitControl, 
                  verbose = FALSE,
                  tuneGrid = rfGrid)
  
  prerfb<-predict(rfFitb$finalModel,bootTest[,3:4])
  fitdTest<-c(fitdTest,RMSE(prerfb,bootTest$Vhat))
  fitdTrain<-c(fitdTrain,rfFitb$results$RMSE)
}
mean(fitcTest)
mean(fitcTrain)

### Test set invoking
midtermtest  <- read.table(file = "midtermtest.csv", sep=",");
names(midtermtest)<-c("X1","X2")

### Prediction
prerf<-predict(rfFit$finalModel,midtermtest)
prerf2<-predict(rfFit2$finalModel,midtermtest)

### Predictions and predictors
Results<-data.frame(midtermtest,muhat=round(prerf,6),Vhat=round(prerf2,6))

mean(prerf)
#0.283836
var(prerf)
#0.014495
mean(prerf2)
#0.008542836
var(prerf2)
#0.000426

### Output is written to a file
write.table(Results, "results.csv", row.names=F, col.names=F, sep=",")
print(rfFit$finalModel)



