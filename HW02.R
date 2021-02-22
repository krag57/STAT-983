setwd("~/Documents/Class/STAT-983")
library(Hmisc)
library(lattice)
library(broom)
library(tidyverse)
library(leaps)
library(MASS)
library(lars)

fat <- read.table(file = "fat.csv", sep=",", header=TRUE)
n = dim(fat)[1] 
n1 = round(n/10) 
set.seed(06)
flag = sort(sample(1:n, n1))

flag = c(1, 21, 22, 57, 70, 88, 91, 94, 121, 127, 149, 151, 159, 162,
         164, 177, 179, 194, 206, 214, 215, 221, 240, 241, 243);
fat1train = fat[-flag,] 
fat1test = fat[flag,]

#Multi-collinearity
splom(fat1train[,7:18], pscales = 0)
summary(fat1train[,-(1:2)])
par(mfrow = c(4,4))
for (i in 3:18) boxplot(fat1train[,i], xlab=names(fat1train)[i])
dev.off()

#correalation
cor1=round(cor(fat1train[,7:18]),2)
write.csv(cor1, file = "corr.csv")

# Fit a linear regression on the training data
model0 <- lm(brozek ~ .-siri, data = fat1train); 
summary(model0); 
X <- model.matrix(model0);
eign1 <- eigen( t(X) %*% X); 
round(eign1$val,2)

#Condition Index
CI<-sort(sqrt( eign1$val[1] / eign1$val),decreasing = T)[1:5]
write.csv(round(CI,3), file = "CI.csv")

#VIF
VIF1 <- NULL;
X <- model.matrix(model0);
for (i in 2:17) VIF1 <- cbind(VIF1, 1/(1-summary(lm(X[,i] ~ X[,-i]))$r.squared)); 
colnames(VIF1) <- colnames(fat1train)[3:18]
write.csv(round(VIF1,3), file = "VIF1.csv")


### Models

#(i) Full model; Linear regression
ytrue=fat1test$brozek
model1 <- lm(brozek ~ .-siri, data = fat1train); 
MSE1mod1 <-   mean((resid(model1) )^2) 
kj=tidy((model1)) %>% data.frame()
kj[-1,2]
paste(colnames(train)[1], "~",paste(colnames(train)[-1], collapse = kj[-1,2]),sep = "")
write.csv(round(model1$coefficients,4), file = "coef.csv")
pred1a <- predict(model1, fat1test)
MSE2mod1 <-   mean((pred1a - ytrue)^2)

#(ii) Linear regression with the best subset model 
leaps <- regsubsets(brozek ~ .-siri, data= fat1train, nbest= 100, really.big= TRUE,nvmax = 16); 
models <- summary(leaps)$which
models.size <- as.numeric(attr(models, "dimnames")[[1]])
models.rss <- summary(leaps)$rss
plot(models.size, models.rss)

models.best.rss <- tapply(models.rss, models.size, min); 
model0 <- lm( brozek ~ 1, data= fat1train); 
models.best.rss <- c( sum(resid(model0)^2), models.best.rss); 

plot( 0:16, models.best.rss, type = "b", col= "red", xlab="Subset Size k", ylab="Residual Sum-of-Square")
points(models.size, models.rss)

op2 <- which(models.size == 5); 
flag2 <- op2[which.min(models.rss[op2])]; 

mod2selectedmodel <- models[flag2,]; 
mod2Xname <- paste(names(mod2selectedmodel)[mod2selectedmodel][-1], collapse="+"); 
mod2form <- paste ("brozek ~", mod2Xname);
model2 <- lm( as.formula(mod2form), data= fat1train); 

summary(model2);
MSE1mod2 <- mean(resid(model2)^2);
pred2 <- predict(model2, fat1test);
MSE2mod2 <-   mean((pred2 - ytrue)^2)

#(iii) Best model stepwise
model1 <- lm(brozek ~ .-siri, data= fat1train); 
model3  <- step(model1); 

round(coef(model3),3)
summary(model3)

MSE1mod3 <- mean(resid(model3)^2);
pred3 <- predict(model3, fat1test);
MSE2mod3 <-   mean((pred3 - ytrue)^2);

#(iv) Ridge regression
ridge <- lm.ridge( brozek ~ .-siri, data= fat1train, lambda= seq(0,100,0.01));

matplot(ridge$lambda, t(ridge$coef), type="l", lty=1, 
        xlab=expression(lambda), ylab=expression(hat(beta)))

MASS::select(ridge)

lambdaopt <- which.min(ridge$GCV); 
mod4.coef <- coef(ridge)[lambdaopt,]
round(mod4.coef, 4)

rig1coef <- ridge$coef[,lambdaopt];

rig1intercepts <- ridge$ym - sum(ridge$xm * (rig1coef / ridge$scales)); 

pred4 <- scale(fat1test[,-(1:2)], center = F, scale = ridge$scales)%*%  rig1coef + rig1intercepts;
MSE2mod4 <- mean( (pred4 - ytrue)^2);
pred8 <- scale(fat1train[,-(1:2)], center = F, scale = ridge$scales)%*%  rig1coef + rig1intercepts;
MSE2mod8 <- mean( (pred8 - ytrain)^2);
MSE2mod8
MSE2mod4

#(v) LASSO 
lambdas <- seq(0.001,100,len=100)

library(lars)
lars <- lars(as.matrix(fat1train[,-(1:2)]), fat1train[,1], type= "lasso",
             trace= TRUE,normalize=T); 
plot(lars)

Cp1  <- summary(lars)$Cp
index1 <- which.min(Cp1)

coef(lars)[index1,]
lars$beta[index1,]

lasso.lambda <- lars$lambda[index1]
coef.lars1 <- predict(lars, s=lasso.lambda, type="coef", mode="lambda")
coef.lars1$coef
write.csv(round(coef.lars1$coef, 4),'coeflasso.csv')

fit5 <- predict(lars, as.matrix(fat1train[,-(1:2)]), s=lasso.lambda, type="fit", mode="lambda");
yhat5 <- fit5$fit; 
MSE1mod5 <- mean( (yhat5 - fat1train$brozek)^2);  
MSE1mod5;

fit5b <- predict(lars, as.matrix(fat1test[,-(1:2)]), s=lasso.lambda, type="fit", mode="lambda");
yhat5b <- fit5b$fit; 
MSE2mod5 <- mean( (yhat5b - fat1test$brozek)^2); 
MSE2mod5;

## Comparing Models
data.frame(Train=c(MSE1mod1, MSE1mod2, MSE1mod3,MSE2mod8, MSE1mod5),
Test=c(MSE2mod1, MSE2mod2, MSE2mod3,MSE2mod4, MSE2mod5))


## Monte Carlo cross validation.
fat2<-fat[,-2]
test=fat2test<-fat1test[,-2]
train=fat2train<-fat1train[,-2]

bootModelError<-function(data,testSize=4,method='full',boots,subsetSize=5){
  require(leaps)
  require(lars)
  require(MASS)
  n=dim(data)[1]
  trainErrors<-rep(-99,boots)
  testErrors<-rep(-99,boots)
  for (i in 1:boots){
    testIndex=sort(sample(1:n,replace = F,size = testSize))
    train=data[-testIndex,]
    test=data[testIndex,]
    ytrue=test[,1]
    ytrain=train[,1]
    formulae<-as.formula(paste(colnames(train)[1], "~",
                               paste(colnames(train)[-1], collapse = "+"),
                               sep = ""))
    
    
    if(method=='full'){
      model=lm(formulae,data = train)
      trainMSE=mean((resid(model))^2)
      pred<-predict(model, test)
      testMSE<-mean((pred-ytrue)^2)
      
      trainErrors[i]<-trainMSE
      testErrors[i]<-testMSE
      
      
    }
    
    else if(method=='subset'){
      leaps <- regsubsets(formulae, nbest= 100,data= train, really.big= TRUE,nvmax =dim(data)[2]-1); 
      models <- summary(leaps)$which
      models.size <- as.numeric(attr(models, "dimnames")[[1]]);
      models.rss <- summary(leaps)$rss
      
      op2 <- which(models.size == subsetSize)
      flag2 <- op2[which.min(models.rss[op2])]

      mod2selectedmodel <- models[flag2,]; 
      mod2Xname <- paste(names(mod2selectedmodel)[mod2selectedmodel][-1], collapse="+"); 
      mod2form <- paste (colnames(train)[1],"~", mod2Xname);
      model2 <- lm( as.formula(mod2form), data= train); 
      
      # traning and testing errors 
      trainMSE=mean((resid(model2))^2)
      pred<-predict(model2, test)
      testMSE<-mean((pred-ytrue)^2)
      
      trainErrors[i]<-trainMSE
      testErrors[i]<-testMSE
    }
    else if(method=='step'){
      model=lm(formulae,data = train)
      model3  <- step(model,trace = F)
      
      # traning and testing errors 
      trainMSE=mean((resid(model3))^2)
      pred<-predict(model3, test)
      testMSE<-mean((pred-ytrue)^2)
      
      trainErrors[i]<-trainMSE
      testErrors[i]<-testMSE
    }
    else if(method=='ridge'){
      ridge <- lm.ridge( formulae, data =train, lambda= seq(0,100,0.01));
      #plot(ridge)
      
      lambdaopt <- which.min(ridge$GCV); 
      
      rig1coef <- ridge$coef[,lambdaopt];
      
      rig1intercepts <- ridge$ym - sum(ridge$xm * (rig1coef / ridge$scales)); 
      pred <- scale(test[,-1], center = F, scale = ridge$scales)%*%  rig1coef + rig1intercepts;
      fitt <- scale(train[,-1], center = F, scale = ridge$scales)%*%  rig1coef + rig1intercepts
      
      trainMSE<-mean((fitt - ytrain)^2)
      testMSE <- mean((pred - ytrue)^2)
      
      trainErrors[i]<-trainMSE
      testErrors[i]<-testMSE
    }
    
    else if(method=='lasso'){
      lars <- lars(as.matrix(train[,-1]), train[,1], type= "lasso", trace= F)
      Cp1  <- summary(lars)$Cp
      index1 <- which.min(Cp1)
      
      fit5 <- predict(lars, as.matrix(train[,-1]), s=lasso.lambda, type="fit", mode="lambda")
      yhat5 <- fit5$fit
      trainMSE<- mean((yhat5 - ytrain)^2)
      
      fit5b <- predict(lars, as.matrix(test[,-1]), s=lasso.lambda, type="fit", mode="lambda");
      yhat5b <- fit5b$fit
      testMSE <- mean((yhat5b - ytrue)^2)
      
      trainErrors[i]<-trainMSE
      testErrors[i]<-testMSE
    }
  
  }
  
  return(list('method'=method,
  'meanTrEr'=mean(trainErrors),
  'meanTeEr'=mean(testErrors),
  'varTrEr'=var(trainErrors),
  'varTeEr'=var(testErrors),
  'trainErrors'=trainErrors,
  'testErrors'=testErrors)
  )
}

#Results from cross-validation
RidgeBoot<-bootModelError(fat2,testSize=50,method='ridge',boots=100,subsetSize=5)
LassoBoot<-bootModelError(fat2,testSize=50,method='lasso',boots=100,subsetSize=5)
StepBoot<-bootModelError(fat2,testSize=50,method='step',boots=100,subsetSize=5)
SubsetBoot<-bootModelError(fat2,testSize=50,method='subset',boots=100,subsetSize=5)
fullBoot<-bootModelError(fat2,testSize=50,method='full',boots=100,subsetSize=5)

errorData<-data.frame(TrainError=c(fullBoot$trainErrors,SubsetBoot$trainErrors,StepBoot$trainErrors,RidgeBoot$trainErrors,LassoBoot$trainErrors),
TestError=c(fullBoot$testErrors,SubsetBoot$testErrors,StepBoot$testErrors,RidgeBoot$testErrors,LassoBoot$testErrors),
Method=c(rep("Full",100),rep("Subset",100),rep("Step",100),rep("Lasso",100),rep("Ridge",100)),
Sample=c(rep(1:100,times=5)))

ggplot(data =errorData)+
  geom_point(aes(x=Sample,y=TestError,color=Method))+
  geom_line(aes(x=Sample,y=TestError,color=Method))+
  labs(y="Errors",x="Bootstrap samples ",title = "Test Errors")

ggsave("Boot erors.jpg",plot = last_plot()) 


dfe=data.frame(meanTest=round(c(fullBoot$meanTeEr,SubsetBoot$meanTeEr,StepBoot$meanTeEr,RidgeBoot$meanTeEr,LassoBoot$meanTeEr),4),
varTest=round(c(fullBoot$varTeEr,SubsetBoot$varTeEr,StepBoot$varTeEr,RidgeBoot$varTeEr,LassoBoot$varTeEr),4))

write.csv(dfe,'dfe.csv')

