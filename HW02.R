#Hmisc::latex(tb.knn10, file = "./tables/cm_knn10.tex")
setwd("~/Documents/Class/STAT-983")
library(Hmisc)
library(lattice)
library(broom)
library(tidyverse)

fat <- read.table(file = "fat.csv", sep=",", header=TRUE)
n = dim(fat)[1] 
n1 = round(n/10) 

set.seed(06)
flag = sort(sample(1:n, n1))

flag = c(1, 21, 22, 57, 70, 88, 91, 94, 121, 127, 149, 151, 159, 162,
         164, 177, 179, 194, 206, 214, 215, 221, 240, 241, 243);
fat1train = fat[-flag,] 
fat1test = fat[flag,]

splom(fat1train[,7:18], pscales = 0)
summary(fat1train[,-(1:2)])
par(mfrow = c(4,4))
for (i in 3:18) boxplot(fat1train[,i], xlab=names(fat1train)[i])
dev.off()

cor1=round(cor(fat1train[,7:18]),2)
write.csv(cor1, file = "corr.csv")

# Fit a linear regression on the training data
model0 <- lm(brozek ~ .-siri, data = fat1train); 
summary(model0); 

X <- model.matrix(model0);
eign1 <- eigen( t(X) %*% X); 
round(eign1$val,2)

CI<-sort(sqrt( eign1$val[1] / eign1$val),decreasing = T)[1:5]
write.csv(round(CI,3), file = "CI.csv")

VIF1 <- NULL;
X <- model.matrix(model0);
for (i in 2:17) VIF1 <- cbind(VIF1, 1/(1-summary(lm(X[,i] ~ X[,-i]))$r.squared)); 
colnames(VIF1) <- colnames(fat1train)[3:18]
write.csv(round(VIF1,3), file = "VIF1.csv")



model0a <- lm(brozek + 0.1*rnorm(227) ~ .-siri, data= fat1train); 
summary(model0a);

c(summary(model0a)$r.squared, summary(model0)$r.squared)
#[1] 0.6987397 0.6943712
c(summary(model0a)$sig, summary(model0)$sigma)
#[1] 0.7175067 0.7122861

cbind(coef(model0a), coef(model0))
round(cbind(coef(model0a), coef(model0),abs((coef(model0a)-coef(model0))/coef(model0a))),4)

#neck and ankle
model0b <- lm(brozek ~ .-siri-ankle, data= fat1train)

c(summary(model0b)$r.squared, summary(model0)$r.squared)
#[1] 0.6942578 0.6943712
round(c(summary(model0b)$sig, summary(model0)$sigma),4)


#######################
ytrue=fat1test$brozek
model1 <- lm(brozek ~ .-siri, data = fat1train); 
MSE1mod1 <-   mean((resid(model1) )^2) 
kj=tidy((model1)) %>% data.frame()
kj[-1,2]
paste(colnames(train)[1], "~",paste(colnames(train)[-1], collapse = kj[-1,2]),sep = "")
write.csv(round(model1$coefficients,4), file = "coef.csv")
# testign error 
pred1a <- predict(model1, fat1test)
MSE2mod1 <-   mean((pred1a - ytrue)^2)


########################
### (ii) Linear regression with the best subset model 
library(leaps);
leaps <- regsubsets(brozek ~ .-siri, data= fat1train, nbest= 100, really.big= TRUE,nvmax = 16); 
models <- summary(leaps)$which
models.size <- as.numeric(attr(models, "dimnames")[[1]]);
models.rss <- summary(leaps)$rss;
plot(models.size, models.rss); 

# add the best subset and results for the only intercept model

models.best.rss <- tapply(models.rss, models.size, min); 
model0 <- lm( brozek ~ 1, data= fat1train); 
models.best.rss <- c( sum(resid(model0)^2), models.best.rss); 

plot( 0:16, models.best.rss, type = "b", col= "red", xlab="Subset Size k", ylab="Residual Sum-of-Square")
points(models.size, models.rss)


## What is the best subset with k=5

op2 <- which(models.size == 5); 
flag2 <- op2[which.min(models.rss[op2])]; 

## Manual look at the selected model
models[flag2,]
model2 <- lm( brozek ~ density + weight + adipos+free+chest, data = fat1train)

## Auto-find the best subset with k=5
mod2selectedmodel <- models[flag2,]; 
mod2Xname <- paste(names(mod2selectedmodel)[mod2selectedmodel][-1], collapse="+"); 
mod2form <- paste ("brozek ~", mod2Xname);
model2 <- lm( as.formula(mod2form), data= fat1train); 

# traning and testing errors 
summary(model2);
MSE1mod2 <- mean(resid(model2)^2);
pred2 <- predict(model2, fat1test);
MSE2mod2 <-   mean((pred2 - ytrue)^2)

###################################

## use "step()" function based on the AIC criterion
## we need to use the full model first

model1 <- lm(brozek ~ .-siri, data= fat1train); 
model3  <- step(model1); 

## see the coefficents of \beta of model3
round(coef(model3),3)
summary(model3)

## 
# traning and testing errors 
MSE1mod3 <- mean(resid(model3)^2);
pred3 <- predict(model3, fat1test);
MSE2mod3 <-   mean((pred3 - ytrue)^2);


#######################################
### (iv) Ridge regression (MASS: lm.ridge, mda: gen.ridge)

library(MASS);
ridge <- lm.ridge( brozek ~ .-siri, data= fat1train, lambda= seq(0,100,0.01));
plot(ridge) 
### Or try "matplot" to plot the columns of one matrix against the columns of another
matplot(ridge$lambda, t(ridge$coef), type="l", lty=1, 
        xlab=expression(lambda), ylab=expression(hat(beta)))

## (a) manually find the optimal lambda value
MASS::select(ridge)
## 
#modified HKB estimator is 3.355691 
#modified L-W estimator is 3.050708 
# smallest value of GCV  at 4.92 

## the search region of lambda can be different with different step size!!!
##   in some applications step size = 0.01 might be too crude.
ridge2 <- lm.ridge( brozek ~ .-siri, data= fat1train, lambda= seq(0,10,0.001));
select(ridge2)

# Using lambda = 4.92, we can get the best model:
abline(v=0.05)

### Compare the ceofficients of ridge regression vs. linear regression
##           on the scaled data set
ridge$coef[, which(ridge$lambda == 0.05)]
ridge$coef[, which(ridge$lambda == 0)]

## (b) Auto-find the optimal lambda value
lambdaopt <- which.min(ridge$GCV); 

### if you want to see the coefficient of ridge on the original, raw data scale
mod4.coef <- coef(ridge)[lambdaopt,]
round(mod4.coef, 4)

### There are three ways for prediction in Ridge
### Here is a (little more complicated?) way from the scaled coefficients
### 

## The coefficient of ridge on the scaled data
rig1coef <- ridge$coef[,lambdaopt];
## find the intercepts using ybar and xbar from training data
rig1intercepts <- ridge$ym - sum(ridge$xm * (rig1coef / ridge$scales)); 

pred4 <- scale(fat1test[,-(1:2)], center = F, scale = ridge$scales)%*%  rig1coef + rig1intercepts;
MSE2mod4 <- mean( (pred4 - ytrue)^2);
pred8 <- scale(fat1train[,-(1:2)], center = F, scale = ridge$scales)%*%  rig1coef + rig1intercepts;
MSE2mod8 <- mean( (pred8 - ytrain)^2);
MSE2mod8
MSE2mod4
##[1] 0.4943582


## V. LASSO 
## IMPORTANT: You need to install the R package "lars" beforehand
lambdas <- seq(0.001,100,len=100)

##
library(lars)
lars <- lars(as.matrix(fat1train[,-(1:2)]), fat1train[,1], type= "lasso",
             trace= TRUE,normalize=T); 
plot(lars)

## select the path with the smallest Cp 
Cp1  <- summary(lars)$Cp
index1 <- which.min(Cp1)

## Three ways to get beta values
coef(lars)[index1,]
lars$beta[index1,]

lasso.lambda <- lars$lambda[index1]
coef.lars1 <- predict(lars, s=lasso.lambda, type="coef", mode="lambda")
coef.lars1$coef
write.csv(round(coef.lars1$coef, 4),'coeflasso.csv')
## Fitted value
## training error for lasso
fit5 <- predict(lars, as.matrix(fat1train[,-(1:2)]), s=lasso.lambda, type="fit", mode="lambda");
yhat5 <- fit5$fit; 
MSE1mod5 <- mean( (yhat5 - fat1train$brozek)^2);  
MSE1mod5;

## 0.4398267  training error for lasso 
### test error for lasso 

fit5b <- predict(lars, as.matrix(fat1test[,-(1:2)]), s=lasso.lambda, type="fit", mode="lambda");
yhat5b <- fit5b$fit; 
MSE2mod5 <- mean( (yhat5b - fat1test$brozek)^2); 
MSE2mod5;
## 0.5074249  test error for lasso

#play with codes
coef.lars(lars, s=lasso.lambda, mode="lambda")
names(lars);


## Compare MSE for OLS, Ridge and LASSO
## Traning + Testing data
data.frame(Train=c(MSE1mod1, MSE1mod2, MSE1mod3,MSE2mod8, MSE1mod5),
Test=c(MSE2mod1, MSE2mod2, MSE2mod3,MSE2mod4, MSE2mod5))



## Training erros
#[1] 0.4391998 0.5210112 0.4393627 0.4473565 0.4398267
## testing errors
#[1] 0.5212740 0.4005308 0.5165135 0.4943582 0.5074249
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
      plot(ridge)
      
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

RidgeBoot<-bootModelError(fat2,testSize=25,method='ridge',boots=10,subsetSize=5)
LassoBoot<-bootModelError(fat2,testSize=25,method='lasso',boots=10,subsetSize=5)
StepBoot<-bootModelError(fat2,testSize=25,method='step',boots=10,subsetSize=5)
SubsetBoot<-bootModelError(fat2,testSize=25,method='subset',boots=10,subsetSize=5)
fullBoot<-bootModelError(fat2,testSize=25,method='full',boots=10,subsetSize=5)

errorData<-data.frame(TrainError=c(fullBoot$trainErrors,SubsetBoot$trainErrors,StepBoot$trainErrors,RidgeBoot$trainErrors,LassoBoot$trainErrors),
TestError=c(fullBoot$testErrors,SubsetBoot$testErrors,StepBoot$testErrors,RidgeBoot$testErrors,LassoBoot$testErrors),
Method=c(rep("Full",10),rep("Subset",10),rep("Step",10),rep("Lasso",10),rep("Ridge",10)),
Sample=c(rep(1:10,times=5)))


# Test and Train error of KNN
#TTErrors<-data.frame(y=c(trainKnnErrors,testKnnErrors),x=c(1,3,5,7,15),Error=c(rep("Train",5),rep("Test",5)))
ggplot(data =errorData)+
  geom_point(aes(x=Sample,y=TrainError,color=Method))+
  geom_line(aes(x=Sample,y=TrainError,color=Method))+
  labs(y="Errors",x="Bootstrap samples ",title = "Test Errors")

ggsave("Boot erors.jpg",plot = last_plot()) 


dfe=data.frame(meanTest=round(c(fullBoot$meanTrEr,SubsetBoot$meanTrEr,StepBoot$meanTrEr,RidgeBoot$meanTrEr,LassoBoot$meanTrEr),4),
varTest=round(c(fullBoot$varTrEr,SubsetBoot$varTrEr,StepBoot$varTrEr,RidgeBoot$varTrEr,LassoBoot$varTrEr),4))

write.csv(dfe,'dfe.csv')

