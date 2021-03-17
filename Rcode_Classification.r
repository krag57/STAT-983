
## Example A: The Vowel data set from our textbook, also see the homepage

https://statweb.stanford.edu/~tibs/ElemStatLearn/datasets/vowel.info.txt

# read Table

vowel.train <- read.table("vowel.train.csv",  header= TRUE, sep = ",");

vowel.test <- read.table("vowel.test.csv",  header= TRUE, sep = ",");



##
vowel.train$y <- as.factor(vowel.train$y); 

### Method A1: LDA

library(MASS)
# fit1 <- lda( y ~ ., data= vowel.train, CV= TRUE)
fit1 <- lda(vowel.train[,2:11], vowel.train[,1])
##

pred1 <- predict(fit1,vowel.train[,2:11])$class
mean( pred1  != vowel.train$y) 
## 0.3162879 for miss.class.train.error

mean( predict(fit1,vowel.test[,2:11])$class != vowel.test$y) 
## 0.5562771 for miss.class.test.error


## Method A2: QDA
fit2 <- qda(vowel.train[,2:11], vowel.train[,1])
##
pred2 <- predict(fit2,vowel.train[,2:11])$class
mean( pred2!= vowel.train$y) 
## 0.01136364 for miss.class.train.error

mean( predict(fit2,vowel.test[,2:11])$class != vowel.test$y) 
## 0.5281385 for miss.class.test.error

## Method A3: Naive Bayes

library(e1071)
fit3 <- naiveBayes( vowel.train[,2:11], vowel.train[,1])
pred3 <- predict(fit3, vowel.train[,2:11]);
mean( pred3 != vowel.train$y) 
##  0.2765152 for miss.class.train.error

mean( predict(fit3,vowel.test[,2:11]) != vowel.test$y) 
##  0.5324675 for miss.class.test.error



### Method A4: (multinomial) logisitic regression) 
## You need to install the library "nnet" first 

library(nnet)
fit4 <- multinom( y ~., data=vowel.train) 
summary(fit4);
pred4<- predict(fit4, vowel.train[,2:11])
table(pred4,  vowel.train$y) 
mean( pred4 != vowel.train$y)
##  0.2215909 for miss.class.train.error

mean( predict(fit4,vowel.test[,2:11]) != vowel.test$y) 
##  0.512987 for miss.class.test.error


### Method A5: SVM
## You need to install the library "e1071" first 

library(e1071)

fit5 <- svm(y ~., data=vowel.train) 
summary(fit5);
pred5 <- predict(fit5,  vowel.train[,2:11])
mean( pred5 != vowel.train$y)

## 0.02462121 for miss.class.train.error
mean( predict(fit5,vowel.test[,2:11]) != vowel.test$y) 
##  0.3874459 for miss.class.test.error

##########
## For the vowel dataset, among all above 5 classification methods,
##  svm has the smallest mis-classification rate
##########


