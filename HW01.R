setwd("~/Documents/Class/STAT-983")

library(ggplot2)
library(gridExtra)
library(ggpubr)
library(reshape2)
library(ggcorrplot)
library(class)

ziptrain <- read.table("zip.train.csv", sep = ",")
ziptrain27 <- subset(ziptrain, ziptrain[,1]==2 | ziptrain[,1]==7)

#### EDA
dim(ziptrain27)
#[1] 1376  257
round(cor(ziptrain27),2)
table(ziptrain27[,1])
#2   7 
#731 645


n=10
plots <- vector("list", n)
set.seed(11)
rowIndex<-sample(1:length(ziptrain27),size = n)
for (i in 1:n){
  xVal = t(matrix(data.matrix(ziptrain27[rowIndex[i],-1]),byrow=TRUE,16,16)[16:1,])
  gg <- melt(data.frame(x=1:16,xVal),id="x")
  plots[[i]]<-ggplot(gg) + geom_raster(aes(x=x,y=variable,fill=value))+
    labs(x = paste0("label = ",ziptrain27[rowIndex[i],1]))+
    scale_x_continuous(expand=c(0,0))+
    scale_fill_continuous(type = "gradient")+
    guides(fill=FALSE)+                  
    theme(axis.text = element_blank(),
          axis.title.x=element_text(face = "bold"),
          axis.ticks=element_blank(),
          axis.title=element_blank())
}
pixelFigure=ggarrange(plotlist=plots,ncol = 5,nrow = 2)
annotate_figure(pixelFigure,top = text_grob("Images training sample",face = "bold",size = 16,hjust = 1.5,vjust=1))
ggsave("pixelFigure.jpg",plot = pixelFigure)


#### Models

# linear Regression
mod1 <- lm( V1 ~ . , data= ziptrain27)
regResults<-summary(mod1)
plot(mod1)
plot(regResults$residuals)
stdResid<-scale(regResults$residuals,center = T,scale = T)
ggplot(data=as.data.frame(stdResid), aes(y=stdResid,x=1:length(stdResid)))+
  geom_point(color="black")+
  labs(x="Order",y="Std Residuals",title = "Residuals")+
  geom_hline(yintercept = 0,linetype="dashed",colour="red")+
  theme(plot.title=element_text(face = "bold"))
ggsave("LRResidual.jpg",plot = last_plot())


pred1.train <- predict.lm(mod1, ziptrain27[,-1])
ggplot(data=as.data.frame(pred1.train), aes(x=pred1.train))+
  geom_density(color="darkblue", fill="lightblue")+
  labs(x="Predicted",y="Density",title = "Predictions")+
  geom_vline(xintercept = 4.5,
             linetype="dashed")
ggsave("LRPredictions.jpg",plot = last_plot())
y1pred.train <- 2 + 5*(pred1.train >= 4.5)
mean(y1pred.train != ziptrain27[,1])


# KNN
kk <- 1
xnew <- ziptrain27[,-1]
ypred2.train <- knn(ziptrain27[,-1], xnew, ziptrain27[,1], k=kk)
mean( ypred2.train != ziptrain27[,1])

trainKnnErrors<-c()
for (i in c(1, 3, 5, 7, 15)){
  xnew <- ziptrain27[,-1]
  ypred2.train <- knn(ziptrain27[,-1], xnew, ziptrain27[,1], k=i)
  trainKnnErrors<-c(trainKnnErrors,mean( ypred2.train != ziptrain27[,1]))
  print(mean( ypred2.train != ziptrain27[,1]))
}
trainKnnErrors
#0.00000000 0.01017442 0.01235465 0.01453488 0.01744186

#### Predictions

## Test Errors
ziptest <- read.table("zip.test.csv",sep = ",")
ziptest27 <- subset(ziptest, ziptest[,1]==2 | ziptest[,1]==7)

# Testing Error of LR
pred1.test <- predict.lm(mod1, ziptest27[,-1])
y1pred.test <- 2 + 5*(pred1.test >= 4.5)
missedLR<-which(y1pred.test != ziptest27[,1])

# Miss-classified plots LR
n=5
plots <- vector("list", n)=
  rowIndex<-missedLR[1:5]
for (i in 1:n){
  xVal = t(matrix(data.matrix(ziptest27[rowIndex[i],-1]),byrow=TRUE,16,16)[16:1,])
  gg <- melt(data.frame(x=1:16,xVal),id="x")
  plots[[i]]<-ggplot(gg) + geom_raster(aes(x=x,y=variable,fill=value))+
    labs(x = paste0("True = ",ziptest27[rowIndex[i],1],"; Pred. = ",y1pred.test[rowIndex[i]]))+
    scale_x_continuous(expand=c(0,0))+
    scale_fill_continuous(type = "gradient")+
    guides(fill=FALSE)+                  
    theme(axis.text = element_blank(),
          axis.title.x=element_text(face = "bold"),
          axis.ticks=element_blank(),
          axis.title=element_blank())
}
pixelFigure=ggarrange(plotlist=plots,ncol = 5,nrow = 1)
pixelFigure
ggsave("LR Miss.jpg",plot = pixelFigure)


# Testing error of KNN
testKnnErrors<-c()
for (i in c(1, 3, 5, 7, 15)){
  xnew2 <- ziptest27[,-1]
  ypred2.test <- knn(ziptrain27[,-1], xnew2, ziptrain27[,1], k=i)
  testKnnErrors<-c(testKnnErrors,mean( ypred2.test != ziptest27[,1]))
  print(mean( ypred2.test != ziptest27[,1]))
}

# Test and Train error of KNN
TTErrors<-data.frame(y=c(trainKnnErrors,testKnnErrors),x=c(1,3,5,7,15),Error=c(rep("Train",5),rep("Test",5)))
ggplot(data =TTErrors )+
  geom_point(aes(x=x,y=y,color=Error))+
  geom_line(aes(x=x,y=y,color=Error))+
  labs(y="Errors",x="K ",title = "KNN Errors")
  
ggsave("Knn erors.jpg",plot = last_plot()) 
  

xnew5 <- ziptest27[,-1]
ypred5.test <- knn(ziptrain27[,-1], xnew5, ziptrain27[,1], k=5)
print(mean( ypred5.test != ziptest27[,1]))
missedKNN5<-which((ypred5.test != ziptest27[,1]))


# Miss-classified plots KNN5
n=5
plots <- vector("list", n)
rowIndex<-missedKNN5[1:5]
for (i in 1:n){
  xVal = t(matrix(data.matrix(ziptest27[rowIndex[i],-1]),byrow=TRUE,16,16)[16:1,])
  gg <- melt(data.frame(x=1:16,xVal),id="x")
  plots[[i]]<-ggplot(gg) + geom_raster(aes(x=x,y=variable,fill=value))+
    labs(x = paste0("True = ",ziptest27[rowIndex[i],1],"; Pred. = ", ypred5.test[rowIndex[i]]))+
    scale_x_continuous(expand=c(0,0))+
    scale_fill_continuous(type = "gradient")+
    guides(fill=FALSE)+                  
    theme(axis.text = element_blank(),
          axis.title.x=element_text(face = "bold"),
          axis.ticks=element_blank(),
          axis.title=element_blank())
}
pixelFigure=ggarrange(plotlist=plots,ncol = 5,nrow = 1)
pixelFigure
ggsave("KNN5 Miss.jpg",plot = pixelFigure)


