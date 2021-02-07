setwd("~/Documents/Class/STAT-983")

library(ggplot2)
library(gridExtra)
library(ggpubr)
library(reshape2)
library(ggcorrplot)
library(class)

ziptrain <- read.table("zip.train.csv", sep = ",")
ziptrain27 <- subset(ziptrain, ziptrain[,1]==2 | ziptrain[,1]==7)

### EDA
dim(ziptrain27)
sum(ziptrain27[,1] == 2)
summary(ziptrain27)
corr<-cor(ziptrain27)
round(cor(ziptrain27),2)

ggcorrplot(corr[1:10,1:10],
           ggtheme = ggplot2::theme_gray)


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


### Models

# linear Regression
mod1 <- lm( V1 ~ . , data= ziptrain27)
regResults<-summary(mod1)
plot(mod1)
plot(regResults$residuals)
pred1.train <- predict.lm(mod1, ziptrain27[,-1])
plot(density(pred1.train))
ggplot(data=as.data.frame(pred1.train), aes(x=pred1.train))+
  geom_density(color="darkblue", fill="lightblue")+
  labs(x="Predicted",y="Density",title = "Predictions")+
  geom_vline(xintercept = 4.5,
             linetype="dashed")
y1pred.train <- 2 + 5*(pred1.train >= 4.5)
mean( y1pred.train != ziptrain27[,1])


# KNN
kk <- 1
xnew <- ziptrain27[,-1]
ypred2.train <- knn(ziptrain27[,-1], xnew, ziptrain27[,1], k=kk)
mean( ypred2.train != ziptrain27[,1])

testKnnErrors<-c()
for (i in c(1, 3, 5, 7, 15)){
  xnew <- ziptrain27[,-1]
  ypred2.train <- knn(ziptrain27[,-1], xnew, ziptrain27[,1], k=i)
  testKnnErrors<-c(testKnnErrors,mean( ypred2.train != ziptrain27[,1]))
  print(mean( ypred2.train != ziptrain27[,1]))
}


### Test Errors
ziptest <- read.table("zip.test.csv",sep = ",")
ziptest27 <- subset(ziptest, ziptest[,1]==2 | ziptest[,1]==7)
# Testing error of KNN
kk <- 1
xnew2 <- ziptest27[,-1]
ypred2.test <- knn(ziptrain27[,-1], xnew2, ziptrain27[,1], k=kk)
mean( ypred2.test != ziptest27[,1])


