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


Auto1 <- read.table(file = "Auto.csv", sep = ",", header=T)
mpg01 = I(Auto1$mpg >= median(Auto1$mpg))*1
Auto  = data.frame(mpg01, Auto1[,-1]) ## replace column "mpg" by "mpg01"
origins <- c('USA', 'Europe', 'Japan')
Auto$origin <- factor(Auto$origin, labels = origins)


set.seed(33)
tstIndex=sample(1:dim(Auto)[1],round(0.2*dim(Auto)[1]),replace = F)
Autotrain=Auto[-tstIndex,]
Autotest=Auto[tstIndex,]


#Multi-collinearity
plot.new()
splom(Auto, pscales = 0,groups = Auto$mpg01)
Auto$mpg01<-as.factor(Auto$mpg01)
Auto$cylinders<-as.factor(Auto$cylinders)
Auto$year<-as.factor(Auto$year)
summary(Auto)
par(mfrow = c(2,3))
for (i in 2:7) boxplot(Autotrain[,i], xlab=names(Auto)[i])
dev.off()

#correalation
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
