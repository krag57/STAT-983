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
summary(Auto)
par(mfrow = c(2,3))
for (i in 2:7) boxplot(Autotrain[,i], xlab=names(Auto)[i])
dev.off()

#correalation
cor1=round(cor(Auto[,2:7]),2)
#write.csv(cor1, file = "corr.csv")

ggplot(data = Auto, aes(x = origin, y = weight)) +
  geom_boxplot() +
  xlab('Region of Origin') +
  ylab('Weight') +
  ggtitle('Weight Comparison by Region of Origin') 

ggplot(data = Auto, aes(x = as.factor(cylinders), y = weight)) +
  geom_boxplot() +
  xlab('Number of Cylinders') +
  ylab('Weight') +
  ggtitle('Weight Comparison by Number of Cylinders') 

ggplot(data = Auto, aes(x = as.factor(mpg01), y = weight)) +
  geom_boxplot() +
  xlab('Number of Cylinders') +
  ylab('Weight') +
  ggtitle('Weight Comparison by Number of Cylinders') 


ggplot(data = Auto, aes(x =  as.factor(cylinders), fill = origin)) +
  geom_bar() +
  xlab('Number of Cylinders') +
  ylab('Count') +
  ggtitle('Cars from Each Region by Number of Cylinders')

ggplot(data = Auto, aes(x =  as.factor(mpg01), fill = origin)) +
  geom_bar() +
  xlab('Number of Cylinders') +
  ylab('Count') +
  ggtitle('Cars from Each Region by Number of Cylinders')

ggplot(data = Auto, aes(x = as.factor(year), fill = as.factor(cylinders))) +
  geom_bar() +
  facet_wrap(~ origin, ncol = 1) +
  xlab('Model Year') +
  ylab('Count') +
  ggtitle('Each Region of Origin\'s Product Mix Over Time')

ggplot(data = Auto, aes(x = as.factor(year), fill = as.factor(mpg01))) +
  geom_bar() +
  facet_wrap(~ origin, ncol = 1) +
  xlab('Model Year') +
  ylab('Count') +
  ggtitle('Each Region of Origin\'s Product Mix Over Time')

ggplot(data = Auto, aes(x = as.factor(cylinders), fill = as.factor(mpg01))) +
  geom_bar() +
  xlab('Number of Cylinders') +
  ylab('Count') +
  ggtitle('Cars from Each Region by Number of Cylinders')
