setwd("~/Documents/Class/STAT-983")

library(ggplot2)
library(gridExtra)
library(ggpubr)
library(reshape2)

ziptrain <- read.table("zip.train.csv", sep = ",")
ziptrain27 <- subset(ziptrain, ziptrain[,1]==2 | ziptrain[,1]==7)

### EDA
dim(ziptrain27)
sum(ziptrain27[,1] == 2)
summary(ziptrain27)
round(cor(ziptrain27),2)
## To see the letter picture of the 5-th row by changing the row observation
## to a matrix 
rowindex = 5
## You can try other "rowindex" values to see other rows
#ziptrain27[rowindex,1]
#Xval = t(matrix(data.matrix(ziptrain27[rowindex,-1]),byrow=TRUE,16,16)[16:1,])
#image(Xval,col="virdis",axes=FALSE) 

n=10
plots <- vector("list", n)
rowIndex<-sample(1:length(ziptrain27),size = n)
for (i in 1:n){
  xVal = t(matrix(data.matrix(ziptrain27[rowIndex[i],-1]),byrow=TRUE,16,16)[16:1,])
  gg <- melt(data.frame(x=1:16,xVal),id="x")
  plots[[i]]<-ggplot(gg) + geom_raster(aes(x=x,y=variable,fill=value))+
    labs(x = paste0("label = ",ziptrain27[rowIndex[i],1]))+
    scale_x_continuous(expand=c(0,0))+
    scale_fill_continuous(type = "gradient")+# get rid of extra space on x-axis
    guides(fill=FALSE)+                  # turn off color legend
    theme(axis.text = element_blank(),
          axis.title.x=element_text(face = "bold"),     # turn off the axis annotations
          axis.ticks=element_blank(),
          axis.title=element_blank())
}
#figure=do.call(ggarrange(), plots)
figure=ggarrange(plotlist=plots,ncol = 5,nrow = 2,)
annotate_figure(figure,top = text_grob("Images training sample",face = "bold",size = 16))




