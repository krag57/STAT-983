setwd("~/Documents/Class/STAT-983")
ziptrain <- read.table("zip.train.csv", sep = ",")
ziptrain27 <- subset(ziptrain, ziptrain[,1]==2 | ziptrain[,1]==7)

### EDA


