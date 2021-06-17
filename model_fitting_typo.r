setwd("/home/bella/Dropbox/Master/2nd Semester/Typo")

data= read.csv("final_data1.csv")
head(data)
cor.test(data$langstat,data$col,method="sp")

plot(data$langstat,data$col,xlab="Status of Language",ylab="Number of Colour Terms",main = "Celtic Language Family" ,pch=18, col="blue")
text(data$col,data$langstat,row.names(data$iso),cex=0.6, pos=4, col="red")



typo.data = read.csv("final_data.csv",header=TRUE)
col.lm=lm(langstat~col,data=typo.data)
summary(col.lm)
col.lm=lm(col~langstat,data=typo.data)
summary(col.lm)
plot(col~langstat,data=typo.data)
abline(col.lm)
library(ggplot2)
ggplot(data,aes(x=col,y=langstat))+
geom_point()+
stat_smooth(method="lm",col="red")
ggplot(data,aes(x=langstat,y=col))+
geom_point()+
stat_smooth(method="lm",col="red")
library(vioplot)
vioplot(data$col, data$langstat,data = data)
library(ggplot2)
ggplot2(data, xName='dose',yName='len',
                   addMean=TRUE, meanPointShape=23, meanPointSize=3,
                   meanPointColor="black", meanPointFill="blue")
            