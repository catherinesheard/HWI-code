library(RColorBrewer)
library(ggplot2)
library(readr)
library(tidyr)
library(Hmisc)
library(plyr)
library(reshape2)
library(gridExtra)
library(gtable)

raincloud_theme = theme(
  text = element_text(size = 10),
  axis.title.x = element_text(size = 16),
  axis.title.y = element_text(size = 16),
  axis.text = element_text(size = 14),
  axis.text.x = element_text(angle = 45, vjust = 0.5),
  legend.title=element_text(size=16),
  legend.text=element_text(size=16),
  legend.position = "right",
  plot.title = element_text(lineheight=.8, face="bold", size = 16),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))

setwd("...")


x<- read.csv("dispersaldata.csv")


if(sum(is.na(x$Hand.Wing.Index..Claramunt.2011.))>0){x<-x[-which(is.na(x$Hand.Wing.Index..Claramunt.2011.)),]}
if(sum(is.na(x$tempRange))>0){x<-x[-which(is.na(x$tempRange)),]}
if(sum(is.na(x$Quad360Count))>0){x<-x[-which(is.na(x$Quad360Count)),]}
if(sum(is.na(x$Latitude_mean))>0){x<-x[-which(is.na(x$Latitude_mean)),]}
if(sum(x$Order=="Apterygiformes")>0){x<-x[-which(x$Order=="Apterygiformes"),]} 

cols<-brewer.pal(9, "RdBu")
cols.fill<-"red"
glat<-ggplot(x, aes(x=abs(Latitude_mean), y=Hand.Wing.Index..Claramunt.2011.)) + 
  geom_point(alpha=1,col="grey",size=0.5)+
  geom_smooth(color="darkred",fill="darksalmon")+
  xlab("Mid-point latitude")+
  ylab("HWI") +
  theme_bw() +
  raincloud_theme +
  annotate("text", label = "(a)", x = 75, y = 10, size=6,color = "black")+
  theme(axis.text.x = element_text(angle = 0),axis.line.x = element_line(color="black"),axis.line.y = element_line(color="black"))


cols<-brewer.pal(9, "PRGn")
gtempvar<-ggplot(x, aes(x=tempRange, y=Hand.Wing.Index..Claramunt.2011.)) + 
  geom_point(alpha=1,col="grey",size=0.5)+
  geom_smooth(color="darkred",fill="darksalmon")+
  xlab("Temperature variability")+
  ylab("HWI") +
  theme_bw() +
  raincloud_theme +
  annotate("text", label = "(b)", x = 60, y = 10, size=6,color = "black")+
  theme(axis.text.x = element_text(angle = 0),axis.line.x = element_line(color="black"),axis.line.y = element_line(color="black"))


cols<-brewer.pal(9, "PRGn")
grs<-ggplot(x, aes(x=log(Quad360Count), y=Hand.Wing.Index..Claramunt.2011.)) + 
  geom_point(alpha=1,col="grey",size=0.5)+
  geom_smooth(color="darkred",fill="darksalmon")+
  xlab("Geographic range")+
  ylab("HWI") +
  theme_bw() +
  raincloud_theme +
  annotate("text", label = "(c)", x = 8.4, y = 10, size=6,color = "black")+
  theme(axis.text.x = element_text(angle = 0),axis.line.x = element_line(color="black"),axis.line.y = element_line(color="black"))

grid.arrange(glat, gtempvar, grs, nrow = 3) 

pdf("Fig5-colour-edited-pink-nk.pdf",width = 5, height = 10) 
grid.arrange(glat, gtempvar, grs, nrow = 3)      
dev.off() 
