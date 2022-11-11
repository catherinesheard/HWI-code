setwd("...")

library(metaviz)
library(ggplot2)
library(grid)
library(RColorBrewer)

zz<-c(-0.034,
      0.015,
      0.026,
      0.019,
      0.117,
      0.022,
      -0.019,
      -0.115,
      0.132,
      0.059
)
ll<-c(-0.068,
      0.005,
      0.006,
      0.001,
      0.095,
      0.009,
      -0.029,
      -0.142,
      0.107,
      0.028
)
uu<-c(-0.002,
      0.027,
      0.043,
      0.039,
      0.138,
      0.037,
      -0.007,
      -0.088,
      0.161,
      0.088
)

tt<-as.data.frame(cbind(
  c("Body Mass", 
    "Island", "Latitude", "Temperature", "Temp. Var.", 
    "Precipitation", "Precip. Var.", 
    "Territoriality",
    "Migration","Open Habitat"),
  zz,
  ll,
  uu))
tt$uu<-as.numeric(as.vector(tt$uu))
tt$ll<-as.numeric(as.vector(tt$ll))
tt$zz<-as.numeric(as.vector(tt$zz))

tt$se<-(tt$uu-tt$zz)/1.96


tt<-tt[c(3,2,4,5,6,7,1,10,9,8),] #reorder

tt2<-data.frame(V1=c("","",""),zz=rep(10,3),ll=rep(9.9,3),uu=rep(10.1,3),se=rep(.1/1.96,3)) #make blank space a really stupid way

comb<-rbind(tt,tt2)

tt<-comb[c(11,1,2,12,3,4,5,6,13,7,8,9,10),]

tehblue<-my.pal<-rev(brewer.pal(11,"RdBu"))[3]

pdf("Fig4.pdf",width=8,height=5)
g<-viz_forest(x = tt[, c("zz", "se")], study_labels = tt[, c("V1")],type="study_only",
              xlab = "z-score", variant = "rain",col="Blues", method = "DL",
              annotate_CI = FALSE,
              text_size=5,
              #y_breaks=NULL,
              x_breaks=round(seq(-0.15,0.2,by=0.05),2),
              x_limit=c(-0.16,0.2)) + 
  annotate("text", label = "BIOGEOGRAPHY", x = -0.23, y = 13, size=5,color = tehblue,fontface="bold")+ #to line up exactly: x = -0.35
  annotate("text", label = "CLIMATE", x = -0.205, y = 10, size=5,color = tehblue,fontface="bold")+ #to line up exactly: x = -0.31
  annotate("text", label = "ECOLOGY", x = -0.21, y = 5, size=5,color = tehblue,fontface="bold")+ #to line up exactly: x = -0.32
  annotate("text", label = "-", x = -0.16, y = 1, size=5,color = "black")+ 
  annotate("text", label = "-", x = -0.16, y = 2, size=5,color = "black")+ 
  annotate("text", label = "-", x = -0.16, y = 3, size=5,color = "black")+ 
  annotate("text", label = "-", x = -0.16, y = 4, size=5,color = "black")+ 
  annotate("text", label = "-", x = -0.16, y = 6, size=5,color = "black")+ 
  annotate("text", label = "-", x = -0.16, y = 7, size=5,color = "black")+ 
  annotate("text", label = "-", x = -0.16, y = 8, size=5,color = "black")+ 
  annotate("text", label = "-", x = -0.16, y = 9, size=5,color = "black")+ 
  annotate("text", label = "-", x = -0.16, y = 11, size=5,color = "black")+ 
  annotate("text", label = "-", x = -0.16, y = 12, size=5,color = "black")+ 
  theme(plot.margin = margin(2, 2, 2, 2, "cm"),axis.ticks=element_blank())



g = ggplotGrob(g) #this bit is needed to make words outside of the plot
g$layout$clip[g$layout$name=="panel"] <- "off"
grid.draw(g)

dev.off()
#annotate_CI will put the values in text to the right