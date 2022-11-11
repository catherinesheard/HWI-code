#dispersal map
library(maptools);library(PBSmapping);library(epitools)
library(RColorBrewer);library(classInt)
library(rgdal);library(sp); library(spdep)
library(viridis)

#Set workspace and load the stuff we need
setwd("...")

load("wrld.RData")
load("grd360.RData")
load("wrldBehr.RData")
load("grdbehr360.RData")

#Make the shape files place nice
grd360<-readShapePoly("360x114global.shp",IDvar="ID",verbose=T,proj4string=CRS("+proj=longlat +datum=WGS84"))
wrld<-readShapePoly("world_outline.shp",IDvar="ID",verbose=T,proj4string=CRS("+proj=longlat +datum=WGS84"))

setwd("...")

HWIProp <- read.csv("HWI-2018-mapping.csv", header = T)
SpeciesHWI <- data.frame(HWIProp$Quad360ID,HWIProp$AvgOfHWI,HWIProp$StDevOfHWI,HWIProp$CountOfHWI)
names(SpeciesHWI)<- c("QuadID","Avg","StD","Richness")
which(SpeciesHWI$Richness>9)->want
SpeciesHWI[want,,]->SpeciesHWIBetter


ForMapping <- data.frame(SpeciesHWIBetter$QuadID, SpeciesHWIBetter$Avg)
names(ForMapping) <- c("QuadID", "Avg")


# STEP 2: Merge Family Richness with attribute table
grdbehr<-grdbehr360
grdbehr@data<-merge(grdbehr@data,ForMapping,by.x="ID",by.y="QuadID", all.x=T)
summary(grdbehr@data)

# STEP 3: Create a nice color scheme
my.class.fr<-classIntervals(grdbehr@data$Avg,n=30,style="equal" )
my.pal<-brewer.pal(7, "Purples") # choose colors
my.pal<-rev(viridis(30))
my.pal<-rev(magma(30))
my.pal<-rev(brewer.pal(11,"RdBu"))
my.col.fr<-findColours(my.class.fr,my.pal) # ramp colors based on classInts

legendnames<-c("[20.5,22.9)","[22.9,25.2)","[25.2,27.5)","[27.5,29.9)","[29.9,32.2)",
               "[32.2,34.6)","[34.6,36.9)","[36.9,39.3)",
               "[39.3,41.6)","[41.6,43.9)","[43.9,46.3)",
               "[46.3,48.6)","[48.6,51.0)","[51.0,53.3)","[53.3,55.7)")

legendnames<-c("20.5-25.2","25.2-31.1","31.1-36.9",
               "36.9-43.9","43.9-48.6","36.9-55.7")
# STEP 4: Map
#windows(13,7)
setwd("C:/Users/cs16502/Dropbox/Dispersal/2019-07-15/Figures & images/")
#pdf("Fig3-all-avg.pdf",width=20,height=10)
#par(mar=c(1,1,2,1))
plot(grdbehr,col=my.col.fr,border=my.col.fr)
plot(wrldbehr,add=T)
title(main = "Average HWI",cex.main=5)
legend(-17000000,1000000,#legend=names(attr(my.col.fr,"table")),
       legend=legendnames, 
       #fill=attr(my.col.fr,"palette"),
       #fill=c(my.pal[1],my.pal[7],my.pal[13],my.pal[19],my.pal[25],my.pal[30]),
       fill=c(my.pal[1],my.pal[3],my.pal[5],my.pal[7],my.pal[9],my.pal[11]),
       #cex=3.5,bty="n") #suppress legend box
       cex=3.5)
text(-15000000,5000000,"(a)",cex=5)
#dev.off()



