library(MCMCglmm)

setwd("...")

x<- read.csv("...") #read in data, see https://github.com/catherinesheard/Global-HWI


if(sum(is.na(x$Hand.Wing.Index..Claramunt.2011.))>0){x<-x[-which(is.na(x$Hand.Wing.Index..Claramunt.2011.)),]}
if(sum(is.na(x$log10SpecGenFinalMass))>0){x<-x[-which(is.na(x$log10SpecGenFinalMass)),]}
if(sum(is.na(x$Tree_name))>0){x<-x[-which(is.na(x$Tree_name)),]}
if(sum(is.na(x$AvgIsland))>0){x<-x[-which(is.na(x$AvgIsland)),]}
if(sum(is.na(x$Stability.2016))>0){x<-x[-which(is.na(x$Stability.2016)),]}
if(sum(is.na(x$Territory.2016))>0){x<-x[-which(is.na(x$Territory.2016)),]}
if(sum(is.na(x$Migration.status))>0){x<-x[-which(is.na(x$Migration.status)),]}
if(sum(is.na(x$Diet))>0){x<-x[-which(is.na(x$Diet)),]}   
if(sum(is.na(x$Latitude_mean))>0){x<-x[-which(is.na(x$Latitude_mean)),]}   
if(sum(is.na(x$meanAnnualTemp))>0){x<-x[-which(is.na(x$meanAnnualTemp)),]}
if(sum(is.na(x$tempRange))>0){x<-x[-which(is.na(x$tempRange)),]}
if(sum(is.na(x$AnnualPrecip))>0){x<-x[-which(is.na(x$AnnualPrecip)),]}   
if(sum(is.na(x$precipRange))>0){x<-x[-which(is.na(x$precipRange)),]}  
if(sum(is.na(x$Habitat))>0){x<-x[-which(is.na(x$Habitat)),]} 
if(sum(x$Order=="Apterygiformes")>0){x<-x[-which(x$Order=="Apterygiformes"),]} 

trees<-read.tree("...") #sample of 100 trees from birdtree.org
tree<-trees[[2]] #arbitrary tree for data cleaning


#match data and trees
datanames<-as.vector(x$Tree_name)
datanames<-gsub(" ","_",datanames)
treenames<-tree$tip.label

badnames<-rep(0,length(treenames))
for(i in 1:length(treenames)){
  if(sum(treenames[i]==datanames)==0){badnames[i]=1}
}
badnames<-treenames[which(badnames==1)]

trees<-lapply(trees,drop.tip,tip=badnames)


arcsin <- function(p) { log((p+0.001)/(1-p+0.001)) }

#scale continuous variables

x$zHWI<-scale(log(x$Hand.Wing.Index..Claramunt.2011.+1))
x$zMass<-scale(x$log10SpecGenFinalMass)
x$zRange<-scale(log(x$Quad360Count))
x$zIsl<-scale(arcsin(x$AvgIsland))
x$zLat<-scale(abs(x$Latitude_mean))
x$zTemp<-scale(x$meanAnnualTemp)
x$zTR<-scale(x$tempRange)
x$zPrecip<-scale(x$AnnualPrecip)
x$zPR<-scale(x$precipRange)

x$Territory.2016[x$Territory.2016=="weak"]<-"none"

x$mig<-rep("no",dim(x)[1])
x$mig[x$Migration.status=="Full Migrant"]<-"yes"
x$mig<-as.factor(x$mig)

x$Habitat[x$Habitat==1]<-"Closed"
x$Habitat[x$Habitat==2]<-"Closed"
x$Habitat[x$Habitat==3]<-"Open"

x$animal<-datanames


#dummy run to determine start point

i=1

tree<-trees[[i]]  

animalA<-inverseA(tree)$Ainv 

prior.PN<-list(G=list(G1=list(V=1,nu=0.002)),R=list(V=1,nu=0.002))



mod<-MCMCglmm(zHWI~zMass+zIsl+zLat+zTemp+zTR+zPrecip+zPR+Territory.2016+Diet+mig+Habitat, 
              random=~animal, 
              ginverse=list(animal=animalA), 
              prior = prior.PN, 
              verbose=FALSE, 
              family="gaussian",
              data = x,
              nitt=5500*2,
              thin=10,
              burnin=500*2,
              pl=TRUE,
              pr=TRUE,
              slice=TRUE)

Final.disp<-mod
Final.disp$VCV[((i-1)*10+1):(i*10), ]<-mod$VCV[1:10,] 
Final.disp$Sol[((i-1)*10+1):(i*10), ]<-mod$Sol[1:10,] 
Final.disp$Liab[((i-1)*10+1):(i*10), ]<-mod$Liab[1:10,] 

nsamp.l<-nrow(mod$VCV)
start1.l=list(R=mod$VCV[nsamp.l,"units"], G=list(G1=mod$VCV[nsamp.l,"animal"]))

save(Final.disp,file="dispersal-full-nokiwi.Rdata")


#full loop over 100 trees


for(i in 1:100){
  tree<-trees[[i]]  
  
  animalA<-inverseA(tree)$Ainv 
  
  mod<-MCMCglmm(zHWI~zMass+zIsl+zLat+zTemp+zTR+zPrecip+zPR+Territory.2016+Diet+mig+Habitat, 
                random=~animal, 
                ginverse=list(animal=animalA), 
                prior = prior.PN,  
                start= start1.l,
                verbose=FALSE, 
                family="gaussian",
                data = x,
                nitt=15000,
                thin=1000,
                burnin=5000,
                pl=TRUE,
                pr=TRUE,
                slice=TRUE)
  
  
  print(i)
  
  Final.disp$VCV[((i-1)*10+1):(i*10), ]<-mod$VCV[1:10,] 
  Final.disp$Sol[((i-1)*10+1):(i*10), ]<-mod$Sol[1:10,] 
  Final.disp$Liab[((i-1)*10+1):(i*10), ]<-mod$Liab[1:10,] 
  
  nsamp.l<-nrow(mod$VCV)
  start1.l=list(R=mod$VCV[nsamp.l,"units"], G=list(G1=mod$VCV[nsamp.l,"animal"]))
  
  
  
  save(Final.disp,file="dispersal-full-nokiwi.Rdata")
  
  
}

save(Final.disp,file="dispersal-full-nokiwi.Rdata")