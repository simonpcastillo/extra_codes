inputfolder <- "E:/data-raw/batch*/batch*/box*/cellPos_csv"
files <- Sys.glob(paste0(inputfolder, "/*_cellPos.csv"))
files[1]
#load the libraries
library(spatstat)
library(tidyverse)
dat= read.csv("https://raw.githubusercontent.com/lionel68/Blog/master/PointPatternAnalysis/Dataset_mieren_af.csv", sep=";")
dat2 = read.csv(files[10])
  
  
dat$x <- dat$X/100
dat$y <- dat$Y/100

#creating the point pattern
all_pp <- ppp(dat[,"x"],dat[,"y"],owin(range(dat$x),range(dat$x)))
all_pp2 <- ppp(dat2[,"x"],dat2[,"y"],owin(range(dat2$x),range(dat2$y)))

class(all_pp)
plot(all_pp)


#a first manipulation would be to add information to each points
#the so-called marks, in this example we could add 
#the species names
marks(all_pp) <- dat$soort
marks(all_pp2) <- dat2$class


#a second manipulation could be to remove any duplicated points
all_pp <- unique(all_pp)
all_pp2 <- unique(all_pp2)


#then add the coordinate unit
unitname(all_pp) <- c("meter","meter")
unitname(all_pp2) <- c("meter","meter")

summary(all_pp);summary(all_pp2)


#we could subset the point pattern using the marks
ant_pp <- subset(all_pp,marks=="Tetramorium_caespitum")
ant_pp <- subset(all_pp,marks=="Senecio_jacobea")

ant_pp2 <- subset(all_pp2,marks=="p")

#in that case we do not need the marks any more
ant_pp <- unmark(ant_pp)
ant_pp2 <- unmark(ant_pp2)


#split based on the species names
split_pp <- split(all_pp)
split_pp2 <- split(all_pp2)

class(split_pp)
as.matrix(lapply(split_pp,npoints),ncol=1)
as.matrix(lapply(split_pp2,npoints),ncol=1)



dens_all <- density(split_pp)
dens_all2 <- density(split_pp2)

plot(dens_all)
plot(dens_all2)

m3 <- kppm(ant_pp ~ Senecio_jacobea,data=dens_all)
m3b <- kppm(ant_pp2 ~ p,data=dens_all2)

#let's look at the expected K values
plot(m3b,what="statistic",pause=FALSE)

#draw a cool perspective map
pp2 <- predict(m3b)
pp <- predict(m3)

M<-persp(dens_all$Senecio_jacobea,colin=pp*10,box=FALSE,
         visible=TRUE,apron = TRUE,theta=55,phi=25,
         expand=6,main="Density")


P<- persp(dens_all2$p,colin=pp2,box=FALSE,
      apron = T,theta=230,phi=45,
      expand=.5,main="Density Ki67+", scale=T)
perspPoints(ant_pp2,Z=dens_all2$p,M=P,pch=16, alpha=0.5, col=rgb(0,0,0,0.2))

N<- persp(dens_all2$n,colin=pp2,box=FALSE,
          apron = T,theta=230,phi=45,
          expand=.5,main="Density Ki67-", scale=T)
perspPoints(ant_pp2,Z=dens_all2$n,M=N,pch=16, alpha=0.5, col=rgb(0,0,0,0.2))


L<- persp(dens_all2$l,colin=pp2,box=FALSE,
          apron = T,theta=230,phi=45,
          expand=.5,main="Density Lymphocytes", scale=T)
perspPoints(ant_pp2,Z=dens_all2$l,M=L,pch=16, alpha=0.5, col=rgb(0,0,0,0.2))
