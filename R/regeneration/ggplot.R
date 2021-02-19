setwd("S:/Users/Dain/연구/돌발홍수/data/regeneration")
ggdata<-read.table("ggmap.csv",sep=",",head=T)

#######################################################################################
if (!require("ggmap")) install.packages("ggmap") 


library(maps) 
library(ggmap) 
p<-qmap(location="seoul", zoom = 9, maptype = "toner-lite",source = "stamen")
p <- p + geom_point(data=data.frame(ggdata), aes(lon, lat,cex=0.5,col="red"))
p






library(maps) 
map('world','South Korea') 

points(ggdata$lon, ggdata$lat, pch=16, col=ggdata$col) 
points(ggdata$lon, ggdata$lat,  col=1) 
library(ggmap) 
p <- get_map('Seoul', zoom=9, maptype='roadmap') 
p <- get_map('Seoul', zoom=9, maptype='toner-lite') 
a<-ggmap(p) + geom_point(data=data.frame(ggdata),aes(lon, lat,  colour=-count), alpha=0.9)
plot(a) 




df=data.frame(x=rnorm(20),y=runif(20),z=(-13:6))
ggplot(df,aes(x=x,y=y))+geom_point(aes(size=-z))+
  scale_size("New legend",breaks=c(-10,-5,0,5,10),labels=c(10,5,0,-5,-10))
