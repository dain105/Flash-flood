pcp.mtx<-as.matrix(read.table("E:/flash/data/1h_pcp_mtx.csv",sep=","),ncol=92,byrow=T)
run.mtx<-as.matrix(read.table("E:/flash/data/1h_run_mtx.csv",sep=","),ncol=92,byrow=T)
sws.mtx<-as.matrix(read.table("E:/flash/data/1h_sws_mtx.csv",sep=","),ncol=92,byrow=T)



pcp.mtx<-rbind(pcp.mtx,rep(0,38))
run.mtx<-rbind(run.mtx,rep(0,38))
sws.mtx<-rbind(sws.mtx,rep(0,38))

pcp.mtx3<-pcp.mtx2<-pcp.mtx1<-pcp.mtx
run.mtx3<-run.mtx2<-run.mtx1<-run.mtx
sws.mtx2<-sws.mtx

pcp.mtx4<-matrix(rep(0,13248*38),ncol=38)

for (i in 2:dim(pcp.mtx)[1])
{
  
 if (i>2)
 {
   run.mtx1[i,]=(0.86*run.mtx[i,]+0.94*run.mtx[i-1,])/(0.86+0.94)
}
 
 if (i>3)
 {
   run.mtx2[i,]=run.mtx[i-2,]
   pcp.mtx1[i,]=(0.90*pcp.mtx[i,]+0.93*pcp.mtx[i-1,]+0.63*pcp.mtx[i-2,])/(0.90+0.93+0.63)
 }
 
 if (i>4)
 {pcp.mtx2[i,]=pcp.mtx[i-3,]}
 
if (i>6)
{
  run.mtx3[i,]=(0.90*run.mtx[i-3,]+0.93*run.mtx[i-4,]+0.63*run.mtx[i-5,])/(0.90+0.93+0.64)
  pcp.mtx3[i,]=(0.71*pcp.mtx[i-4,]+0.93*pcp.mtx[i-5,])/(0.71+0.93)
}


 if (i>7)
 {
   for (j in 1:38)
   if (pcp.mtx[i,j]+pcp.mtx[i-1,j]+pcp.mtx[i-2,j]+pcp.mtx[i-3,j]+pcp.mtx[i-4,j]+pcp.mtx[i-5,j] >= 70) pcp.mtx4[i,j]<-1
   if (pcp.mtx[i,j]+pcp.mtx[i-1,j]+pcp.mtx[i-2,j]+pcp.mtx[i-3,j]+pcp.mtx[i-4,j]+pcp.mtx[i-5,j] >= 110) pcp.mtx4[i,j]<-2
 }
  
# for (j in 1:38)
# sws.mtx2[i,j]<-(sws.mtx[i,j]-min(sws.mtx[,j],na.rm=T))/(max(sws.mtx[,j],na.rm=T)-min(sws.mtx[,j],na.rm=T))
 
 print(i/dim(pcp.mtx)[1])
}

for (j in 1:38)
  sws.mtx2[,j]<-sws.mtx[,j]/(max(sws.mtx[,j],na.rm=T))
# sws.mtx2[,j]<-(sws.mtx[,j]-min(sws.mtx[,j],na.rm=T))/(max(sws.mtx[,j],na.rm=T)-min(sws.mtx[,j],na.rm=T))

par(mfrow=c(4,1))
fun.index<-function(i,mtx1=pcp.mtx,mtx2=run.mtx,mtx3=sws.mtx,mtx4=index.mtx,mtx5=pcp.mtx4){
  rain.cau<-which(mtx5[,i]==1)
  rain.warn<-which(mtx5[,i]==2)
    
  gap=as.Date(dataset$date[i], origin = "1970-01-01")-as.Date(paste(format(as.Date(dataset$date[i], origin = "1970-01-01"),"%Y"),"-07-01",sep=""))
  eline=as.numeric(gap)*24+dataset$e.time[i]
  
  plot(mtx1[1:(92*24),i],type="l",ylim=c(0,max(mtx1[,i],na.rm=T)), main=paste(dataset$code[i],dataset$type[i],"pcp","(2008)"), ylab="intensity", col=1,
       xaxt="n")
  axis(1, at=seq(1,2161,120),seq(as.Date("2008-06-30")+1,as.Date("2008-06-30")+91,5))
    if (format(as.Date(dataset$date[i], origin = "1970-01-01"),"%Y")=="2008")
    abline(v=eline,col=3)
    abline(v=rain.cau,col="green3",lty=3)
    abline(v=rain.warn,col="red3",lty=3)
  
  plot(mtx2[1:(92*24),i],type="l",ylim=c(0,max(mtx2[,i],na.rm=T)), main=paste(dataset$code[i],dataset$type[i],"run","(2008)"), ylab="intensity", col=1,
       xaxt="n")
  axis(1, at=seq(1,2161,120),seq(as.Date("2008-06-30")+1,as.Date("2008-06-30")+91,5))
  if (format(as.Date(dataset$date[i], origin = "1970-01-01"),"%Y")=="2008")
    abline(v=eline,col=3)
  
  plot(mtx3[1:(92*24),i],type="l",ylim=c(0,max(mtx3[,i],na.rm=T)), main=paste(dataset$code[i],dataset$type[i],"sws","(2008)"), ylab="level", col=1,
       xaxt="n")
  axis(1, at=seq(1,2161,120),seq(as.Date("2008-06-30")+1,as.Date("2008-06-30")+91,5))
  if (format(as.Date(dataset$date[i], origin = "1970-01-01"),"%Y")=="2008")
    abline(v=eline,col=3)
  
  plot(mtx4[1:(92*24),i],type="l",ylim=c(0,max(mtx4[,i],na.rm=T)), main=paste(dataset$code[i],dataset$type[i],"The flash flood index","(2008)"), ylab="index", col=1,
       xaxt="n")
    axis(1, at=seq(1,2161,120),seq(as.Date("2008-06-30")+1,as.Date("2008-06-30")+91,5))
  abline(h=40,col=4,lty=2)
  abline(h=60,col=2,lty=2)
  if (format(as.Date(dataset$date[i], origin = "1970-01-01"),"%Y")=="2008")
    abline(v=eline,col=3)
    abline(v=rain.cau,col="green3",lty=3)
    abline(v=rain.warn,col="red3",lty=3)
  #################################################################################################
  plot(mtx1[(92*24+1):(92*24*2),i],type="l",col=1,ylim=c(0,max(mtx1[,i],na.rm=T)), main=paste(dataset$code[i],dataset$type[i],"pcp","(2009)"), ylab="intensity",
       xaxt="n")
  axis(1, at=seq(1,2161,120),seq(as.Date("2009-06-30")+1,as.Date("2009-06-30")+91,5))
  if (format(as.Date(dataset$date[i], origin = "1970-01-01"),"%Y")=="2009")
    abline(v=eline,col=3)
  abline(v=rain.cau-92*24,col="green3",lty=3)
  abline(v=rain.warn-92*24,col="red3",lty=3)
  
  plot(mtx2[(92*24+1):(92*24*2),i],type="l",col=1,ylim=c(0,max(mtx2[,i],na.rm=T)), main=paste(dataset$code[i],dataset$type[i],"run","(2009)"), ylab="intensity",
       xaxt="n")
  axis(1, at=seq(1,2161,120),seq(as.Date("2009-06-30")+1,as.Date("2009-06-30")+91,5))
  if (format(as.Date(dataset$date[i], origin = "1970-01-01"),"%Y")=="2009")
    abline(v=eline,col=3)
  
  plot(mtx3[(92*24+1):(92*24*2),i],type="l",col=1,ylim=c(0,max(mtx3[,i],na.rm=T)), main=paste(dataset$code[i],dataset$type[i],"sws","(2009)"), ylab="level",
       xaxt="n")
  axis(1, at=seq(1,2161,120),seq(as.Date("2009-06-30")+1,as.Date("2009-06-30")+91,5))
  if (format(as.Date(dataset$date[i], origin = "1970-01-01"),"%Y")=="2009")
    abline(v=eline,col=3)
  
  plot(mtx4[(92*24+1):(92*24*2),i],type="l",col=1,ylim=c(0,max(mtx4[,i],na.rm=T)), 
         main=paste(dataset$code[i],dataset$type[i],"The flash flood index","(2009)"), ylab="index",xaxt="n")
  axis(1, at=seq(1,2161,120),seq(as.Date("2009-06-30")+1,as.Date("2009-06-30")+91,5))
  abline(h=40,col=4,lty=2)
  abline(h=60,col=2,lty=2)
  if (format(as.Date(dataset$date[i], origin = "1970-01-01"),"%Y")=="2009")
    abline(v=eline,col=3)
  abline(v=rain.cau-92*24,col="green3",lty=3)
  abline(v=rain.warn-92*24,col="red3",lty=3)
  
  #################################################################################################
  plot(mtx1[(92*24*2+1):(92*24*3),i],type="l",col=1,ylim=c(0,max(mtx1[,i],na.rm=T)), main=paste(dataset$code[i],dataset$type[i],"pcp","(2010)"), ylab="intensity",
       xaxt="n")
  axis(1, at=seq(1,2161,120),seq(as.Date("2010-06-30")+1,as.Date("2010-06-30")+91,5))
  if (format(as.Date(dataset$date[i], origin = "1970-01-01"),"%Y")=="2010")
    abline(v=eline,col=3)
  abline(v=rain.cau-92*24*2,col="green3",lty=3)
  abline(v=rain.warn-92*24*2,col="red3",lty=3)
  
  plot(mtx2[(92*24*2+1):(92*24*3),i],type="l",col=1,ylim=c(0,max(mtx2[,i],na.rm=T)), main=paste(dataset$code[i],dataset$type[i],"run","(2010)"), ylab="intensity",
       xaxt="n")
  axis(1, at=seq(1,2161,120),seq(as.Date("2010-06-30")+1,as.Date("2010-06-30")+91,5))
  if (format(as.Date(dataset$date[i], origin = "1970-01-01"),"%Y")=="2010")
    abline(v=eline,col=3)
  
  plot(mtx3[(92*24*2+1):(92*24*3),i],type="l",col=1,ylim=c(0,max(mtx3[,i],na.rm=T)), main=paste(dataset$code[i],dataset$type[i],"sws","(2010)"), ylab="level",
       xaxt="n")
  axis(1, at=seq(1,2161,120),seq(as.Date("2010-06-30")+1,as.Date("2010-06-30")+91,5))
  if (format(as.Date(dataset$date[i], origin = "1970-01-01"),"%Y")=="2010")
    abline(v=eline,col=3)
  
  plot(mtx4[(92*24*2+1):(92*24*3),i],type="l",col=1,ylim=c(0,max(mtx4[,i],na.rm=T)), 
       main=paste(dataset$code[i],dataset$type[i],"The flash flood index","(2010)"), ylab="index",xaxt="n")
  axis(1, at=seq(1,2161,120),seq(as.Date("2010-06-30")+1,as.Date("2010-06-30")+91,5))
  abline(h=40,col=4,lty=2)
  abline(h=60,col=2,lty=2)
  if (format(as.Date(dataset$date[i], origin = "1970-01-01"),"%Y")=="2010")
    abline(v=eline,col=3)
  abline(v=rain.cau-92*24*2,col="green3",lty=3)
  abline(v=rain.warn-92*24*2,col="red3",lty=3)
  #################################################################################################
  plot(mtx1[(92*24*3+1):(92*24*4),i],type="l",col=1,ylim=c(0,max(mtx1[,i],na.rm=T)), main=paste(dataset$code[i],dataset$type[i],"pcp","(2011)"), ylab="intensity",
       xaxt="n")
  axis(1, at=seq(1,2161,120),seq(as.Date("2011-06-30")+1,as.Date("2011-06-30")+91,5))
  if (format(as.Date(dataset$date[i], origin = "1970-01-01"),"%Y")=="2011")
    abline(v=eline,col=3)
  abline(v=rain.cau-92*24*3,col="green3",lty=3)
  abline(v=rain.warn-92*24*3,col="red3",lty=3)
  
  plot(mtx2[(92*24*3+1):(92*24*4),i],type="l",col=1,ylim=c(0,max(mtx2[,i],na.rm=T)), main=paste(dataset$code[i],dataset$type[i],"run","(2011)"), ylab="intensity",
       xaxt="n")
  axis(1, at=seq(1,2161,120),seq(as.Date("2011-06-30")+1,as.Date("2011-06-30")+91,5))
  if (format(as.Date(dataset$date[i], origin = "1970-01-01"),"%Y")=="2011")
    abline(v=eline,col=3)
  
  plot(mtx3[(92*24*3+1):(92*24*4),i],type="l",col=1,ylim=c(0,max(mtx3[,i],na.rm=T)), main=paste(dataset$code[i],dataset$type[i],"sws","(2011)"), ylab="level",
       xaxt="n")
  axis(1, at=seq(1,2161,120),seq(as.Date("2011-06-30")+1,as.Date("2011-06-30")+91,5))
  if (format(as.Date(dataset$date[i], origin = "1970-01-01"),"%Y")=="2011")
    abline(v=eline,col=3)
  
  plot(mtx4[(92*24*3+1):(92*24*4),i],type="l",col=1,ylim=c(0,max(mtx4[,i],na.rm=T)), 
       main=paste(dataset$code[i],dataset$type[i],"The flash flood index","(2011)"), ylab="index",xaxt="n")
  axis(1, at=seq(1,2161,120),seq(as.Date("2011-06-30")+1,as.Date("2011-06-30")+91,5))
  abline(h=40,col=4,lty=2)
  abline(h=60,col=2,lty=2)
  if (format(as.Date(dataset$date[i], origin = "1970-01-01"),"%Y")=="2011")
    abline(v=eline,col=3)
  abline(v=rain.cau-92*24*3,col="green3",lty=3)
  abline(v=rain.warn-92*24*3,col="red3",lty=3)
  #################################################################################################
  plot(mtx1[(92*24*4+1):(92*24*5),i],type="l",col=1,ylim=c(0,max(mtx1[,i],na.rm=T)), main=paste(dataset$code[i],dataset$type[i],"pcp","(2012)"), ylab="intensity",
       xaxt="n")
  axis(1, at=seq(1,2161,120),seq(as.Date("2012-06-30")+1,as.Date("2012-06-30")+91,5))
  if (format(as.Date(dataset$date[i], origin = "1970-01-01"),"%Y")=="2012")
    abline(v=eline,col=3)
  abline(v=rain.cau-92*24*4,col="green3",lty=3)
  abline(v=rain.warn-92*24*4,col="red3",lty=3)
  
  plot(mtx2[(92*24*4+1):(92*24*5),i],type="l",col=1,ylim=c(0,max(mtx2[,i],na.rm=T)), main=paste(dataset$code[i],dataset$type[i],"run","(2012)"), ylab="intensity",
       xaxt="n")
  axis(1, at=seq(1,2161,120),seq(as.Date("2012-06-30")+1,as.Date("2012-06-30")+91,5))
  if (format(as.Date(dataset$date[i], origin = "1970-01-01"),"%Y")=="2012")
    abline(v=eline,col=3)
  
  plot(mtx3[(92*24*4+1):(92*24*5),i],type="l",col=1,ylim=c(0,max(mtx3[,i],na.rm=T)), main=paste(dataset$code[i],dataset$type[i],"sws","(2012)"), ylab="level",
       xaxt="n")
  axis(1, at=seq(1,2161,120),seq(as.Date("2012-06-30")+1,as.Date("2012-06-30")+91,5))
  if (format(as.Date(dataset$date[i], origin = "1970-01-01"),"%Y")=="2012")
    abline(v=eline,col=3)
  
  plot(mtx4[(92*24*4+1):(92*24*5),i],type="l",col=1,ylim=c(0,max(mtx4[,i],na.rm=T)), 
       main=paste(dataset$code[i],dataset$type[i],"The flash flood index","(2012)"), ylab="index",xaxt="n")
  axis(1, at=seq(1,2161,120),seq(as.Date("2012-06-30")+1,as.Date("2012-06-30")+91,5))
  abline(h=40,col=4,lty=2)
  abline(h=60,col=2,lty=2)
  if (format(as.Date(dataset$date[i], origin = "1970-01-01"),"%Y")=="2012")
    abline(v=eline,col=3)
  abline(v=rain.cau-92*24*4,col="green3",lty=3)
  abline(v=rain.warn-92*24*4,col="red3",lty=3)
  #################################################################################################
  plot(mtx1[(92*24*5+1):(92*24*6),i],type="l",col=1,ylim=c(0,max(mtx1[,i],na.rm=T)), main=paste(dataset$code[i],dataset$type[i],"pcp","(2013)"), ylab="intensity",
       xaxt="n")
  axis(1, at=seq(1,2161,120),seq(as.Date("2013-06-30")+1,as.Date("2013-06-30")+91,5))
  if (format(as.Date(dataset$date[i], origin = "1970-01-01"),"%Y")=="2013")
    abline(v=eline,col=3)
  abline(v=rain.cau-92*24*5,col="green3",lty=3)
  abline(v=rain.warn-92*24*5,col="red3",lty=3)
  
  plot(mtx2[(92*24*5+1):(92*24*6),i],type="l",col=1,ylim=c(0,max(mtx2[,i],na.rm=T)), main=paste(dataset$code[i],dataset$type[i],"run","(2013)"), ylab="intensity",
       xaxt="n")
  axis(1, at=seq(1,2161,120),seq(as.Date("2013-06-30")+1,as.Date("2013-06-30")+91,5))
  if (format(as.Date(dataset$date[i], origin = "1970-01-01"),"%Y")=="2013")
    abline(v=eline,col=3)
  
  plot(mtx3[(92*24*5+1):(92*24*6),i],type="l",col=1,ylim=c(0,max(mtx3[,i],na.rm=T)), main=paste(dataset$code[i],dataset$type[i],"sws","(2013)"), ylab="level",
       xaxt="n")
  axis(1, at=seq(1,2161,120),seq(as.Date("2013-06-30")+1,as.Date("2013-06-30")+91,5))
  if (format(as.Date(dataset$date[i], origin = "1970-01-01"),"%Y")=="2013")
    abline(v=eline,col=3)
  
  plot(mtx4[(92*24*5+1):(92*24*6),i],type="l",col=1,ylim=c(0,max(mtx4[,i],na.rm=T)), 
       main=paste(dataset$code[i],dataset$type[i],"The flash flood index","(2013)"), ylab="index",xaxt="n")
  axis(1, at=seq(1,2161,120),seq(as.Date("2013-06-30")+1,as.Date("2013-06-30")+91,5))
  abline(h=40,col=4,lty=2)
  abline(h=60,col=2,lty=2)
  if (format(as.Date(dataset$date[i], origin = "1970-01-01"),"%Y")=="2013")
    abline(v=eline,col=3)
  abline(v=rain.cau-92*24*5,col="green3",lty=3)
  abline(v=rain.warn-92*24*5,col="red3",lty=3)
}

# weight of linear for
# Need for optimization (how to)

# RUN part
#   run.part1<-exp(-0.339+0.338*run.mtx1+0.870*run.mtx2-0.936*run.mtx3)
#   run.part2<-run.part1/(1+run.part1)

# PCP part
#   pcp.part1<-exp(-4.805-0.009*pcp.mtx1+1.085*pcp.mtx2-0.517*pcp.mtx3)
#   pcp.part2<-pcp.part1/(1+pcp.part1)
# SWS part



run.part<-  0.037*run.mtx1-0.011*run.mtx2+0.013*run.mtx3
pcp.part<-  0.025*pcp.mtx1+0.022*pcp.mtx2+0.006*pcp.mtx3

a=61.5
b=18.5
index.mtx<-(run.part*0.613+pcp.part*0.836)/(0.836+0.613)*a+sws.mtx2*b

max(index.mtx,na.rm=T)

pdf("E:/wise/2015/r-work/rapid_flood/plot/check_index.pdf")
par(mfrow=c(4,1))
for(i in 1:38)
{fun.index(i)}
dev.off()
