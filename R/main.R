library(psych)

setwd("c:/work/rapid_flood")
#자료 입력 
LSM5 = read.table(file="E:/flash/LSM_3days_v5.csv", header = FALSE, sep=",")
# event time 보정 자료
LSM = read.table(file="E:/flash/LSM_3days_v6.csv", header = FALSE, sep=",")

# ########################################################
# # 그림으로 저장
 pdf("plot/basic_plot.pdf")
 
 for (i in 1:38)
 { case<-make.data(LSM,i,1)
   case5<-make.data(LSM5,i,1)
   basic.plot(case5)
   basic.plot(case)
}
 
 dev.off()
# 
########################################################
# Combining dataset and Check 

duration<-6 #last 48hrs


########################################################
# dataset after moving reporting time
dataset<-list()
dataset$code<-rep(NA,38)
dataset$type<-rep(NA,38)
dataset$date<-rep(NA,38)
dataset$e.time<-rep(NA,38)
dataset$data<-rep(NA,duration*4*38)
dim(dataset$data)<-c(duration,4,38)


for (i in 1:38)
{
case<-make.data(LSM,i,duration)
case<-comb.data(case)

dataset$code[i]<-case[1,1]
dataset$type[i]<-case[2,1]
dataset$date[i]<-as.Date(substr(case[1,1],2,9),format="%Y%m%d")
dataset$e.time[i]<-(as.numeric(case[duration+5,1])-1) %% 24

case=matrix(as.numeric(case[4:(duration+3),1:4]),ncol=4)
dataset$data[,,i]<-case
}

#############################################
# dataset with original reporting time
dataset5<-list()
dataset5$code<-rep(NA,38)
dataset5$type<-rep(NA,38)
dataset5$date<-rep(NA,38)
dataset5$e.time<-rep(NA,38)
dataset5$data<-rep(NA,duration*4*38)
dim(dataset5$data)<-c(duration,4,38)
for (i in 1:38)
{
  case5<-make.data(LSM5,i,duration)
  case5<-comb.data(case5)
  
  dataset5$code[i]<-case5[1,1]
  dataset5$type[i]<-case5[2,1]
  dataset5$date[i]<-as.Date(substr(case5[1,1],2,9),format="%Y%m%d")
  dataset5$e.time[i]<-(as.numeric(case5[duration+5,1])-1) %% 24
  
  case5=matrix(as.numeric(case5[4:(duration+3),1:4]),ncol=4)
  dataset5$data[,,i]<-case5
}

table(dataset$type)
sub.dataset<-cbind(dataset$type,1:38)

###############################################################
# subtract data related type "A"
###############################################################

# sub.A.data<-which(sub.dataset[,1] %in% c("A"))
# num.A=length(sub.A.data)

###############################################################
# Analysis of RUN
# using the intensity of hourly precipitation
###############################################################

run.dataset<-rep(NA,duration)
for(i in 1:38)
  run.dataset<-rbind(run.dataset,dataset$data[,1,i])

  run.dataset<-run.dataset[complete.cases(run.dataset),]
  
###########################################################################
# Factor analysis (RUN)
run.fit <-principal(run.dataset,nfactors=3,rotate="varimax", scores=TRUE)

plot(run.fit$loadings[,1],col=2,type="l",ylim=c(-0.1,1), xlab="time(hour)", ylab="loadings"
     ,main="The result of RUN (Principal component analysis with varimax rotation)")
lines(run.fit$loadings[,2],col="blue")
lines(run.fit$loadings[,3],col="black")

abline(v=3,col="blue",lwd=1,lty=2)
abline(v=4,col="red",lwd=1,lty=2)
axis(1,at=c(1:48),cex=0.7)

run.fit

###########################################################################################
# Analysis of PCP
# using the intensity of hourly precipitation
###########################################################################################
pcp.dataset<-rep(NA,duration)
for(i in 1:38)
  pcp.dataset<-rbind(pcp.dataset,dataset$data[,2,i])
  pcp.dataset<-pcp.dataset[complete.cases(pcp.dataset),]

#   pcp.dataset3<-pcp.dataset2<-pcp.dataset
# for (i in 1:38){
#   for (j in 1:duration){
#     pcp.dataset2[i,j]<-round(mean(pcp.dataset[i,1:j]),1)
#     pcp.dataset3[i,j]<-sum(pcp.dataset[i,1:j])
#   }
# }

###########################################################################
# Factor analysis (PCP)
pcp.fit <-principal(pcp.dataset,nfactors=3,rotate="varimax")
pcp.fit

plot(pcp.fit$loadings[,1],col=2,type="l",ylim=c(-0.1,1), xlab="time(hour)", ylab="loadings"
     ,main="The result of PCP data (after principal component analysis)")
lines(pcp.fit$loadings[,2],col="blue")
lines(pcp.fit$loadings[,3],col="black")

abline(v=15,col="green",lwd=2,lty=2)
abline(v=4,col="black",lwd=1,lty=2)
abline(v=5,col="blue",lwd=1,lty=2)
abline(v=10,col="blue",lwd=1,lty=2)
abline(v=11,col="red",lwd=1,lty=2)
axis(1,at=c(1:duration))


###########################################################################################
# Analysis of sws
# using the raw scale of sws level
###########################################################################################
sws.dataset<-rep(NA,duration)
for(i in 1:38)
  sws.dataset<-rbind(sws.dataset,dataset$data[,3,i])
  sws.dataset<-sws.dataset[complete.cases(sws.dataset),]

#sws.dataset3<-sws.dataset2<-sws.dataset
  #for (i in 1:num.A){
  #for (j in 1:duration){
  #sws.dataset2[i,j]<-max(sws.dataset)-sws.dataset2[i,j]
  # sws.dataset3[i,j]<-round((max(sws.dataset)-sws.dataset[i,j])/(max(sws.dataset)-min(sws.dataset)),3)
  # }
  #}

###########################################################################
# Factor analysis (sws)
sws.fit <-principal(sws.dataset,nfactors=2,rotate="varimax")

plot(sws.fit$loadings[,1],col=2,type="l",ylim=c(0,1), xlab="time(hour)", ylab="loadings"
     ,main="The result of SWS data (after principal component analysis)")
lines(sws.fit$loadings[,2],col="blue")
lines(sws.fit$loadings[,3],col="black")

abline(v=duration,col="green",lwd=2,lty=2)
abline(v=6,col="black",lwd=1,lty=2)
abline(v=7,col="blue",lwd=1,lty=2)
abline(v=15,col="blue",lwd=1,lty=2)
abline(v=16,col="red",lwd=1,lty=2)
axis(1,at=c(1:duration))



#######################################################################################
r.short<-(run.dataset[,1]*run.fit$loadings[1,2]+run.dataset[,2]*run.fit$loadings[2,2])/sum(run.fit$loadings[1:2,2])
r.mid<-(run.dataset[,3]*run.fit$loadings[3,3])
r.long<-(run.dataset[,4]*run.fit$loadings[4,1]+run.dataset[,5]*run.fit$loadings[5,1]+run.dataset[,6]*run.fit$loadings[6,1])/sum(run.fit$loadings[4:6,1])

p.short<-(pcp.dataset[,1]*pcp.fit$loadings[1,1]+pcp.dataset[,2]*pcp.fit$loadings[2,1]+pcp.dataset[,3]*pcp.fit$loadings[3,1])/sum(pcp.fit$loadings[1:3,1])
p.mid<-(pcp.dataset[,4]*pcp.fit$loadings[4,3])
p.long<-(pcp.dataset[,5]*pcp.fit$loadings[5,2]+pcp.dataset[,6]*pcp.fit$loadings[6,2])/(sum(pcp.fit$loadings[5:6,2]))

response<-c(1,    1,    1,    0,  0,  1,  0,  1,  0,  1,  0,  0,  0,  1,  1,  1,  0,  1,
            1,  1,  1,  1,  0,  1,  1,  1,  1,  1,  1,  0,  0,  0,  0,  0,  0,  1,  1,  1)

run.result<-lm(response~0+r.short+r.mid+r.long)
#plot(run.result)
summary(run.result)

# run.result2<-glm(response~r.short+r.mid+r.long, family="binomial")
# plot(run.result2)

pcp.result<-lm(response~0+p.short+p.mid+p.long)
#plot(pcp.result)
summary(pcp.result)


 plot(p.short,response, xlab="단기 강우량", ylab="돌발홍수여부")
 abline(lm(response~p.short),col=2)
 abline(glm(response~p.short, family="binomial"),col=3)
 legend(30,0.2, col=c(2,3), legend=c("multiple-regression","logistic-regression"), lty=1)
 
##########################################################################

 pcp.mtx<-as.matrix(read.table("data/1h_pcp_mtx.csv",sep=","),ncol=92,byrow=T)
 run.mtx<-as.matrix(read.table("data/1h_run_mtx.csv",sep=","),ncol=92,byrow=T)
 sws.mtx<-as.matrix(read.table("data/1h_sws_mtx.csv",sep=","),ncol=92,byrow=T)
 
 
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
 
 
 run.part<-  0.037*run.mtx1-0.011*run.mtx2+0.013*run.mtx3
 pcp.part<-  0.025*pcp.mtx1+0.022*pcp.mtx2+0.006*pcp.mtx3
 
 opt.ffin<-function(a,b){
   
   index.mtx<-(run.part*0.613+pcp.part*0.836)/(0.836+0.613)*a+sws.mtx2*b
   
   nindex<-matrix(rep(0,13248*38),ncol=38)
   optim.index<-matrix(rep(0,13248*38),ncol=38)
   
   for (i in 1:38){
     
     y.gap<-92*24*abs(2009-as.numeric(format(as.Date(dataset$date[i], origin = "1970-01-01"),"%Y")))
     gap=as.Date(dataset$date[i], origin = "1970-01-01")-as.Date(paste(format(as.Date(dataset$date[i], origin = "1970-01-01"),"%Y"),"-07-01",sep=""))
     eline=y.gap+as.numeric(gap)*24+dataset$e.time[i]
     optim.index[eline,i]=1
     # rain.cau<-which(pcp.mtx4[,i]==1)
     # rain.warn<-which(pcp.mtx4[,i]==2)
     
     nindex[which(index.mtx[,i]>=40),i]=1
   }
   
   
   check.mtx1<-sum(optim.index*index.mtx,na.rm=T)
   check.mtx2<-sum(nindex*index.mtx,na.rm=T) 
   
   return(c(check.mtx1,check.mtx2,check.mtx1/check.mtx2*100))
 }
 
 length(seq(1,80,0.1))
 
 tp.mtx<-matrix(rep(0,791*3),ncol=3)
 
 cnt=0
 for (i in seq(1,80,0.1))
 {cnt=cnt+1
 tp.mtx[cnt,]<-opt.ffin(i,80-i)
 }
 
 plot(tp.mtx[,3],type="l",xaxt="n", ylab="value", xlab="weight 1 prime")
 axis(1, at=seq(1,791,40), label=seq(1,80,4))
 
 abline(v=which(tp.mtx[,3]==max(tp.mtx[,3])),col=2, lty=2)
 
 legend(610,0.03, legend="61.5", text.col=2, border="white", box.col="white")
 
