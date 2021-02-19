##########################################################
# function <- make.data
# raw data로 부터 원하는 형태의 자료 생성
# case$code : 발생지점
# case$type : 발생형태
# case$data : 입력자료 (run, pcp, sws, wtd)
# case$e.time : 돌발홍수 신고시점
###########################################################

make.data<-function(LSM, i,duration){
  case<-list()
  case$code <- paste(LSM[2,(2+5*(i-1))])
  case$type <- paste(LSM[76,(2+5*(i-1))])
  case$date<- as.Date(substr(paste(LSM[2,(2+5*(i-1))]),2,9),format="%Y%m%d")
  case$data <- as.matrix(LSM[4:75,c((2+5*(i-1)):(6+5*(i-1)))])
  case$data<-matrix(as.numeric(case$data),ncol=5)
  colnames(case$data)<-c("run","pcp","sws","wtd","event")
  
  for (i in 1:72)
    if (!is.na(case$data[i,5])) case$e.time=i
  
  case$intensity1<-cal.iten(case,1,duration)
  case$intensity2<-cal.iten(case,2,duration)
  case$intensity3<-cal.iten(case,3,duration)
  case$intensity4<-cal.iten(case,4,duration)
  
  return(case)
}


#########################################################
# function <- cal.iten
# parameter : case, time
# A function for calculation the intensity of precipitation given time
#########################################################
cal.iten<-function(case,var.num, duration){
  if(!var.num %in% c(1,2,3,4))  stop("num is not valid")
  
  intensity<-rep(NA,duration)
  
  for (time in 1:duration){
  e.time<-case$e.time
  intensity[time]<-(case$data[(e.time-time)+1:time,var.num])  }
  
  return(intensity)
}

#########################################################
# make combinded dataset
# function <- comb.data

comb.data<-function(case){
  c1<-c(case$code, case$type, case$e.time, case$intensity1, case$date, case$e.time)
  c2<-c(case$code, case$type, case$e.time, case$intensity2, case$date, case$e.time)
  c3<-c(case$code, case$type, case$e.time, case$intensity3, case$date, case$e.time)
  c4<-c(case$code, case$type, case$e.time, case$intensity4, case$date, case$e.time)

  return(t(rbind(c1,c2,c3,c4)))
}

#########################################################
# function <- basic.plot
# code별 기본 그림 생성 
# #######################################################

basic.plot<-function(case){
par(mfrow=c(4,1))
plot(case$data[,1],type="l",col="blue",lwd=1.5,
     main=paste("code=",case$code," type (",case$type,")", "   RUN",sep=""))
abline(v=case$e.time,col="red",lwd=1)
plot(case$data[,2],type="l",col="black",lwd=1.5,
     main=paste("code=",case$code," type (",case$type,")", "   PCP",sep=""))
abline(v=case$e.time,col="red",lwd=1)
plot(case$data[,3],type="l",col="green",lwd=1.5,
     main=paste("code=",case$code," type (",case$type,")", "   SWS",sep=""))
abline(v=case$e.time,col="red",lwd=1)
plot(case$data[,4],type="l",col="black",lwd=1.5,
     main=paste("code=",case$code," type (",case$type,")", "   WTD",sep=""))
abline(v=case$e.time,col="red",lwd=1)
par(mfrow=c(1,1))
}

