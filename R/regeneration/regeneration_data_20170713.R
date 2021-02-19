library(stringr)


setwd("S:/Users/Dain/연구/돌발홍수/data/regeneration")

pcp.mtx <- read.csv("1h_pcp_mtx.csv", header=T)
run.mtx <- read.csv("1h_run_mtx.csv", header=T)
sws.dataset <- read.csv("sws.dataset3.csv", header=T)

#토양수분상태 데이터 변환
#토양수분상태는 지역별로 다르기때문에 4년치 데이터에서 38개 지역의 최대토양상태를 계산해서 나눠준다.
#sws.dataset <- read.csv("1h_sws_mtx.csv", header=T)
 #sws.dataset3<-sws.dataset2<-sws.dataset
 #for (i in 1:13248){
 # for (j in 1:38){
 #     sws.dataset2[i,j]<-max(sws.dataset)-sws.dataset2[i,j]
 #    sws.dataset3[i,j]<-round((max(sws.dataset)-sws.dataset[i,j])/(max(sws.dataset)-min(sws.dataset)),3)
 #   }
 # }
 # write.csv(sws.dataset3,"sws.dataset3.csv")

###########################################################
#date 생성
###########################################################
cnt=1
date<-list()

for (year in 2008:2013){
  for(mon in 7:9){
    for (day in 1:31){
      for (time in 1:24){
      date[cnt]<-paste0(year,"-",mon,"-",day,"-",time)
      cnt=cnt+1
      }
    }
  }
}
date<-as.matrix(unlist(date))
date<-as.matrix(date[substr(date,6,9)!="9-31"])

length(date)
dim(pcp.mtx)

##########################################################################################
##########################################################################################
#2009~2012까지 데이터 분할
##########################################################################################
##########################################################################################
pcp<-pcp.mtx[2209:11040,]
run<-run.mtx[2209:11040,]
sws<-sws.dataset[2209:11040,]
date<-as.matrix(date[2209:11040,])
dim(date)
##########################################################################################
#4year 강우량 데이터 생성
#6개씩 나눔
##########################################################################################

a<-as.matrix(pcp)
aa=function(i){
  return(a[(i):(5+i)])  
}

pcp.dataset=matrix(0,nrow=335611, ncol=6)
for(i in 1:335611 )
{
  pcp.dataset[i,]=aa(i)
}

# write.csv(bb,"pcp.csv")


##########################################################################################
#4year 지표유출량 데이터 생성
#6개씩 나눔
##########################################################################################

a<-as.matrix(run)
aa=function(i){
  return(a[(i):(5+i)])  
}

run.dataset=matrix(0,nrow=335611, ncol=6)
for(i in 1:335611)
{
  run.dataset[i,]=aa(i)
}
# write.csv(bb,"run.csv")



##########################################################################################
#4year 토양수분상태 데이터 생성
#6개씩 나눔
##########################################################################################
a<-as.matrix(sws)
aa=function(i){
  return(a[(i):(5+i)])  
}


sws.dataset=matrix(0,nrow=335611, ncol=6)
for(i in 1:335611){
  sws.dataset[i,]=aa(i)
}

#엑셀로 데이터 저장
# write.csv(bb,"E:/flash/data/sws.csv")

##########################################################################################
#date  데이터 생성
#6개씩 나눔
##########################################################################################
a<-as.matrix(date)
aa=function(i){
  return(a[(i):(5+i)])  
}

date.dataset=matrix(0,nrow=335611, ncol=6)
for(i in 1:335611 )
{
  date.dataset[i,]=aa(i)
}

#write.csv(bb,"date.csv")

##########################################################################################
##########################################################################################
#merge data
##########################################################################################
##########################################################################################

dataset<-cbind(date.dataset,pcp.dataset,run.dataset,sws.dataset)
colnames(dataset) <- c("date1","date2","date3","date4","date5","date6","pcp1","pcp2","pcp3","pcp4","pcp5","pcp6","run1","run2","run3","run4","run5","run6","sws1","sws2","sws3","sws4","sws5","sws6")
a<-abs(as.numeric(str_sub(dataset[,6],-2)))
dataset2<-as.data.frame(cbind(dataset,a))
dataset3<-subset(dataset2,a==(6:24))  

write.csv(dataset3,"fyear.data.csv")
