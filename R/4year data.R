pcp.mtx<-as.matrix(read.table("E:/flash/data/1h_pcp_mtx.csv",sep=","),ncol=92,byrow=T)
run.mtx<-as.matrix(read.table("E:/flash/data/1h_run_mtx.csv",sep=","),ncol=92,byrow=T)
sws.mtx<-as.matrix(read.table("E:/flash/data/1h_sws_mtx.csv",sep=","),ncol=92,byrow=T)
pcp<-pcp.mtx[2209:11040,]
run<-run.mtx[2209:11040,]
sws.dataset<-sws.mtx[2209:11040,]


#4year 강우량 데이터 생성
a<-as.matrix(pcp)
aa=function(i){
return(a[(i):(5+i)])  
}

bb=matrix(0,nrow=335611, ncol=6)
for(i in 1:335611 )
  {
 bb[i,]=aa(i)
}

write.csv(bb,"E:/flash/data/pcp.csv")




#4year 지표유출량 데이터 생성
a<-as.matrix(run)
aa=function(i){
  return(a[(i):(5+i)])  
}

bb=matrix(0,nrow=335611, ncol=6)
for(i in 1:335611)
{
  bb[i,]=aa(i)
}
write.csv(bb,"E:/flash/data/run.csv")



#토양수분상태는 지역별로 다르기때문에 4년치 데이터에서 38개 지역의 최대토양상태를 계산해서 나눠준다.
sws.mtx<-as.matrix(read.table("E:/flash/data/1h_sws_mtx.csv",sep=","),ncol=92,byrow=T)
sws<-sws.mtx[2208:11040,]
sws.dataset<-as.matrix(t(sws))

sws.dataset3<-sws.dataset2<-sws.dataset


vec1<-vec2<-rep(NA,38)
for (i in 1:38)
{vec1[i]=max(sws.dataset[i,])
vec2[i]=min(sws.dataset[i,])}
vec1
vec2

for (i in 1:38)  {
  sws.dataset3[i,]<-(sws.dataset[i,]-vec2[i])/(vec1[i]-vec2[i])
}
a<-as.matrix(t(sws.dataset3))




#6시간별로 나누기
aa=function(i){
  return(a[(i):(5+i)])  
}


bb=matrix(0,nrow=335611, ncol=6)
for(i in 1:335611){
  bb[i,]=aa(i)
}

#엑셀로 데이터 저장
write.csv(bb,"E:/flash/data/sws.csv")

sws.dataset<-read.table("E:/flash/data/sws.csv",sep=",")

