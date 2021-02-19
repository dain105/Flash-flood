###########################################################################################
#디렉토리 지정 및 자료입력
setwd("S:/Users/Dain/연구/돌발홍수//data/regeneration")
LSM7 = read.table(file="LSM_3days_v7.csv", header = FALSE, sep=",")
duration<-6 #last 48hrs

###########################################################################################
# dataset after moving reporting time
###########################################################################################
dataset<-list()
dataset$code<-rep(NA,38)
dataset$type<-rep(NA,38)
dataset$date<-rep(NA,38)
dataset$e.time<-rep(NA,38)
dataset$data<-rep(NA,duration*4*38)
dim(dataset$data)<-c(duration,4,38)


for (i in 1:38)
{
case<-make.data(LSM7,i,duration)
case<-comb.data(case)

dataset$code[i]<-case[1,1]
dataset$type[i]<-case[2,1]
dataset$date[i]<-as.Date(substr(case[1,1],2,9),format="%Y%m%d")
dataset$e.time[i]<-(as.numeric(case[duration+5,1])-1) %% 24

case=matrix(as.numeric(case[4:(duration+3),1:4]),ncol=4)
dataset$data[,,i]<-case
}

###########################################################################################
# dataset with original reporting time
###########################################################################################

dataset7<-list()
dataset7$code<-rep(NA,38)
dataset7$type<-rep(NA,38)
dataset7$date<-rep(NA,38)
dataset7$e.time<-rep(NA,38)
dataset7$data<-rep(NA,duration*4*38)
dim(dataset7$data)<-c(duration,4,38)
for (i in 1:38)
{
  case7<-make.data(LSM7,i,duration)
  case7<-comb.data(case7)
  
  dataset7$code[i]<-case7[1,1]
  dataset7$type[i]<-case7[2,1]
  dataset7$date[i]<-as.Date(substr(case7[1,1],2,9),format="%Y%m%d")
  dataset7$e.time[i]<-(as.numeric(case7[duration+5,1])-1) %% 24
  
  case7=matrix(as.numeric(case7[4:(duration+3),1:4]),ncol=4)
  dataset7$data[,,i]<-case7
}

table(dataset$type)
sub.dataset<-cbind(dataset$type,1:38)


###########################################################################################
# generate RUN data
###########################################################################################

run.dataset<-rep(NA,duration)
for(i in 1:38)
  run.dataset<-rbind(run.dataset,dataset$data[,1,i])
  run.dataset<-run.dataset[complete.cases(run.dataset),]
  
###########################################################################################
# generate PCP data
###########################################################################################
pcp.dataset<-rep(NA,duration)
for(i in 1:38)
  pcp.dataset<-rbind(pcp.dataset,dataset$data[,2,i])
  pcp.dataset<-pcp.dataset[complete.cases(pcp.dataset),]

###########################################################################################
# generate sws data
###########################################################################################
sws.dataset<-rep(NA,duration)
for(i in 1:38)
  sws.dataset<-rbind(sws.dataset,dataset$data[,3,i])
  sws.dataset<-sws.dataset[complete.cases(sws.dataset),]

#토양수분상태는 지역별로 다르기때문에 38개 지역의 최대토양상태를 계산해서 나눠준다.
  sws.dataset3<-sws.dataset2<-sws.dataset
  for (i in 1:38){
    for (j in 1:6){
      sws.dataset2[i,j]<-max(sws.dataset)-sws.dataset2[i,j]
      sws.dataset3[i,j]<-round((max(sws.dataset)-sws.dataset[i,j])/(max(sws.dataset)-min(sws.dataset)),3)
    }
  }
  
  
  ###########################################################################################
  # Data merge
  ###########################################################################################
  
  response<-c(1,1,1,0,0,1,0,1,0,1,0,0,0,1,1,1,0,1,1,1,1,1,0,1,1,1,1,1,1,0,0,0,0,0,0,1,1,1)
  machine.data<-cbind(pcp.dataset, run.dataset, sws.dataset3,response)
  colnames(machine.data) <- c("pcp1","pcp2","pcp3","pcp4","pcp5","pcp6","run1","run2","run3","run4","run5","run6","sws1","sws2","sws3","sws4","sws5","sws6","response")
  write.csv(machine.data,"machine.data.csv")

  