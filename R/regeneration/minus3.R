#자료 입력 
library(psych)
setwd("S:/Users/Dain/연구/돌발홍수/data/regeneration")
m3 = read.table(file="m3.csv", header = FALSE, sep=",")

duration<-6 #last 48hrs

########################################################
# dataset after moving reporting time
########################################################
dataset<-list()
dataset$code<-rep(NA,38)
dataset$type<-rep(NA,38)
dataset$date<-rep(NA,38)
dataset$e.time<-rep(NA,38)
dataset$data<-rep(NA,duration*4*38)
dim(dataset$data)<-c(duration,4,38)


for (i in 1:38)
{
  case<-make.data(m3,i,duration)
  case<-comb.data(case)
  
  dataset$code[i]<-case[1,1]
  dataset$type[i]<-case[2,1]
  dataset$date[i]<-as.Date(substr(case[1,1],2,9),format="%Y%m%d")
  dataset$e.time[i]<-(as.numeric(case[duration+5,1])-1) %% 24
  
  case=matrix(as.numeric(case[4:(duration+3),1:4]),ncol=4)
  dataset$data[,,i]<-case
}

###########################################################################################
# Analysis of RUN
###########################################################################################
run.m3<-rep(NA,duration)
for(i in 1:38)
  run.m3<-rbind(run.m3,dataset$data[,1,i])

run.m3<-run.m3[complete.cases(run.m3),]


###########################################################################################
# Analysis of PCP
###########################################################################################
pcp.m3<-rep(NA,duration)
for(i in 1:38)
  pcp.m3<-rbind(pcp.m3,dataset$data[,2,i])
pcp.m3<-pcp.m3[complete.cases(pcp.m3),]


###########################################################################################
# Analysis of SWS
###########################################################################################
sws.m3<-rep(NA,duration)
for(i in 1:38)
sws.m3<-rbind(sws.m3,dataset$data[,3,i])
sws.m3<-sws.m3[complete.cases(sws.m3),]


#토양수분상태는 지역별로 다르기때문에 4년치 데이터에서 38개 지역의 최대토양상태를 계산해서 나눠준다.

sws.m33<-sws.m32<-sws.m3
for (i in 1:38){
  for (j in 1:6){
    sws.m32[i,j]<-max(sws.m3)-sws.m32[i,j]
    sws.m33[i,j]<-round((max(sws.m3)-sws.m3[i,j])/(max(sws.m3)-min(sws.m3)),3)
  }
}

###########################################################################
# machine learning
###########################################################################

response<-c(1,1,1,0,0,1,0,1,0,1,0,0,0,1,1,1,0,1,1,1,1,1,0,1,1,1,1,1,1,0,0,0,0,0,0,1,1,1)
machine.data<-cbind(pcp.m3, run.m3, sws.m33)
data_1<-cbind(as.matrix(machine.data[,1:18])%*%(pca.mat),response)
colnames(data_1) <- c("RC1","RC2","RC3","response")
library(randomForest)
library(caret)
library(rpart)
class(response)


#랜덤포레스트
rf <- randomForest(as.factor(response) ~ .,data_1, var.importance=TRUE)
rf$confusion
rf$predicted
Tabs.f10 <- table(rf$predicted, response)
r.rf.f10<-confusionMatrix(Tabs.f10)
m3.rf<-round(r.rf.f10$overall,4)

#의사결정나무
tree<- rpart((response) ~ ., data=as.data.frame(data_1), method="class")
plot( tree)
text(tree, use.n=TRUE)
tree.pred <- predict(tree, testData=data_1,type='class')
Tabs.tree <- table(tree.pred,response)
dt <- confusionMatrix(Tabs.tree)
m3.dt<-round(dt$overall,4)

#서포트벡터머신
library(e1071)
svm.model<-svm(as.factor(response) ~ ., data=data_1)
svm.pred <- predict(svm.model, data_1)
Tabs.svm <- table(svm.pred, response)
svm<-confusionMatrix(Tabs.svm)
m3.svm<-round(svm $overall,4)

#나이브베이즈
nBayes <-naiveBayes(data_1,(as.factor(response)))
pred_nbayes<-predict(nBayes, data_1)
table(pred_nbayes, response)
tabs.nb<-table(pred_nbayes, response)
nb<-confusionMatrix(tabs.nb)
m3.nb<-round(nb$overall,4)

#로지스틱회귀분석
library(MASS)

glmg<-glm(response ~., family = "binomial", as.data.frame.array(data_1), maxit=100)
as.numeric(glmg$fitted.values)
summary(glmg)
fit<-round(glmg$fitted.values)
table(fit, response)
tabs.glmg<-table(fit, response)
nb<-confusionMatrix(tabs.glmg)
m3.glm<-round(nb$overall,4)

