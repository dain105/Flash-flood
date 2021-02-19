#자료 입력 
library(psych)
setwd("S:/Users/Dain/연구/돌발홍수/data/regeneration")
m6 = read.table(file="m6.csv", header = FALSE, sep=",")

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
  case<-make.data(m6,i,duration)
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
run.m6<-rep(NA,duration)
for(i in 1:38)
  run.m6<-rbind(run.m6,dataset$data[,1,i])

run.m6<-run.m6[complete.cases(run.m6),]


###########################################################################################
# Analysis of PCP
###########################################################################################
pcp.m6<-rep(NA,duration)
for(i in 1:38)
  pcp.m6<-rbind(pcp.m6,dataset$data[,2,i])
pcp.m6<-pcp.m6[complete.cases(pcp.m6),]


###########################################################################################
# Analysis of SWS
###########################################################################################
sws.m6<-rep(NA,duration)
for(i in 1:38)
sws.m6<-rbind(sws.m6,dataset$data[,3,i])
sws.m6<-sws.m6[complete.cases(sws.m6),]


#토양수분상태는 지역별로 다르기때문에 4년치 데이터에서 38개 지역의 최대토양상태를 계산해서 나눠준다.

sws.m63<-sws.m62<-sws.m6
for (i in 1:38){
  for (j in 1:6){
    sws.m62[i,j]<-max(sws.m6)-sws.m62[i,j]
    sws.m63[i,j]<-round((max(sws.m6)-sws.m6[i,j])/(max(sws.m6)-min(sws.m6)),3)
  }
}

###########################################################################
# machine learning
###########################################################################

response<-c(1,1,1,0,0,1,0,1,0,1,0,0,0,1,1,1,0,1,1,1,1,1,0,1,1,1,1,1,1,0,0,0,0,0,0,1,1,1)
machine.data<-cbind(pcp.m6, run.m6, sws.m63)
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
m6.rf<-round(r.rf.f10$overall,4)

#의사결정나무
tree<- rpart((response) ~ ., data=as.data.frame(data_1), method="class")
plot( tree)
text(tree, use.n=TRUE)
tree.pred <- predict(tree, testData=data_1,type='class')
Tabs.tree <- table(tree.pred,response)
dt <- confusionMatrix(Tabs.tree)
m6.dt<-round(dt$overall,4)

#서포트벡터머신
library(e1071)
svm.model<-svm(as.factor(response) ~ ., data=data_1)
svm.pred <- predict(svm.model, data_1)
Tabs.svm <- table(svm.pred, response)
svm<-confusionMatrix(Tabs.svm)
m6.svm<-round(svm $overall,4)

#나이브베이즈
nBayes <-naiveBayes(data_1,(as.factor(response)))
pred_nbayes<-predict(nBayes, data_1)
table(pred_nbayes, response)
tabs.nb<-table(pred_nbayes, response)
nb<-confusionMatrix(tabs.nb)
m6.nb<-round(nb$overall,4)

#로지스틱회귀분석
library(MASS)

glmg<-glm(response ~., family = "binomial", as.data.frame.array(data_1), maxit=100)
as.numeric(glmg$fitted.values)
summary(glmg)
fit<-round(glmg$fitted.values)
table(fit, response)
tabs.glmg<-table(fit, response)
nb<-confusionMatrix(tabs.glmg)
m6.glm<-round(nb$overall,4)

