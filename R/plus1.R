library(psych)

setwd("c:/work/rapid_flood")

#자료 입력 
p1 = read.table(file="E:/flash/p1.csv", header = FALSE, sep=",")
# event time 보정 자료


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
  case<-make.data(p1,i,duration)
  case<-comb.data(case)
  
  dataset$code[i]<-case[1,1]
  dataset$type[i]<-case[2,1]
  dataset$date[i]<-as.Date(substr(case[1,1],2,9),format="%Y%m%d")
  dataset$e.time[i]<-(as.numeric(case[duration+5,1])-1) %% 24
  
  case=matrix(as.numeric(case[4:(duration+3),1:4]),ncol=4)
  dataset$data[,,i]<-case
}
###############################################################
# subtract data related type "A"
###############################################################

# sub.A.data<-which(sub.dataset[,1] %in% c("A"))
# num.A=length(sub.A.data)

###############################################################
# Analysis of RUN
# using the intensity of hourly precipitation
###############################################################

run.p1<-rep(NA,duration)
for(i in 1:38)
  run.p1<-rbind(run.p1,dataset$data[,1,i])

run.p1<-run.p1[complete.cases(run.p1),]


###########################################################################################
# Analysis of PCP
# using the intensity of hourly precipitation
###########################################################################################
pcp.p1<-rep(NA,duration)
for(i in 1:38)
  pcp.p1<-rbind(pcp.p1,dataset$data[,2,i])
pcp.p1<-pcp.p1[complete.cases(pcp.p1),]

#   pcp.dataset3<-pcp.dataset2<-pcp.dataset
# for (i in 1:38){
#   for (j in 1:duration){
#     pcp.dataset2[i,j]<-round(mean(pcp.dataset[i,1:j]),1)
#     pcp.dataset3[i,j]<-sum(pcp.dataset[i,1:j])
#   }
# }

###########################################################################



###########################################################################################
# Analysis of sws
# using the raw scale of sws level
###########################################################################################
sws.p1<-rep(NA,duration)
for(i in 1:38)
  sws.p1<-rbind(sws.p1,dataset$data[,3,i])
sws.p1<-sws.p1[complete.cases(sws.p1),]

sws.p1.3<-sws.dataset2<-sws.p1
for (i in 1:num.A){
for (j in 1:duration){
sws.dataset2[i,j]<-max(sws.dataset)-sws.dataset2[i,j]
 sws.dataset3[i,j]<-round((max(sws.p1)-sws.dataset[i,j])/(max(sws.p1)-min(sws.dataset)),3)
 }
}



###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################

response<-c(1,1,1,0,0,1,0,1,0,1,0,0,0,1,1,1,0,1,1,1,1,1,0,1,1,1,1,1,1,0,0,0,0,0,0,1,1,1)
machine.p1<-cbind(pcp.p1, run.p1, sws.p1,response)
colnames(machine.p1) <- c("pcp1","pcp2","pcp3","pcp4","pcp5","pcp6","run1","run2","run3","run4","run5","run6","sws1","sws2","sws3","sws4","sws5","sws6","response")
library(randomForest)
library(caret)
library(rpart)
class(response)



#요인분석
library(psych)
p_cor<-princomp(machine.p1,cor=TRUE)
summary(p_cor)
screeplot(p_cor,type="lines")

pca.ma<-principal(machine.p1[,1:18], rotate="varimax", nfactors=4)
pca.ma$eigenvalue
pca.ma$scores<-pca.ma$scores[complete.cases(pca.ma$scores),]
data_1<-pca.ma$scores

#랜덤포레스트
rf <- randomForest(as.factor(response) ~ .,data_1, var.importance=TRUE)
rf$confusion
rf$predicted
Tabs.f10 <- table(rf$predicted, response)
r.rf.f10<-confusionMatrix(Tabs.f10)
round(r.rf.f10$overall,4)

#의사결정나무
tree<- rpart((response) ~ ., data=as.data.frame(data_1), method="class")
plot( tree)
text(tree, use.n=TRUE)
tree.pred <- predict(tree, testData=data_1,type='class')
Tabs.tree <- table(tree.pred,response)
dt <- confusionMatrix(Tabs.tree)
round(dt$overall,4)

#서포트벡터머신
library(e1071)
svm.model<-svm(as.factor(response) ~ ., data=data_1)
svm.pred <- predict(svm.model, data_1)
Tabs.svm <- table(svm.pred, response)
svm<-confusionMatrix(Tabs.svm)
round(svm $overall,4)

#나이브베이즈
nBayes <-naiveBayes(data_1,(as.factor(response)))
pred_nbayes<-predict(nBayes, data_1)
table(pred_nbayes, response)
tabs.nb<-table(pred_nbayes, response)
nb<-confusionMatrix(tabs.nb)
round(nb$overall,4)

#로지스틱회귀분석
library(MASS)
library(VIF)

glmg<-glm(response ~., family = "binomial", as.data.frame.array(data_1), maxit=100)
as.numeric(glmg$fitted.values)
summary(glmg)
fit<-round(glmg$fitted.values)
table(fit, response)
tabs.glmg<-table(fit, response)
nb<-confusionMatrix(tabs.glmg)
round(nb$overall,4)

