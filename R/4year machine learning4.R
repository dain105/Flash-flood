

sws.main = read.table(file="E:/flash/data/sws.main.csv", header = FALSE, sep=",")
#토양수분상태는 지역별로 다르기때문에 4년치 데이터에서 38개 지역의 최대토양상태를 계산해서 나눠준다.
sws.mtx<-as.matrix(read.table("E:/flash/data/1h_sws_mtx.csv",sep=","),ncol=92,byrow=T)
sws<-sws.mtx[2208:11040,]
sws.dataset<-as.matrix(t(sws))

sws.main3<-sws.main2<-sws.main


vec1<-vec2<-rep(NA,38)
for (i in 1:38)
{vec1[i]=max(sws.dataset[i,])
vec2[i]=min(sws.dataset[i,])}
vec1
vec2

for (i in 1:38)  {
  sws.main3[i,]<-(sws.main[i,]-vec2[i])/(vec1[i]-vec2[i])
}




response<-c(1,1,1,0,0,1,0,1,0,1,0,0,0,1,1,1,0,1,1,1,1,1,0,1,1,1,1,1,1,0,0,0,0,0,0,1,1,1)
machine.data<-cbind(pcp.dataset, run.dataset, sws.main3,response)
colnames(machine.data) <- c("pcp1","pcp2","pcp3","pcp4","pcp5","pcp6","run1","run2","run3","run4","run5","run6","sws1","sws2","sws3","sws4","sws5","sws6","response")
library(randomForest)
library(caret)
library(rpart)
class(response)


#랜덤포레스트
rf <- randomForest(as.factor(response) ~ .,machine.data, var.importance=TRUE)
rf$confusion
rf$predicted
Tabs.f10 <- table(rf$predicted, response)
r.rf.f10<-confusionMatrix(Tabs.f10)
m0.rf<-round(r.rf.f10$overall,4)
varImpPlot(rf)

#의사결정나무
tree<- rpart((response) ~ ., data=as.data.frame(machine.data), method="class")
plot( tree)
text(tree, use.n=TRUE)
tree.pred <- predict(tree, testData=machine.data,type='class')
Tabs.tree <- table(tree.pred,response)
dt <- confusionMatrix(Tabs.tree)
m0.dt<-round(dt$overall,4)

#서포트벡터머신
library(e1071)
svm.model<-svm(as.factor(response) ~ ., data=machine.data)
svm.pred <- predict(svm.model, machine.data)
Tabs.svm <- table(svm.pred, response)
svm<-confusionMatrix(Tabs.svm)
m0.svm<-round(svm $overall,4)

#나이브베이즈
nBayes <-naiveBayes(machine.data,(as.factor(response)))
nBayes
pred_nbayes<-predict(nBayes, machine.data)
table(pred_nbayes, response)
tabs.nb<-table(pred_nbayes, response)
nb<-confusionMatrix(tabs.nb)
m0.nb<-round(nb$overall,4)

#로지스틱회귀분석
library(MASS)

glmg<-glm(response ~., family = "binomial", as.data.frame.array(machine.data), maxit=100)
as.numeric(glmg$fitted.values)
summary(glmg)
fit<-round(glmg$fitted.values)
table(fit, response)
tabs.glmg<-table(fit, response)
nb<-confusionMatrix(tabs.glmg)
m0.glm<-round(nb$overall,4)

################################## 4년치 머신러닝##########
pcp4.dataset<-read.table("E:/flash/data/pcp.csv",sep=",",header=T)
run4.dataset<-read.table("E:/flash/data/run.csv",sep=",",header=T)
sws4.dataset<-read.table("E:/flash/data/sws1.csv",sep=",",header=T)

data2<- as.matrix(cbind(pcp4.dataset[,2:7], run4.dataset[,2:7], sws4.dataset[,2:7]))
colnames(data2) <- c("pcp1","pcp2","pcp3","pcp4","pcp5","pcp6","run1","run2","run3","run4","run5","run6","sws1","sws2","sws3","sws4","sws5","sws6")


#의사결정나무
tree<- rpart((response) ~ ., data=as.data.frame(machine.data), method="class")
plot( tree)
text(tree, use.n=TRUE)
tree.pred <- predict(tree, as.data.frame(data2), type="class")
t<-as.vector(tree.pred)
tail(cumsum(t),10)



#랜덤포레스트

rf <- randomForest(as.factor(response) ~ .,machine.data, var.importance=TRUE)
rf$confusion
rf.pred <- predict(rf, data2)
r<-as.vector(rf.pred)
cumsum(r)
tail(cumsum(r),10)



#서포트벡터머신
library(e1071)
svm.model<-svm(as.factor(response) ~ ., data=machine.data)
svm.pred <- predict(svm.model, data2)
s<-as.vector(svm.pred)
tail(cumsum(s),10)


#나이브베이즈
nBayes <-naiveBayes(machine.data,(as.factor(response)))
pred_nbayes<-predict(nBayes, data2)
n<-as.vector(pred_nbayes)
tail(cumsum(n),10)

conn<-cbind(data2, n)

write.csv(conn,"E:/flash/data/conn.csv")

#로지스틱회귀분석
library(MASS)
glmg<-glm(response ~., family = "binomial", as.data.frame.array(machine.data), maxit=100)
as.numeric(glmg$fitted.values)
summary(glmg)
fit<-round(glmg$fitted.values)
table(fit, response)
tabs.glmg<-table(fit, response)
nb<-confusionMatrix(tabs.glmg)
m0.glm<-round(nb$overall,4)
pred <- predict(glmg, newdata=as.data.frame(data2),type="response")
pred1<-round(pred)
g<-as.vector(pred1)
cumsum(g)
tail(cumsum(g),10)
