


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


#######################4년치데이터################
pcp4.dataset<-read.table("E:/flash/data/pcp.csv",sep=",",header=T)
run4.dataset<-read.table("E:/flash/data/run.csv",sep=",",header=T)
sws4.dataset<-read.table("E:/flash/data/sws.csv",sep=",",header=T)

data2_1<-as.matrix(cbind(pcp4.dataset[,2:7], run4.dataset[,2:7], sws4.dataset[,2:7]))
colnames(data2_1) <- c("pcp1","pcp2","pcp3","pcp4","pcp5","pcp6","run1","run2","run3","run4","run5","run6","sws1","sws2","sws3","sws4","sws5","sws6")

#4년치데이터로 요인분석
#요인분석
library(psych)
p_cor<-princomp(data2_1,cor=TRUE)
summary(p_cor)
screeplot(p_cor,type="lines")

pca.ma<-principal(data2_1[,1:18], rotate="varimax", nfactors=3)
pca.mat<-pca.ma$weights
data_1<-cbind(as.matrix(machine.data[,1:18])%*%(pca.mat),response)
data_2<-pca.ma$scores



#랜덤포레스트
rf <- randomForest(as.factor(response) ~ .,data_1, var.importance=TRUE)
rf$confusion
rf$predicted
Tabs.f10 <- table(rf$predicted, response)
r.rf.f10<-confusionMatrix(Tabs.f10)
m0.rf<-round(r.rf.f10$overall,4)

#의사결정나무
tree<- rpart((response) ~ ., data=as.data.frame(data_1), method="class")
plot( tree)
text(tree, use.n=TRUE)
tree.pred <- predict(tree, testData=data_1,type='class')
Tabs.tree <- table(tree.pred,response)
dt <- confusionMatrix(Tabs.tree)
m0.dt<-round(dt$overall,4)

#서포트벡터머신
library(e1071)
svm.model<-svm(as.factor(response) ~ ., data=data_1)
svm.pred <- predict(svm.model, data_1)
Tabs.svm <- table(svm.pred, response)
svm<-confusionMatrix(Tabs.svm)
m0.svm<-round(svm $overall,4)

#나이브베이즈
nBayes <-naiveBayes(data_1,(as.factor(response)))
pred_nbayes<-predict(nBayes, data_1)
table(pred_nbayes, response)
tabs.nb<-table(pred_nbayes, response)
nb<-confusionMatrix(tabs.nb)
m0.nb<-round(nb$overall,4)

#로지스틱회귀분석
library(MASS)

glmg<-glm(response ~., family = "binomial", as.data.frame.array(data_1), maxit=100)
as.numeric(glmg$fitted.values)
summary(glmg)
fit<-round(glmg$fitted.values)
table(fit, response)
tabs.glmg<-table(fit, response)
nb<-confusionMatrix(tabs.glmg)
m0.glm<-round(nb$overall,4)

################################## 4년치 머신러닝##########


#랜덤포레스트

rf <- randomForest(as.factor(response) ~ .,data_1, var.importance=TRUE)
rf$confusion
rf.pred <- predict(rf, data_2)
r<-as.vector(rf.pred)
tail(cumsum(r),10)

#의사결정나무
tree<- rpart((response) ~ ., data=as.data.frame(data_1), method="class")
plot( tree)
text(tree, use.n=TRUE)
tree.pred <- predict(tree, as.data.frame(data_2), type="class")
t<-as.vector(tree.pred)
tail(cumsum(t),10)

#서포트벡터머신
library(e1071)
svm.model<-svm(as.factor(response) ~ ., data=data_1)
svm.pred <- predict(svm.model, data_2)
s<-as.vector(svm.pred)
tail(cumsum(s),10)


#나이브베이즈
nBayes <-naiveBayes(data_1,(as.factor(response)))
pred_nbayes<-predict(nBayes, data_2)
n<-as.vector(pred_nbayes)
tail(cumsum(n),10)


#로지스틱회귀분석
library(MASS)
glmg<-glm(response ~., family = "binomial", as.data.frame.array(data_1), maxit=100)
as.numeric(glmg$fitted.values)
summary(glmg)
fit<-round(glmg$fitted.values)
table(fit, response)
tabs.glmg<-table(fit, response)
nb<-confusionMatrix(tabs.glmg)
m0.glm<-round(nb$overall,4)
pred <- predict(glmg, newdata=as.data.frame(data_2),type="response")
pred1<-round(pred)
g<-as.vector(pred1)
tail(cumsum(g),10)
