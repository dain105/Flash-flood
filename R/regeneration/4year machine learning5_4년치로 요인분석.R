setwd("S:/Users/Dain/연구/돌발홍수/data/regeneration")
response<-c(1,1,1,0,0,1,0,1,0,1,0,0,0,1,1,1,0,1,1,1,1,1,0,1,1,1,1,1,1,0,0,0,0,0,0,1,1,1)


#######################4년치데이터################
fouryear.data<-read.table("fouryear.data.csv",sep=",",head=T)
data2_1<-fouryear.data[,3:20] 
colnames(data2_1) <- c("pcp1","pcp2","pcp3","pcp4","pcp5","pcp6","run1","run2","run3","run4","run5","run6","sws1","sws2","sws3","sws4","sws5","sws6")

#4년치데이터로 요인분석
#요인분석

library(psych)
p_cor<-princomp(data2_1,cor=T)
summary(p_cor)
screeplot(p_cor,type="lines")

pca.ma<-principal(data2_1[,1:18], rotate="varimax", nfactors=3)
pca.mat<-pca.ma$weights

print(pca.ma, digits =3)


#####################4년치요인분석 가중치 적용###########################
data_1<-cbind((machine.data[,1:18])%*%(pca.mat),response)
data_2<-pca.ma$scores
########################################################################
library(randomForest)
library(caret)
library(rpart)
library(e1071)
library(MASS)

#랜덤포레스트
str(rf)
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
Tabs.nb<-table(pred_nbayes, response)
nb<-confusionMatrix(Tabs.nb)
m0.nb<-round(nb$overall,4)

#로지스틱회귀분석
library(MASS)

glmg<-glm(response ~., family = "binomial", as.data.frame.array(data_1), maxit=100)
as.numeric(glmg$fitted.values)
summary(glmg)
fit<-round(glmg$fitted.values)
table(fit, response)
Tabs.glmg<-table(fit, response)
glm<-confusionMatrix(Tabs.glmg)
m0.glm<-round(glm$overall,4)

