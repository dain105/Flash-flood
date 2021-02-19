
#토양수분상태는 지역별로 다르기때문에 38개 지역의 최대토양상태를 계산해서 나눠준다.
sub.A.data<-which(sub.dataset[,1] %in% c("A"))
num.A=length(sub.A.data)
sws.dataset3<-sws.dataset2<-sws.dataset
for (i in 1:num.A){
  for (j in 1:duration){
    sws.dataset2[i,j]<-max(sws.dataset)-sws.dataset2[i,j]
    sws.dataset3[i,j]<-round((max(sws.dataset)-sws.dataset[i,j])/(max(sws.dataset)-min(sws.dataset)),3)
  }
}

response<-c(1,1,1,0,0,1,0,1,0,1,0,0,0,1,1,1,0,1,1,1,1,1,0,1,1,1,1,1,1,0,0,0,0,0,0,1,1,1)
machine.data<-cbind(pcp.dataset, run.dataset, sws.dataset3,response)
colnames(machine.data) <- c("pcp1","pcp2","pcp3","pcp4","pcp5","pcp6","run1","run2","run3","run4","run5","run6","sws1","sws2","sws3","sws4","sws5","sws6","response")
library(randomForest)
library(caret)
library(rpart)
class(response)



#요인분석
library(psych)
p_cor<-princomp(machine.data,cor=TRUE)
summary(p_cor)
screeplot(p_cor,type="lines")

pca.ma<-principal(machine.data[,1:18], rotate="varimax", nfactors=3)
pca.ma$eigenvalue
pca.ma$scores<-pca.ma$scores[complete.cases(pca.ma$scores),]
data_1<-pca.ma$scores

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

