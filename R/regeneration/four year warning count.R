fouryear.data<-read.table("fouryear.data.csv",sep=",",head=T)[,3:20]
fouryear.data1<-read.table("fouryear.data.csv",sep=",",head=T)

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
data_2<-cbind(as.matrix(fouryear.data)%*%(pca.mat))

########################################################################
library(randomForest)
library(caret)
library(rpart)
library(e1071)
library(MASS)  

#랜덤포레스트
rf <- randomForest(as.factor(response) ~ .,data_1, var.importance=TRUE)
rf.pred <- as.matrix(predict(rf, data_2))
dim(data_2)
dim(rf.pred)
tail(cumsum(rf.pred),10)


#비온시간 체크
rf.result<-(cbind(fouryear.data[,1:6],rf.pred))
new.rf <-as.vector(rf.result[,1] + rf.result[,2]+rf.result[,3]+rf.result[,4]+rf.result[,5]+rf.result[,6])
cc<-cbind(fouryear.data1[2],fouryear.data,rf.result,new.rf)

#4년 중 비가 온날 273일
aa<-subset(cc, cc$new.rf>0, select=c(date1,new.rf))
bb<-as.matrix(substr((aa[,1]),1,9))
cc<-substr(bb[,1],1,9)
cc<-unique(cc)

#rf 결과
aa<-cbind(date.dataset[,1],rf.pred)
bb<-subset(aa,aa[,2]==1)
cc<-as.matrix( substr(bb[,1],1,9))
dd<-unique(cc)#78



#의사결정나무
tree<- rpart((response) ~ ., data=as.data.frame(data_1), method="class")
tree.pred <- predict(tree, newdata =as.data.frame(data_2), type = c( "class"))
tail(cumsum(as.matrix(tree.pred)),10)

aa<-cbind(as.matrix(date.dataset[,1]), as.matrix(tree.pred))
bb<-subset(aa,aa[,2]==1)
cc<-as.matrix( substr(bb[,1],1,9))
dd<-unique(cc)#156





#서포트벡터머신
library(e1071)
svm.model<-svm(as.factor(response) ~ ., data=data_1)
svm.pred <- predict(svm.model, data_2)
dim(as.vector(svm.pred))
tail(cumsum(as.matrix(svm.pred)),10)
aa<-cbind(as.matrix(date.dataset[,1]), as.matrix(svm.pred))
bb<-subset(aa,aa[,2]==1)
cc<-as.matrix( substr(bb[,1],1,9))
dd<-unique(cc)#80

#나이브베이즈
nBayes <-naiveBayes(data_1,(as.factor(response)))
nb.pred<-predict(nBayes, data_2)
tail(cumsum(as.matrix(nb.pred)),10)

aa<-cbind(as.matrix(date.dataset[,1]), as.matrix(nb.pred))
bb<-subset(aa,aa[,2]==1)
cc<-as.matrix( substr(bb[,1],1,9))
dd<-unique(cc)#92


#로지스틱회귀분석
library(MASS)
glmg<-glm(response ~., family = "binomial", as.data.frame.array(data_1), maxit=100)
glm.pred<-predict(glmg, newdata = as.data.frame(data_2), type = c( "response"))
fit<-round(glm.pred)
tail(cumsum(as.matrix(fit)),10)
aa<-cbind(as.matrix(date.dataset[,1]),(fit))
bb<-subset(aa,aa[,2]==1)
cc<-as.matrix( substr(bb[,1],1,9))
dd<-unique(cc)#54

