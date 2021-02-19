
m0.dt<-(as.matrix(m0.dt))[1,1]
m1.dt<-(as.matrix(m1.dt))[1,1]
m2.dt<-(as.matrix(m2.dt))[1,1]
m3.dt<-(as.matrix(m3.dt))[1,1]
m4.dt<-(as.matrix(m4.dt))[1,1]
m5.dt<-(as.matrix(m5.dt))[1,1]
m6.dt<-(as.matrix(m6.dt))[1,1]
m7.dt<-(as.matrix(m7.dt))[1,1]
m8.dt<-(as.matrix(m8.dt))[1,1]



m0.rf<-(as.matrix(m0.rf))[1,1]
m1.rf<-(as.matrix(m1.rf))[1,1]
m2.rf<-(as.matrix(m2.rf))[1,1]
m3.rf<-(as.matrix(m3.rf))[1,1]
m4.rf<-(as.matrix(m4.rf))[1,1]
m5.rf<-(as.matrix(m5.rf))[1,1]
m6.rf<-(as.matrix(m6.rf))[1,1]
m7.rf<-(as.matrix(m7.rf))[1,1]
m8.rf<-(as.matrix(m8.rf))[1,1]

m0.svm<-(as.matrix(m0.svm))[1,1]
m1.svm<-(as.matrix(m1.svm))[1,1]
m2.svm<-(as.matrix(m2.svm))[1,1]
m3.svm<-(as.matrix(m3.svm))[1,1]
m4.svm<-(as.matrix(m4.svm))[1,1]
m5.svm<-(as.matrix(m5.svm))[1,1]
m6.svm<-(as.matrix(m6.svm))[1,1]
m7.svm<-(as.matrix(m7.svm))[1,1]
m8.svm<-(as.matrix(m8.svm))[1,1]

m0.nb<-(as.matrix(m0.nb))[1,1]
m1.nb<-(as.matrix(m1.nb))[1,1]
m2.nb<-(as.matrix(m2.nb))[1,1]
m3.nb<-(as.matrix(m3.nb))[1,1]
m4.nb<-(as.matrix(m4.nb))[1,1]
m5.nb<-(as.matrix(m5.nb))[1,1]
m6.nb<-(as.matrix(m6.nb))[1,1]
m7.nb<-(as.matrix(m7.nb))[1,1]
m8.nb<-(as.matrix(m8.nb))[1,1]

m0.glm<-(as.matrix(m0.glm))[1,1]
m1.glm<-(as.matrix(m1.glm))[1,1]
m2.glm<-(as.matrix(m2.glm))[1,1]
m3.glm<-(as.matrix(m3.glm))[1,1]
m4.glm<-(as.matrix(m4.glm))[1,1]
m5.glm<-(as.matrix(m5.glm))[1,1]
m6.glm<-(as.matrix(m6.glm))[1,1]
m7.glm<-(as.matrix(m7.glm))[1,1]
m8.glm<-(as.matrix(m8.glm))[1,1]
no<-as.matrix(-8,-7,-6,-5,-4,-3,-2,-1,0,1) 

accuracy.dt<-as.vector(cbind(m8.dt, m7.dt, m6.dt,m5.dt,m4.dt,m3.dt,m2.dt,m1.dt,m0.dt))
accuracy.rf<-as.vector(cbind(m8.rf,m7.rf,m6.rf,m5.rf,m4.rf,m3.rf,m2.rf,m1.rf,m0.rf))
accuracy.svm<-as.vector(cbind(m8.svm,m7.svm,m6.svm,m5.svm,m4.svm,m3.svm,m2.svm,m1.svm,m0.svm))
accuracy.nb<-as.vector(cbind(m8.nb,m7.nb,m6.nb,m5.nb,m4.nb,m3.nb,m2.nb,m1.nb,m0.nb))
accuracy.glm<-as.vector(cbind(m8.glm,m7.glm,m6.glm,m5.glm,m4.glm,m3.glm,m2.glm,m1.glm,m0.glm))



plot(accuracy.dt, type="o",cex=2, pch=15,ylim=c(0,1),col=1,ylab="",xlab="",xaxt="n")
par(new=T)
plot(accuracy.rf, type="o", pch=8, cex=2, ylim=c(0,1),col=2,ylab="",xlab="",axes=FALSE,xaxt="n")
par(new=T)
plot(accuracy.nb, type="o",cex=2.5, pch=20, col=6, ylim=c(0,1),ylab="",xlab="",axes=FALSE,xaxt="n")
par(new=T)
plot(accuracy.svm, type="o",pch=18,cex=2,col=3, ylim=c(0,1),ylab="",xlab="",axes=FALSE,xaxt="n")
par(new=T)
plot(accuracy.glm, type="o", pch=17 ,cex=1.5, ylim=c(0,1),col=4,
     main="Forecast Lead Time",xlab="Forecast Lead Time(hour)", ylab="Accuracy(%)",axes=FALSE)
par(new=T)
axis(1,at=1:10, lab=c(-8,-7,-6,-5,-4,-3,-2, -1, 0, 1))
par(new=T)
legend(x=0.8, y=0.45,cex=0.8, pt.cex =1.8,
       c("decision tree", "random forest", "naive bayes ", "svm", "logistic regression"),
       col=c(1,2,6,3,4),
       pch=c(15,8,20,18,17))

