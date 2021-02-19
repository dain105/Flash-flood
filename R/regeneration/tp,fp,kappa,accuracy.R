rf.TP<-rf$confusion[1,1]
rf.FP<-rf$confusion[2,1]
rf.FN<-rf$confusion[1,2]
rf.TN<-rf$confusion[2,2]

tree.TP<-Tabs.tree[1,1]
tree.FP<-Tabs.tree[2,1]
tree.FN<-Tabs.tree[1,2]
tree.TN<-Tabs.tree[2,2]

svm.TP<-Tabs.svm[1,1]
svm.FP<-Tabs.svm[2,1]
svm.FN<-Tabs.svm[1,2]
svm.TN<-Tabs.svm[2,2]

nb.TP<-Tabs.nb[1,1]
nb.FP<-Tabs.nb[2,1]
nb.FN<-Tabs.nb[1,2]
nb.TN<-Tabs.nb[2,2]

glm.TP<-Tabs.glmg[1,1]
glm.FP<-Tabs.glmg[2,1]
glm.FN<-Tabs.glmg[1,2]
glm.TN<-Tabs.glmg[2,2]



ma.dt<-(as.matrix(m0.dt))[1,1]
mk.dt<-(as.matrix(m0.dt))[2,1]
tree.TPR<-tree.TP/tree.TP+tree.FN
tree.FPR<-tree.FP/tree.FP+tree.TN
tree.PRE<-tree.TP/tree.FP+tree.TN
tree.F_m<-2*(tree.PRE*tree.TPR)/(tree.PRE+tree.TPR)



ma.rf<-(as.matrix(m0.rf))[1,1]
mk.rf<-(as.matrix(m0.rf))[2,1]
rf.TPR<-rf.TP/rf.TP+rf.FN
rf.FPR<-rf.FP/rf.FP+rf.TN
rf.PRE<-rf.TP/rf.FP+rf.TN
rf.F_m<-2*(rf.PRE*rf.TPR)/(rf.PRE+rf.TPR)



ma.svm<-(as.matrix(m0.svm))[1,1]
mk.svm<-(as.matrix(m0.svm))[2,1]
svm.TPR<-svm.TP/svm.TP+svm.FN
svm.FPR<-svm.FP/svm.FP+svm.TN
svm.PRE<-svm.TP/svm.FP+svm.TN
svm.F_m<-2*(svm.PRE*svm.TPR)/(svm.PRE+svm.TPR)

ma.nb<-(as.matrix(m0.nb))[1,1]
mk.nb<-(as.matrix(m0.nb))[2,1]
nb.TPR<-nb.TP/nb.TP+nb.FN
nb.FPR<-nb.FP/nb.FP+nb.TN
nb.PRE<-nb.TP/nb.FP+nb.TN
nb.F_m<-2*(nb.PRE*nb.TPR)/(nb.PRE+nb.TPR)




ma.glm<-(as.matrix(m0.glm))[1,1]
mk.glm<-(as.matrix(m0.glm))[2,1]
glm.TPR<-glm.TP/glm.TP+glm.FN
glm.FPR<-glm.FP/glm.FP+glm.TN
glm.PRE<-glm.TP/glm.FP+glm.TN
glm.F_m<-2*(glm.PRE*glm.TPR)/(glm.PRE+glm.TPR)

mm<-c(ma.dt,ma.rf, ma.nb,ma.svm, ma.glm,
      mk.dt,mk.rf, mk.nb, mk.svm,mk.glm,
      tree.TPR,rf.TPR,nb.TPR, svm.TPR,glm.TPR,
      tree.FPR, rf.FPR, nb.FPR,svm.FPR, glm.FPR,
      tree.F_m,rf.F_m,nb.F_m,svm.F_m,glm.F_m)


m<-matrix(mm,nrow=5,ncol=5)
m[is.na(as.data.frame(m))]<-0
m
