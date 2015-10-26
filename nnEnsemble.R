install.packages("rminer")

library(party)
library(rminer) 
library(adabag)

setwd('C:\\Users\\BlueMoon\\Documents\\GitHub\\kdd')

d=read.csv('training.csv', TRUE, ';')
test=read.csv('test.csv', TRUE, ';')
test$STATUS <- as.factor(c(rep(0, 50)))
d=d[,c(2:32)]
#print(summary(d))

# MLP
NNmine=mining(STATUS~.,d,model="mlpe",Runs=5,method=c("kfold",3),search="heuristic5",feat="s")
NN=fit(STATUS~.,d,model="mlpe",search=NNmine$mpar)
PNN=predict(NN,test)

# SVM
SVmine=mining(STATUS~.,d,model="ksvm",Runs=5,method=c("kfold",3),search="heuristic5",f="s")
SV=fit(STATUS~.,d,model="ksvm",search=SVmine$mpar) # fit the SVM 
PSVM=predict(SV,test)

# CIF
CIF <- cforest(STATUS~., data = d, controls=cforest_unbiased(ntree=100, mtry=3))
PCIF<- predict(CIF, test, OOB=TRUE, type = "response")

# RF
d$STATUS <- as.factor(d$STATUS)
RF=fit(STATUS~.,d,model="randomforest") # fit a random forest
PRFu=predict(RF,test)
PRF=c(1:50)
for(i in 1:length(PRFu[,1])) {
    PRF[i] <- ifelse(PRFu[i,1] < PRFu[i,2],PRFu[i,2]-PRFu[i,1],-(PRFu[i,1]-PRFu[i,2]))
}

# Boosting
data.train <- read.csv('training.csv', TRUE, ';')
data.train=data.train[,c(2:32)]
data.test <- read.csv('test.csv', TRUE, ';')
data.train$STATUS <- as.factor(data.train$STATUS)
data.test$STATUS <- as.factor(c(rep(1, 25),rep(-1,25)))
BO <- boosting(STATUS ~ ., data = data.train, coeflearn="Zhu")
PBOu <- (predict(BO, newdata=data.test, type="class"))$prob
PBO=c(1:50)
for(i in 1:length(PBOu[,1])) {
    	PBO[i] <- ifelse(PBOu[i,1] < PBOu[i,2],PBOu[i,2]-PBOu[i,1],-(PBOu[i,1]-PBOu[i,2]))
}

# kNN
KNNmine=mining(STATUS~.,d,model="kknn",Runs=5,method=c("kfold",3),search="heuristic5",f="s")
KNN=fit(STATUS~.,d,model="kknn",search=SVmine$mpar) 
PKNN=predict(KNN,test)



# get the predictions:
results=c(1:50)
for(i in 1:length(test$STATUS)){
	PRF[i] <- ifelse(PRF[i] > 0,PRF[i]^2,-(PRF[i]^2))
	PNN[i] <- ifelse(PNN[i] > 0,PNN[i]^2,-(PNN[i]^2))
	PSVM[i] <- ifelse(PSVM[i] > 0,PSVM[i]^2,-(PSVM[i]^2))
	PBO[i] <- ifelse(PBO[i] > 0,PBO[i]^2,-(PBO[i]^2))
	PKNN[i] <- ifelse(PKNN[i] > 0,PKNN[i]^2,-(PKNN[i]^2))
	PCIF[i] <- ifelse(PCIF[i] > 0,PCIF[i]^2,-(PCIF[i]^2))
	results[i]=PRF[i]+PNN[i]+PSVM[i]+PBO[i]+PKNN[i]+PCIF[i]
}

P=data.frame(ID=test$ID,STATUS=results)
for(i in 1:length(P$STATUS)) {
    P$STATUS[i] <- ifelse(P$STATUS[i] > 0,1,-1)
}
write.csv(P,"results.csv", row.names=FALSE) # save output and predictions

#mpause("Show average MAE metric:")
#eNN=mmetric(NN,metric="MAE")
#mi=meanint(eNN)
#cat("MAE average=:",mi$mean,"+-",mi$int,"\n")

#mpause("Show scatter plot:")
#mgraph(NN,graph="RSC",main="MLP",baseline=TRUE,Grid=TRUE)

#mpause("Show importance graph:")
#nw=c(1:31)
#mgraph(NNmine,graph="IMP",leg=nw,xval=0.0,Grid=TRUE)




