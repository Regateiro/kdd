#install.packages("rminer")

library(party)
library(rminer) 
library(adabag)

setwd('C:\\Users\\BlueMoon\\Documents\\GitHub\\kdd')

d=read.csv('training.csv', TRUE, ';')
test=read.csv('test.csv', TRUE, ';')

# Add new features
d$X31 <- d$X1 * d$X22
d$X32 <- d$X1 * d$X28
d$X33 <- d$X3 * d$X10
d$X34 <- d$X3 * d$X18
d$X35 <- d$X3 * d$X22
d$X36 <- d$X3 * d$X28
d$X37 <- d$X6 * d$X7
d$X38 <- d$X6 * d$X8
d$X39 <- d$X6 * d$X18
d$X40 <- d$X7 * d$X12
d$X41 <- d$X7 * d$X19
d$X42 <- d$X7 * d$X24
d$X43 <- d$X7 * d$X30
d$X44 <- d$X8 * d$X12
d$X45 <- d$X8 * d$X19
d$X46 <- d$X8 * d$X23
d$X47 <- d$X8 * d$X24
d$X48 <- d$X8 * d$X30
d$X49 <- d$X12 * d$X17
d$X50 <- d$X12 * d$X18
d$X51 <- d$X13 * d$X21
d$X52 <- d$X17 * d$X19
d$X53 <- d$X18 * d$X19
d$X54 <- d$X18 * d$X30
d$X55 <- d$X21 * d$X23
d$X56 <- d$X23 * d$X25
d$X57 <- d$X23 * d$X26

test$X31 <- test$X1 * test$X22
test$X32 <- test$X1 * test$X28
test$X33 <- test$X3 * test$X10
test$X34 <- test$X3 * test$X18
test$X35 <- test$X3 * test$X22
test$X36 <- test$X3 * test$X28
test$X37 <- test$X6 * test$X7
test$X38 <- test$X6 * test$X8
test$X39 <- test$X6 * test$X18
test$X40 <- test$X7 * test$X12
test$X41 <- test$X7 * test$X19
test$X42 <- test$X7 * test$X24
test$X43 <- test$X7 * test$X30
test$X44 <- test$X8 * test$X12
test$X45 <- test$X8 * test$X19
test$X46 <- test$X8 * test$X23
test$X47 <- test$X8 * test$X24
test$X48 <- test$X8 * test$X30
test$X49 <- test$X12 * test$X17
test$X50 <- test$X12 * test$X18
test$X51 <- test$X13 * test$X21
test$X52 <- test$X17 * test$X19
test$X53 <- test$X18 * test$X19
test$X54 <- test$X18 * test$X30
test$X55 <- test$X21 * test$X23
test$X56 <- test$X23 * test$X25
test$X57 <- test$X23 * test$X26

test$STATUS <- as.factor(c(rep(0, 50)))
d=d[,c(2:length(d))]
#print(summary(d))

remove_outliers <- function(x, val, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(val, 1-val), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

attributes=colnames(d)[c(2:length(d))]
d.o <- d
for(i in 1:length(attributes)){
	d.o[[attributes[i]]]=remove_outliers(d.o[[attributes[i]]], 0.05)
}
#remove outliers
d.n <- d[complete.cases(d.o), ]
d.r <- d[!complete.cases(d.o), ]

test.o <- test
for(i in 1:length(attributes)){
	test.o[[attributes[i]]]=remove_outliers(test.o[[attributes[i]]], 0.05)
}
#remove outliers
test.n <- test[complete.cases(test.o), ]
test.r <- test[!complete.cases(test.o), ]



# MLP
NNmine.n=mining(STATUS~.,d.n,model="mlpe",Runs=5,method=c("kfold",3),search="heuristic5",feat="s")
NN.n=fit(STATUS~.,d.n,model="mlpe",search=NNmine.n$mpar)
PNN.n=predict(NN.n,test.n)

NNmine.r=mining(STATUS~.,d.r,model="mlpe",Runs=5,method=c("kfold",3),search="heuristic5",feat="s")
NN.r=fit(STATUS~.,d.r,model="mlpe",search=NNmine.r$mpar)
PNN.r=predict(NN.r,test.r)

# SVM
SVmine.n=mining(STATUS~.,d.n,model="ksvm",Runs=5,method=c("kfold",3),search="heuristic5",f="s")
SV.n=fit(STATUS~.,d.n,model="ksvm",search=SVmine.n$mpar) # fit the SVM 
PSVM.n=predict(SV.n,test.n)

SVmine.r=mining(STATUS~.,d.r,model="ksvm",Runs=5,method=c("kfold",3),search="heuristic5",f="s")
SV.r=fit(STATUS~.,d.r,model="ksvm",search=SVmine.r$mpar) # fit the SVM 
PSVM.r=predict(SV.r,test.r)

# CIF
CIF.n <- cforest(STATUS~., data = d.n, controls=cforest_unbiased(ntree=100, mtry=3))
PCIF.n <- predict(CIF.n, test.n, OOB=TRUE, type = "response")

CIF.r <- cforest(STATUS~., data = d.r, controls=cforest_unbiased(ntree=100, mtry=3))
PCIF.r <- predict(CIF.r, test.r, OOB=TRUE, type = "response")

# RF
d.n$STATUS <- as.factor(d.n$STATUS)
RF.n=fit(STATUS~.,d.n,model="randomforest") # fit a random forest
PRFu.n=predict(RF.n,test.n)
PRF.n=c(1:50)
for(i in 1:length(PRFu.n[,1])) {
    PRF.n[i] <- ifelse(PRFu.n[i,1] < PRFu.n[i,2],PRFu.n[i,2]-PRFu.n[i,1],-(PRFu.n[i,1]-PRFu.n[i,2]))
}

d.r$STATUS <- as.factor(d.r$STATUS)
RF.r=fit(STATUS~.,d.r,model="randomforest") # fit a random forest
PRFu.r=predict(RF.r,test.r)
PRF.r=c(1:50)
for(i in 1:length(PRFu.r[,1])) {
    PRF.r[i] <- ifelse(PRFu.r[i,1] < PRFu.r[i,2],PRFu.r[i,2]-PRFu.r[i,1],-(PRFu.r[i,1]-PRFu.r[i,2]))
}

# kNN
KNNmine.n=mining(STATUS~.,d.n,model="kknn",Runs=5,method=c("kfold",3),search="heuristic5",f="s")
KNN.n=fit(STATUS~.,d.n,model="kknn",search=KNNmine.n$mpar) 
PKNN.n=predict(KNN.n,test.n)

KNNmine.r=mining(STATUS~.,d.r,model="kknn",Runs=5,method=c("kfold",3),search="heuristic5",f="s")
KNN.r=fit(STATUS~.,d.r,model="kknn",search=KNNmine.r$mpar) 
PKNN.r=predict(KNN.r,test.r)

# Boosting
#d$STATUS <- as.factor(d$STATUS)

test.n$STATUS <- as.factor(c(rep(1, length(test.n[[1]])/2),rep(-1,length(test.n[[1]])/2)))
BO.n <- boosting(STATUS ~ ., data = d.n, coeflearn="Zhu")
PBOu.n <- (predict(BO.n, newdata=test.n, type="class"))$prob
PBO.n=c(1:length(PBOu.n[,1]))
for(i in 1:length(PBOu.n[,1])) {
    	PBO.n[i] <- ifelse(PBOu.n[i,1] < PBOu.n[i,2],PBOu.n[i,2]-PBOu.n[i,1],-(PBOu.n[i,1]-PBOu.n[i,2]))
}

test.r$STATUS <- as.factor(c(rep(1, length(test.r[[1]])/2),rep(-1,length(test.r[[1]])/2)))
BO.r <- boosting(STATUS ~ ., data = d.r, coeflearn="Zhu")
PBOu.r <- (predict(BO.r, newdata=test.r, type="class"))$prob
PBO.r=c(1:length(PBOu.r[,1]))
for(i in 1:length(PBOu.r[,1])) {
    	PBO.r[i] <- ifelse(PBOu.r[i,1] < PBOu.r[i,2],PBOu.r[i,2]-PBOu.r[i,1],-(PBOu.r[i,1]-PBOu.r[i,2]))
}

PRF=c(1:50)
PNN=c(1:50)
PSVM=c(1:50)
PBO=c(1:50)
PKNN=c(1:50)
PCIF=c(1:50)
counter = 1
i=1
j=1
while(counter<=50){
	if((i<=length(test.n$ID) || j>length(test.r$ID)) && test.n$ID[i] < test.r$ID[j]){
		PRF[counter]=PRF.n[i]
		PNN[counter]=PNN.n[i]
		PSVM[counter]=PSVM.n[i]
		PBO[counter]=PBO.n[i]
		PKNN[counter]=PKNN.n[i]
		PCIF[counter]=PCIF.n[i]
		i=i+1
	}else{
		PRF[counter]=PRF.r[j]
		PNN[counter]=PNN.r[j]
		PSVM[counter]=PSVM.r[j]
		PBO[counter]=PBO.r[j]
		PKNN[counter]=PKNN.r[j]
		PCIF[counter]=PCIF.r[j]
		j=j+1
	}
	counter=counter+1
}


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
#REMOVE OUTLIERS

#mpause("Show average MAE metric:")
#eNN=mmetric(NN,metric="MAE")
#mi=meanint(eNN)
#cat("MAE average=:",mi$mean,"+-",mi$int,"\n")

#mpause("Show scatter plot:")
#mgraph(NN,graph="RSC",main="MLP",baseline=TRUE,Grid=TRUE)

#mpause("Show importance graph:")
#nw=c(1:31)
#mgraph(NNmine,graph="IMP",leg=nw,xval=0.0,Grid=TRUE)




