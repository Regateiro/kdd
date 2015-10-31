install.packages("rminer")

library(party)
library(rminer) 
library(adabag)

#setwd('C:\\Users\\BlueMoon\\Documents\\GitHub\\kdd')

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
for(i in 1:length(attributes)){
	d[[attributes[i]]]=remove_outliers(d[[attributes[i]]], 0.05)
}
#remove outliers
#d <- d[complete.cases(d), ]
#replace outliers
for(i in 1:length(attributes)){
	avg = mean(d[[attributes[i]]], na.rm=TRUE)
	for(j in 1:length(d[[attributes[i]]])){
		if(is.na(d[[attributes[i]]][j])){
			d[[attributes[i]]][j]=avg
		}
	}
}


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




