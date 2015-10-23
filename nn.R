install.packages("rminer")
library(rminer) # load the rminer library

#setwd('C:\\Users\\BlueMoon\\Documents\\GitHub\\kdd')

d=read.csv('training.csv', TRUE, ';')
test=read.csv('test.csv', TRUE, ';')
test$STATUS <- as.factor(c(rep(0, 50)))

#mpause("Select some attributes:")
AT=c(2:32)
d=d[,AT]
#print(summary(d))

#mpause("Model a MLP using 50 runs of a 3-fold cross-validation:")
NNmine=mining(STATUS~.,d,model="mlpe",Runs=50,method=c("kfold",3),search="heuristic5",f="s")

#mpause("Show MLP best parameters and time elapsed:")
#print(centralpar(NNmine$mpar))
#cat("total time elapsed:",sum(NN$time),"s\n")

# fit a NN with H=10
NN=fit(STATUS~.,d,model="mlpe",search=NNmine$mpar)

PNN=predict(NN,test);
P=data.frame(ID=test$ID,STATUS=PNN)
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
#mgraph(NN,graph="IMP",leg=nw,xval=0.0,Grid=TRUE)




