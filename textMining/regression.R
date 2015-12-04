library(rminer) 

#setwd('C:\\Users\\BlueMoon\\Documents\\GitHub\\kdd\\textMining')
load("featuresP.data", verbose = TRUE)

testIndexes = sample(length(features[[1]]), 1000)


#knnParams=mining(zz_rates~.,features[-testIndexes,],model="ksvm",Runs=5,method=c("kfold",3),
#	search="heuristic5",f="s")
#RF=fit(zz_rates~.,features[-testIndexes,],model="ksvm",search=knnParams$mpar)

RF=fit(zz_rates~.,features[-testIndexes,],model="ksvm") 
PRFu1=predict(RF,features[testIndexes,])