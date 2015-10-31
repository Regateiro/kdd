install.packages("rminer")
library(rminer) # load the rminer library
library(adabag)
setwd('C:\\Users\\BlueMoon\\Documents\\GitHub\\kdd')

set.seed(1234)
d=read.csv('training.csv', TRUE, ';')
d=d[,c(2:32)]

cols=colnames(d)[c(2:31)]
SVmine=mining(STATUS~X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 +
X13 + X14 + X15 + X16 + X17 + X18 + X19 + X20 + X21 + X22 + X23 + X24 +
X26 + X27 + X28 + X29 + X30,d,model="mlpe",
	Runs=5,method=c("kfold",3),search="heuristic5",f="s")
SV=fit(STATUS~X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 +
X13 + X14 + X15 + X16 + X17 + X18 + X19 + X20 + X21 + X22 + X23 + X24 +
X26 + X27 + X28 + X29 + X30,d,model="mlpe",search=SVmine$mpar) 
PSVM=predict(SV,d)

# get the predictions:
results=c(1:118)
for(i in 1:length(d$STATUS)){
	results[i]=PSVM[i]
}

for(i in 1:length(results)) {
    results[i] <- ifelse(results[i] > 0,1,-1)
}

counter = 0
for(i in 1:length(results)) {
    counter = ifelse(results[i] == d$STATUS[i],counter+1,counter)
}
maxAcc=counter
newCols=cols
print("Max Acc:")
print(counter/118*100)

while(TRUE){
removeCol=0

for(index in 1:length(newCols)){
	SVmine=mining(as.formula(paste("STATUS~",paste(newCols[-index],collapse="+"))),d,model="ksvm",
		Runs=5,method=c("kfold",3),search="heuristic5",f="s")
	SV=fit(as.formula(paste("STATUS~",paste(newCols[-index],collapse="+"))),d,model="ksvm",search=SVmine$mpar) 
	results=predict(SV,d)

	for(i in 1:length(results)) {
    		results[i] <- ifelse(results[i] > 0,1,-1)
	}

	counter = 0
	for(i in 1:length(results)) {
    		counter = ifelse(results[i] == d$STATUS[i],counter+1,counter)
	}

	if(counter>=maxAcc){
		print("removing column X with acc Y")
		print(index)
		print(counter/118*100)
		removeCol=index
		maxAcc=counter
	}else{
		print("NOT removing column X with acc Y")
		print(index)
		print(counter/118*100)
	}
	
}
if(removeCol!=0){
	print("Removing ")
	print(removeCol)
	newCols=newCols[-removeCol]
}else{
	print("Not removing anything")
	print(newCols)
	break
}
}



featSelect <- function(cols, maxAcc) {
	for(index in 1:length(cols)){
	SV=fit(as.formula(paste("STATUS~",paste(cols[-index],collapse="+"))),
		d,model="mlpe",search=SVmine$mpar) 
	results=predict(SV,d)

	for(i in 1:length(results)) {
    		results[i] <- ifelse(results[i] > 0,1,-1)
	}

	counter = 0
	for(i in 1:length(results)) {
    		counter = ifelse(results[i] == d$STATUS[i],counter+1,counter)
	}

	if(counter>maxAcc){
		print("removing column X with acc Y")
		print(index)
		featSelect(cols[-index], counter)
	}
	
	}

  	print("Best result: ")
	print(maxAcc/118*100)
	print(cols)
}

SVmine=mining(STATUS~.,d,model="mlpe",
	Runs=5,method=c("kfold",3),search="heuristic5",f="s")
featSelect(cols, maxAcc)

