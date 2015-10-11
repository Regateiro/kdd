# Install packages
install.packages("adabag")
install.packages("randomForest")

# Load libraries
library(adabag)
library(randomForest)

# Load CSV
data.train <- read.csv('training.csv', TRUE, ';')
data.test <- read.csv('test.csv', TRUE, ';')
data.train$STATUS <- as.factor(data.train$STATUS)

# predict expects the label to be present to build the confusion matrix... I just used ones
data.test$STATUS <- as.factor(c(rep(1, 25),rep(-1,25)))

# Decision Tree with boosting
model.boost_dt <- boosting(STATUS ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + 
                                 X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + 
                                 X20 + X21 + X22 + X23 + X24 + X25 + X26 + X27 + X28 + 
                                 X29 + X30, data = data.train, coeflearn="Zhu")

prediction.boost_dt <- predict(model.boost_dt, newdata=data.test, type="class")

# SVM
model.svm <- svm(STATUS ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + 
                                 X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + 
                                 X20 + X21 + X22 + X23 + X24 + X25 + X26 + X27 + X28 + 
                                 X29 + X30, data = data.train, probability=TRUE, kernel="radial")

prediction.svm <- predict(model.svm, newdata=data.test, probability=TRUE)

# Ensemble
prediction.boostforest_ens_dt.prob <- prediction.boost_dt$prob + attr(prediction.svm,"probabilities")

prediction.boostforest_ens_dt <- vector(,length(prediction.boostforest_ens_dt.prob[,1]))
for(i in 1:length(prediction.boostforest_ens_dt.prob[,1])) {
    prediction.boostforest_ens_dt[i] <- ifelse(prediction.boostforest_ens_dt.prob[i,1] < prediction.boostforest_ens_dt.prob[i,2],1,-1)
}

# Output
out <- "boostforest_ens_dt.out"
if (file.exists(out)) file.remove(out)
write("Id,STATUS", file = out, append=TRUE)
for(i in 1:length(prediction.boostforest_ens_dt)) {
    write(paste(i, prediction.boostforest_ens_dt[i], sep = ","), file = out, append=TRUE)
}