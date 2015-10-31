# Install packages
install.packages("adabag")
install.packages("e1071")

# Load libraries
library(adabag)
library(e1071)

# Load CSV
data.train <- read.csv('training.csv', TRUE, ';')
data.test <- read.csv('test.csv', TRUE, ';')
data.train$STATUS <- as.factor(data.train$STATUS)

# predict expects the label to be present to build the confusion matrix... I just used ones
data.test$STATUS <- as.factor(c(rep(1, 25),rep(-1,25)))
data.train$X33 <- data.train$X3 * data.train$X10
data.train$X47 <- data.train$X8 * data.train$X24
data.train$X48 <- data.train$X8 * data.train$X30
data.train$X49 <- data.train$X12 * data.train$X17
data.train$X52 <- data.train$X17 * data.train$X19
data.test$X33 <- data.test$X3 * data.test$X10
data.test$X47 <- data.test$X8 * data.test$X24
data.test$X48 <- data.test$X8 * data.test$X30
data.test$X49 <- data.test$X12 * data.test$X17
data.test$X52 <- data.test$X17 * data.test$X19

# Decision Tree with boosting
# model.boost_dt <- boosting(STATUS ~ X10 + X28 + X11 + X26, data = data.train)
model.boost_dt <- boosting(STATUS ~ X1 + X2 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + X20 + X21 + X22 + X23 + X24 + X25 + X26 + X27 + X28 + X29 + X30, data = data.train)

prediction.boost_dt <- predict(model.boost_dt, newdata=data.test, type="class")

# SVM
# model.svm <- svm(STATUS ~ X24 + X18 + X23 + X5 + X9, data = data.train, probability=TRUE, kernel="radial")
model.svm <- svm(STATUS ~ X24 + X18 + X52 + X15 + X23, data = data.train, probability=TRUE)
#24 9 23    1 11 (RBF)

prediction.svm <- predict(model.svm, newdata=data.test, probability=TRUE)

# Ensemble
prediction.boostsvm_ens_dt.prob <- prediction.boost_dt$prob + attr(prediction.svm,"probabilities")

prediction.boostsvm_ens_dt <- vector(,length(prediction.boostsvm_ens_dt.prob[,1]))
for(i in 1:length(prediction.boostsvm_ens_dt.prob[,1])) {
    prediction.boostsvm_ens_dt[i] <- ifelse(prediction.boostsvm_ens_dt.prob[i,1] < prediction.boostsvm_ens_dt.prob[i,2],1,-1)
}

# Output
out <- "boostsvm_ens_dt.out"
if (file.exists(out)) file.remove(out)
write("Id,STATUS", file = out, append=TRUE)
for(i in 1:length(prediction.boostsvm_ens_dt)) {
    write(paste(i, prediction.boostsvm_ens_dt[i], sep = ","), file = out, append=TRUE)
}
