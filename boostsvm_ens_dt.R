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

# Decision Tree with boosting
model.boost_dt <- boosting(STATUS ~ X10 + X28 + X11 + X26, data = data.train)

prediction.boost_dt <- predict(model.boost_dt, newdata=data.test, type="class")

# SVM
model.svm <- svm(STATUS ~ X24 + X18 + X23 + X5 + X9, data = data.train, probability=TRUE, kernel="radial")
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
