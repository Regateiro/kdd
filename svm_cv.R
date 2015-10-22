# Install packages
install.packages("e1071")

# Load libraries
library(e1071)

# Load CSV
data.train <- read.csv('training.csv', TRUE, ';')
data.test <- read.csv('test.csv', TRUE, ';')

data.train$ID <- NULL
data.train$STATUS <- as.factor(data.train$STATUS)

# SVM
model.svm <- svm(STATUS ~ X24 + X18 + X23 + X5 + X9, data = data.train, probability=TRUE, kernel="radial", cross=10)
prediction.svm <- predict(model.svm, probability=TRUE)
confusion <- table(prediction.svm, data.train$STATUS)
result <- sum(diag(confusion))/sum(confusion)
print(result)