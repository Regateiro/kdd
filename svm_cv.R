# Install packages
install.packages("e1071")

# Load libraries
library(e1071)

# Load CSV
data.train <- read.csv('training.csv', TRUE, ';')
data.test <- read.csv('test.csv', TRUE, ';')

data.train$ID <- NULL
data.train$STATUS <- as.factor(data.train$STATUS)
data.train$X31 <- data.train$X1 * data.train$X22
data.train$X32 <- data.train$X1 * data.train$X28
data.train$X33 <- data.train$X3 * data.train$X10
data.train$X34 <- data.train$X3 * data.train$X18
data.train$X35 <- data.train$X3 * data.train$X22
data.train$X36 <- data.train$X3 * data.train$X28
data.train$X37 <- data.train$X6 * data.train$X7
data.train$X38 <- data.train$X6 * data.train$X8
data.train$X39 <- data.train$X6 * data.train$X18
data.train$X40 <- data.train$X7 * data.train$X12
data.train$X41 <- data.train$X7 * data.train$X19
data.train$X42 <- data.train$X7 * data.train$X24
data.train$X43 <- data.train$X7 * data.train$X30
data.train$X44 <- data.train$X8 * data.train$X12
data.train$X45 <- data.train$X8 * data.train$X19
data.train$X46 <- data.train$X8 * data.train$X23
data.train$X47 <- data.train$X8 * data.train$X24
data.train$X48 <- data.train$X8 * data.train$X30
data.train$X49 <- data.train$X12 * data.train$X17
data.train$X50 <- data.train$X12 * data.train$X18
data.train$X51 <- data.train$X13 * data.train$X21
data.train$X52 <- data.train$X17 * data.train$X19
data.train$X53 <- data.train$X18 * data.train$X19
data.train$X54 <- data.train$X18 * data.train$X30
data.train$X55 <- data.train$X21 * data.train$X23
data.train$X56 <- data.train$X23 * data.train$X25
data.train$X57 <- data.train$X23 * data.train$X26

set.seed(1234)

# SVM
for(i in 1:57) {
  model.svm <- svm(as.formula(paste("STATUS ~ X24 + X18 + X52 + X15 + X23 + X",i,sep="")), data = data.train, probability=TRUE, cross=10)
  prediction.svm <- predict(model.svm, probability=TRUE)
  confusion <- table(prediction.svm, data.train$STATUS)
  result <- sum(diag(confusion))/sum(confusion)
  print(paste("Feature:",i,"-->",result))
}