# Install packages
install.packages("e1071")

# Load libraries
library(e1071)

# Load CSV
data.train <- read.csv('training.csv', TRUE, ';')
data.test <- read.csv('test.csv', TRUE, ';')

data.train$STATUS <- as.factor(data.train$STATUS)
# predict expects the label to be present to build the confusion matrix... I just used ones
data.test$STATUS <- as.factor(c(rep(1, 25),rep(-1,25)))

# Decision Tree with boosting
model.svm <- svm(STATUS ~ X24 + X18 + X23 + X5 + X9, data = data.train, probability=TRUE, kernel="radial")

prediction.svm <- predict(model.svm, newdata=data.test, probability=TRUE)

out <- "svm.out"
if (file.exists(out)) file.remove(out)
write("Id,STATUS", file = out, append=TRUE)
for(i in 1:length(prediction.svm)) {
    write(paste(i, prediction.svm[i], sep = ","), file = out, append=TRUE)
}