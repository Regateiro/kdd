# Install packages
install.packages("e1071")

# Load libraries
library(e1071)

# Load CSV
data.train <- read.csv('training.csv', TRUE, ';')
data.test <- read.csv('test.csv', TRUE, ';')

data.train$STATUS <- as.factor(data.train$STATUS)
data.train$X52 <- data.train$X17 * data.train$X19

# predict expects the label to be present to build the confusion matrix... I just used ones
data.test$STATUS <- as.factor(c(rep(1, 25),rep(-1,25)))
data.test$X52 <- data.test$X17 * data.test$X19

# Decision Tree with boosting
model.svm <- svm(STATUS ~ X24 + X18 + X52 + X15 + X23, data = data.train, probability=TRUE)

prediction.svm <- predict(model.svm, newdata=data.test, probability=TRUE)

out <- "svm.out"
if (file.exists(out)) file.remove(out)
write("Id,STATUS", file = out, append=TRUE)
for(i in 1:length(prediction.svm)) {
    write(paste(i, prediction.svm[i], sep = ","), file = out, append=TRUE)
}