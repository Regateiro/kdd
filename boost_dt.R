# Install packages
install.packages("adabag")

# Load libraries
library(adabag)

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
model.boost_dt <- boosting(STATUS ~ X33 + X47 + X49 + X48, data = data.train)

prediction.boost_dt <- predict(model.boost_dt, newdata=data.test, type="class")

out <- "boost_dt.out"
if (file.exists(out)) file.remove(out)
write("Id,STATUS", file = out, append=TRUE)
for(i in 1:length(prediction.boost_dt$class)) {
    write(paste(i, prediction.boost_dt$class[i], sep = ","), file = out, append=TRUE)
}