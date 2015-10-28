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

# Decision Tree with boosting
model.boost_dt <- boosting(STATUS ~ X10 + X28 + X11 + X26, data = data.train)
# 1 9 24 29


prediction.boost_dt <- predict(model.boost_dt, newdata=data.test, type="class")

out <- "boost_dt.out"
if (file.exists(out)) file.remove(out)
write("Id,STATUS", file = out, append=TRUE)
for(i in 1:length(prediction.boost_dt$class)) {
    write(paste(i, prediction.boost_dt$class[i], sep = ","), file = out, append=TRUE)
}