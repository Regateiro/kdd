# Install packages
install.packages("adabag")

# Load libraries
library(adabag)

# Load CSV
data.train <- read.csv('training.csv', TRUE, ';')
#data.test <- read.csv('test.csv', TRUE, ';')

# Pre-processing
data.train$STATUS <- as.factor(data.train$STATUS)
data.train$ID <- NULL

# Essure consistent results
set.seed(1234)

# Decision Tree with boosting
prediction.boost_dt <- boosting.cv(STATUS ~ X4, data = data.train, mfinal = 40)

# print(prediction.boost_dt$confusion)
print(sum(diag(prediction.boost_dt$confusion))/sum(prediction.boost_dt$confusion))

prediction.boost_dt <- boosting.cv(STATUS ~ X1 + X10, data = data.train, mfinal = 40)
print(sum(diag(prediction.boost_dt$confusion))/sum(prediction.boost_dt$confusion))

prediction.boost_dt <- boosting.cv(STATUS ~ X2 + X10, data = data.train, mfinal = 40)
print(sum(diag(prediction.boost_dt$confusion))/sum(prediction.boost_dt$confusion))

prediction.boost_dt <- boosting.cv(STATUS ~ X3 + X10, data = data.train, mfinal = 40)
print(sum(diag(prediction.boost_dt$confusion))/sum(prediction.boost_dt$confusion))

prediction.boost_dt <- boosting.cv(STATUS ~ X4 + X10, data = data.train, mfinal = 40)
print(sum(diag(prediction.boost_dt$confusion))/sum(prediction.boost_dt$confusion))

prediction.boost_dt <- boosting.cv(STATUS ~ X5 + X10, data = data.train, mfinal = 40)
print(sum(diag(prediction.boost_dt$confusion))/sum(prediction.boost_dt$confusion))

prediction.boost_dt <- boosting.cv(STATUS ~ X6 + X10, data = data.train, mfinal = 40)
print(sum(diag(prediction.boost_dt$confusion))/sum(prediction.boost_dt$confusion))

prediction.boost_dt <- boosting.cv(STATUS ~ X7 + X10, data = data.train, mfinal = 40)
print(sum(diag(prediction.boost_dt$confusion))/sum(prediction.boost_dt$confusion))

prediction.boost_dt <- boosting.cv(STATUS ~ X8 + X10, data = data.train, mfinal = 40)
print(sum(diag(prediction.boost_dt$confusion))/sum(prediction.boost_dt$confusion))

prediction.boost_dt <- boosting.cv(STATUS ~ X9 + X10, data = data.train, mfinal = 40)
print(sum(diag(prediction.boost_dt$confusion))/sum(prediction.boost_dt$confusion))

prediction.boost_dt <- boosting.cv(STATUS ~ X11 + X10, data = data.train, mfinal = 40)
print(sum(diag(prediction.boost_dt$confusion))/sum(prediction.boost_dt$confusion))

prediction.boost_dt <- boosting.cv(STATUS ~ X12 + X10, data = data.train, mfinal = 40)
print(sum(diag(prediction.boost_dt$confusion))/sum(prediction.boost_dt$confusion))

prediction.boost_dt <- boosting.cv(STATUS ~ X13 + X10, data = data.train, mfinal = 40)
print(sum(diag(prediction.boost_dt$confusion))/sum(prediction.boost_dt$confusion))

prediction.boost_dt <- boosting.cv(STATUS ~ X14 + X10, data = data.train, mfinal = 40)
print(sum(diag(prediction.boost_dt$confusion))/sum(prediction.boost_dt$confusion))

prediction.boost_dt <- boosting.cv(STATUS ~ X15 + X10, data = data.train, mfinal = 40)
print(sum(diag(prediction.boost_dt$confusion))/sum(prediction.boost_dt$confusion))

prediction.boost_dt <- boosting.cv(STATUS ~ X16 + X10, data = data.train, mfinal = 40)
print(sum(diag(prediction.boost_dt$confusion))/sum(prediction.boost_dt$confusion))

prediction.boost_dt <- boosting.cv(STATUS ~ X17 + X10, data = data.train, mfinal = 40)
print(sum(diag(prediction.boost_dt$confusion))/sum(prediction.boost_dt$confusion))

prediction.boost_dt <- boosting.cv(STATUS ~ X18 + X10, data = data.train, mfinal = 40)
print(sum(diag(prediction.boost_dt$confusion))/sum(prediction.boost_dt$confusion))

prediction.boost_dt <- boosting.cv(STATUS ~ X19 + X10, data = data.train, mfinal = 40)
print(sum(diag(prediction.boost_dt$confusion))/sum(prediction.boost_dt$confusion))

prediction.boost_dt <- boosting.cv(STATUS ~ X20 + X10, data = data.train, mfinal = 40)
print(sum(diag(prediction.boost_dt$confusion))/sum(prediction.boost_dt$confusion))

prediction.boost_dt <- boosting.cv(STATUS ~ X21 + X10, data = data.train, mfinal = 40)
print(sum(diag(prediction.boost_dt$confusion))/sum(prediction.boost_dt$confusion))

prediction.boost_dt <- boosting.cv(STATUS ~ X22 + X10, data = data.train, mfinal = 40)
print(sum(diag(prediction.boost_dt$confusion))/sum(prediction.boost_dt$confusion))

prediction.boost_dt <- boosting.cv(STATUS ~ X23 + X10, data = data.train, mfinal = 40)
print(sum(diag(prediction.boost_dt$confusion))/sum(prediction.boost_dt$confusion))

prediction.boost_dt <- boosting.cv(STATUS ~ X24 + X10, data = data.train, mfinal = 40)
print(sum(diag(prediction.boost_dt$confusion))/sum(prediction.boost_dt$confusion))

prediction.boost_dt <- boosting.cv(STATUS ~ X25 + X10, data = data.train, mfinal = 40)
print(sum(diag(prediction.boost_dt$confusion))/sum(prediction.boost_dt$confusion))

prediction.boost_dt <- boosting.cv(STATUS ~ X26 + X10, data = data.train, mfinal = 40)
print(sum(diag(prediction.boost_dt$confusion))/sum(prediction.boost_dt$confusion))

prediction.boost_dt <- boosting.cv(STATUS ~ X27 + X10, data = data.train, mfinal = 40)
print(sum(diag(prediction.boost_dt$confusion))/sum(prediction.boost_dt$confusion))

prediction.boost_dt <- boosting.cv(STATUS ~ X28 + X10, data = data.train, mfinal = 40)
print(sum(diag(prediction.boost_dt$confusion))/sum(prediction.boost_dt$confusion))

prediction.boost_dt <- boosting.cv(STATUS ~ X29 + X10, data = data.train, mfinal = 40)
print(sum(diag(prediction.boost_dt$confusion))/sum(prediction.boost_dt$confusion))

prediction.boost_dt <- boosting.cv(STATUS ~ X30 + X10, data = data.train, mfinal = 40)
print(sum(diag(prediction.boost_dt$confusion))/sum(prediction.boost_dt$confusion))

