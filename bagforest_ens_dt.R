# Install packages
install.packages("adabag")
install.packages("randomForest")

# Load libraries
library(adabag)
library(randomForest)

setwd('C:\\Users\\DiogoJosé\\Documents\\kdd')

# Load CSV
data.train <- read.csv('training.csv', TRUE, ';')
data.test <- read.csv('test.csv', TRUE, ';')
data.train$STATUS <- as.factor(data.train$STATUS)

# predict expects the label to be present to build the confusion matrix... I just used ones
data.test$STATUS <- as.factor(c(rep(1, 25),rep(-1,25)))

# Decision Tree with Bagging
model.bag_dt <- bagging(STATUS ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + 
                                 X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + 
                                 X20 + X21 + X22 + X23 + X24 + X25 + X26 + X27 + X28 + 
                                 X29 + X30, data = data.train)

prediction.bag_dt <- predict(model.bag_dt, newdata=data.test, type="class")

# Decision Tree with RandomForest
model.forest_dt <- randomForest(as.factor(STATUS) ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + 
                                 X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + 
                                 X20 + X21 + X22 + X23 + X24 + X25 + X26 + X27 + X28 + 
                                 X29 + X30, data = data.train)

prediction.forest_dt <- predict(model.forest_dt, newdata=data.test, type="prob")

# Ensemble
prediction.bagforest_ens_dt.prob <- prediction.bag_dt$prob + prediction.forest_dt

prediction.bagforest_ens_dt <- vector(,length(prediction.bagforest_ens_dt.prob[,1]))
for(i in 1:length(prediction.bagforest_ens_dt.prob[,1])) {
    prediction.bagforest_ens_dt[i] <- ifelse(prediction.bagforest_ens_dt.prob[i,1] < prediction.bagforest_ens_dt.prob[i,2],1,-1)
}

# Output
out <- "bagforest_ens_dt.out"
if (file.exists(out)) file.remove(out)
write("Id,STATUS", file = out, append=TRUE)
for(i in 1:length(prediction.bagforest_ens_dt)) {
    write(paste(i, prediction.bagforest_ens_dt[i], sep = ","), file = out, append=TRUE)
}