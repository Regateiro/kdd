# Install packages
install.packages("adabag")
install.packages("randomForest")
install.packages('kknn')
install.packages('party')

# Load libraries
library(party)
library(kknn)
library(adabag)
library(randomForest)

#setwd('C:\\Users\\BlueMoon\\Documents\\GitHub\\kdd')

# Load CSV
data.train <- read.csv('training.csv', TRUE, ';')
data.test <- read.csv('test.csv', TRUE, ';')
data.train$STATUS <- as.factor(data.train$STATUS)

# predict expects the label to be present to build the confusion matrix... I just used ones
data.test$STATUS <- as.factor(c(rep(1, 25),rep(-1,25)))

# Decision Tree with boosting
model.boost_dt <- boosting(STATUS ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + 
                                 X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + 
                                 X20 + X21 + X22 + X23 + X24 + X25 + X26 + X27 + X28 + 
                                 X29 + X30, data = data.train, coeflearn="Zhu")

prediction.boost_dt <- predict(model.boost_dt, newdata=data.test, type="class")

# Decision Tree with RandomForest
model.forest_dt <- cforest(as.factor(STATUS) ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + 
                                 X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + 
                                 X20 + X21 + X22 + X23 + X24 + X25 + X26 + X27 + X28 + 
                                 X29 + X30, data = data.train, controls=cforest_unbiased(ntree=500, mtry=5))
prediction.forest_dt<- predict(model.forest_dt, data.test, OOB=TRUE, type = "prob")
prediction.forest_dt2<- matrix( 0,  nrow=50, ncol=2)
for(i in 1:50) {
	prediction.forest_dt2[i,1] <- prediction.forest_dt[[i]][1]
	prediction.forest_dt2[i,2] <- prediction.forest_dt[[i]][2]
}

#model.forest_dt <- randomForest(as.factor(STATUS) ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + 
#                                 X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + 
#                                 X20 + X21 + X22 + X23 + X24 + X25 + X26 + X27 + X28 + 
#                                 X29 + X30, data = data.train)
#prediction.forest_dt <- predict(model.forest_dt, newdata=data.test, type="prob")

data.train <- read.csv('training.csv', TRUE, ';')
data.test <- read.csv('test.csv', TRUE, ';')
data.test$STATUS <- array(0,50)
res <- kknn(STATUS~., data.train, data.test)
predictions <- matrix( 0,  nrow=50, ncol=2)
for(i in 1:50) {
    predictions[i,1] <- ifelse(res$fit[i]<0,(-res$fit[i]+1)/2,1-(res$fit[i]+1)/2)
    predictions[i,2] <- ifelse(res$fit[i]>0,(res$fit[i]+1)/2,1-(-res$fit[i]+1)/2)
}

# Ensemble
prediction.bagboostforest_ens_dt.prob <- predictions + prediction.boost_dt$prob + prediction.forest_dt2

prediction.bagboostforest_ens_dt <- vector(,length(prediction.bagboostforest_ens_dt.prob[,1]))
for(i in 1:length(prediction.bagboostforest_ens_dt.prob[,1])) {
    prediction.bagboostforest_ens_dt[i] <- ifelse(prediction.bagboostforest_ens_dt.prob[i,1] < prediction.bagboostforest_ens_dt.prob[i,2],1,-1)
}

# Output
out <- "results.csv"
if (file.exists(out)) file.remove(out)
write("Id,STATUS", file = out, append=TRUE)
for(i in 1:length(prediction.bagboostforest_ens_dt)) {
    write(paste(i, prediction.bagboostforest_ens_dt[i], sep = ","), file = out, append=TRUE)
}