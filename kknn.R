# Install packages
install.packages('kknn')

# Load libraries
library(kknn)

setwd('C:\\Users\\BlueMoon\\Documents\\GitHub\\kdd')

# Load CSV
data.train = read.csv('training.csv', TRUE, ';')
data.test = read.csv('test.csv', TRUE, ';')
#data.status <- as.factor(data.train$STATUS)
#data.train$STATUS <- NULL
data.test$STATUS <- array(0,50)

# weighted kNN

# method 1
res <- kknn(STATUS~., data.train, data.test)
#table(data.test$STATUS, res$fit)
#kknn.dist(learn, valid, k = 10, distance = 2)

# method 2
fit.kknn <- kknn(STATUS ~ ., data.train, data.test)
#table(data.test$STATUS, fit.kknn$fit)
fit.train1 <- train.kknn(STATUS ~ ., data.train, kmax = 15, 
    kernel = c("triangular", "rectangular", "epanechnikov", "optimal"), 
    distance = 1)
#table(predict(fit.train1, data.test), data.test$STATUS)
predictions <- predict(fit.train1, data.test)

# move from regression to classification
predictions <- array(0,50)
for(i in 1:length(predictions)) {
    predictions[i] <- ifelse(res$fit[i]>0,1,-1)
}

#sum(pr.test==cl.test)/length(cl.test)

out <- "results.csv"
if (file.exists(out)) file.remove(out)
write("Id,STATUS", file = out, append=TRUE)
for(i in 1:length(predictions)) {
    write(paste(i, predictions[i], sep = ","), file = out, append=TRUE)
}
