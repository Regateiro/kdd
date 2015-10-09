# Install packages
install.packages('class')

# Load libraries
library(class)

setwd('C:\\Users\\BlueMoon\\Documents\\GitHub\\kdd')

# Load CSV
data.train = read.csv('training.csv', TRUE, ';')
data.test = read.csv('test.csv', TRUE, ';')
data.status <- as.factor(data.train$STATUS)
data.train$STATUS <- NULL

# here we do knn with cv on the training set and try to find the best k
maxAccuracy <- 0
maxK <- 0
for(kVal in 1:117){
    # k-nearest neighbor
    predictions <- knn.cv(data.train, data.status, prob=TRUE, k=kVal)

    accuracy <- 0
    for(i in 1:length(predictions)) {
        temp <- ifelse(predictions [i] == data.status[i],1,0)
        accuracy <- accuracy + temp
    }
    accuracy <- accuracy / length(data.status)
    maxK <- ifelse(maxAccuracy > accuracy,maxK,kVal)
    maxAccuracy <- ifelse(maxAccuracy > accuracy,maxAccuracy,accuracy)
}
print(maxAccuracy)
print(maxK)

# now that we know the best k, we can use it on the test set
predictions <- knn(data.train, data.test, data.status, prob=TRUE, k=maxK)

out <- "results.csv"
if (file.exists(out)) file.remove(out)
write("Id,STATUS", file = out, append=TRUE)
for(i in 1:length(predictions)) {
    write(paste(i, predictions[i], sep = ","), file = out, append=TRUE)
}
