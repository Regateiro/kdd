# Install packages
install.packages('class')

# Load libraries
library(class)

#setwd('C:\\Users\\BlueMoon\\Documents\\GitHub\\kdd')

# Load CSV
data.train = read.csv('training.csv', TRUE, ';')
data.test = read.csv('test.csv', TRUE, ';')
data.status <- as.factor(data.train$STATUS)
data.train$STATUS <- NULL

data.train$X27 <- NULL
data.train$X4 <- NULL

# Decision Tree with Forest of Conditional Inference Trees
lol <- knn(data.train, data.test, data.status)

out <- "results.csv"
if (file.exists(out)) file.remove(out)
write("Id,STATUS", file = out, append=TRUE)
for(i in 1:length(lol)) {
    write(paste(i, prediction.forest_dt[i], sep = ","), file = out, append=TRUE)
}
