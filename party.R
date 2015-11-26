# Install packages
install.packages('party')

# Load libraries
library(party)

#setwd('C:\\Users\\BlueMoon\\Documents\\GitHub\\kdd')

# Load CSV
data.train = read.csv('training.csv', TRUE, ';')
data.test = read.csv('test.csv', TRUE, ';')
data.train$STATUS <- as.factor(data.train$STATUS)

# Decision Tree with Forest of Conditional Inference Trees
model.forest_dt <- cforest(as.factor(STATUS) ~ ., data = data.train, controls=cforest_unbiased(ntree=100, mtry=3))
prediction.forest_dt<- predict(model.forest_dt, data.test, OOB=TRUE, type = "response")

out <- "results.csv"
if (file.exists(out)) file.remove(out)
write("Id,STATUS", file = out, append=TRUE)
for(i in 1:length(prediction.forest_dt)) {
    write(paste(i, prediction.forest_dt[i], sep = ","), file = out, append=TRUE)
}