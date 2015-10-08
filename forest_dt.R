# Install packages
install.packages("randomForest")

# Load libraries
library(randomForest)

setwd('C:\\Users\\BlueMoon\\Documents\\GitHub\\kdd')

# Load CSV
data.train = read.csv('training.csv', TRUE, ';')
data.test = read.csv('test.csv', TRUE, ';')
data.train$STATUS <- as.factor(data.train$STATUS)

set.seed(415)
# Decision Tree with RandomForest
model.forest_dt <- randomForest(as.factor(STATUS) ~ X1 + X2 + X3 + X5 + X6 + X7 + X8 + X9 + X10 + 
                                 X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + 
                                 X20 + X21 + X22 + X23 + X24 + X25 + X26 + X28 + 
                                 X29 + X30, data = data.train, nTree=2000)
#X4 + X27 + 
varImpPlot(model.forest_dt)
prediction.forest_dt <- predict(model.forest_dt, newdata=data.test, type="class")

out <- "results.csv"
if (file.exists(out)) file.remove(out)
write("Id,STATUS", file = out, append=TRUE)
for(i in 1:length(prediction.forest_dt)) {
    write(paste(i, prediction.forest_dt[i], sep = ","), file = out, append=TRUE)
}