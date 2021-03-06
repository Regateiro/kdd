# Load libraries
library(rpart)

# Load CSV
data.train = read.csv('training.csv', TRUE, ';')
data.test = read.csv('test.csv', TRUE, ';')

data.train$STATUS <- as.factor(data.train$STATUS)

# Simple Decision Tree
model.dt <- rpart(STATUS ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + 
				   X11 + X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + 
                           X20 + X21 + X22 + X23 + X24 + X25 + X26 + X27 + X28 + 
                           X29 + X30, data = data.train, method="class")
prediction.dt <- predict(model.dt,data.test,type="class")

out <- "dt.out"
if (file.exists(out)) file.remove(out)
write("Id,STATUS", file = out, append=TRUE)
for(i in 1:length(prediction.dt)) {
    write(paste(i, prediction.dt[i], sep = ","), file = out, append=TRUE)
}