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
data.train$X31 <- data.train$X1 * data.train$X22
data.train$X32 <- data.train$X1 * data.train$X28
data.train$X33 <- data.train$X3 * data.train$X10
data.train$X34 <- data.train$X3 * data.train$X18
data.train$X35 <- data.train$X3 * data.train$X22
data.train$X36 <- data.train$X3 * data.train$X28
data.train$X37 <- data.train$X6 * data.train$X7
data.train$X38 <- data.train$X6 * data.train$X8
data.train$X39 <- data.train$X6 * data.train$X18
data.train$X40 <- data.train$X7 * data.train$X12
data.train$X41 <- data.train$X7 * data.train$X19
data.train$X42 <- data.train$X7 * data.train$X24
data.train$X43 <- data.train$X7 * data.train$X30
data.train$X44 <- data.train$X8 * data.train$X12
data.train$X45 <- data.train$X8 * data.train$X19
data.train$X46 <- data.train$X8 * data.train$X23
data.train$X47 <- data.train$X8 * data.train$X24
data.train$X48 <- data.train$X8 * data.train$X30
data.train$X49 <- data.train$X12 * data.train$X17
data.train$X50 <- data.train$X12 * data.train$X18
data.train$X51 <- data.train$X13 * data.train$X21
data.train$X52 <- data.train$X17 * data.train$X19
data.train$X53 <- data.train$X18 * data.train$X19
data.train$X54 <- data.train$X18 * data.train$X30
data.train$X55 <- data.train$X21 * data.train$X23
data.train$X56 <- data.train$X23 * data.train$X25
data.train$X57 <- data.train$X23 * data.train$X26

# Essure consistent results
set.seed(1234)

results <- as.vector(rep(0,30))

# Decision Tree with boosting
for(i in 1:57) {
  prediction.boost_dt <- boosting.cv(as.formula(paste("STATUS ~ X33 + X47 + X49 + X48 + X",i,sep="")), data = data.train, mfinal = 40)
  results[i] <- sum(diag(prediction.boost_dt$confusion))/sum(prediction.boost_dt$confusion)
}

for(i in 1:length(results)) {
  print(results[i])
}
