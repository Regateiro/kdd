install.packages("stringr")
install.packages("SnowballC")
install.packages("tm")
library(stringr)
library(SnowballC)
library(tm)

remove_stopwords <- function(comment) {
   for(i in 1:length(stopwords())) {
      comment <- str_replace_all(comment, paste("([^[:alnum:]]|^)",stopwords()[i],"([^[:alnum:]]|$)",sep=""), " ")
   }
   return(comment)
}

###### VARIABLES ########
file <- "fro"    # 'fro', 'wr' or 'wrs'
verbose <- FALSE # TRUE outputs calculated sentiment
threshold <- 0   # threashold for negative/positive decision
n <- 1000        # number of samples to use
#########################

reviews <- read.csv("ReviewList_processed.csv")
sampleIdxs <- c(sample(which(reviews[,2] < 50),n/2),sample(which(reviews[,2] >= 50),n/2))
pos.csv = read.csv(paste(file,"_pos_terms.txt",sep=""), header = FALSE, sep = ",")
neg.csv = read.csv(paste(file,"_neg_terms.txt",sep=""), header = FALSE, sep = ",")
pos.terms <- wordStem(pos.csv[,1])
neg.terms <- wordStem(neg.csv[,1])
pos.weigths <- pos.csv[,2]
neg.weigths <- neg.csv[,2]
thresScores <- vector(length = 101)
sentiment <- rep(0,n)
givenScores <- vector(length = n)

idx <- 1
for(i in sampleIdxs) {
   givenScores[idx] <- reviews[i,2]
   t <- str_split(reviews[i,3], "[ ]")[[1]]

   for(j in 1:length(t)) {
      if(t[j] %in% pos.terms) {
         sentiment[idx] <- sum(sentiment[idx],mean(pos.weigths[which(pos.terms == t[j])]))     
      } else if(t[j] %in% neg.terms) {
         sentiment[idx] <- sum(sentiment[idx],-mean(neg.weigths[which(neg.terms == t[j])]))      
      }
   }

   sentiment[idx] <- sentiment[idx] / length(t)   # normalize the sentiment
   sentiment[idx] <- (sentiment[idx] + 1) * 50    # map to the interval [0,100]
   idx <- sum(idx,1)
}

for(threshold in 0:100) {

TP <- 0
TN <- 0
FP <- 0
FN <- 0
for(i in 1:n) {
   givenScore <- givenScores[i]

   result <- 0
   if(sentiment[i] > threshold) {
      result <- 1
   } else if(sentiment[i] < threshold) {
      result <- -1
   }

   expected <- 0
   if(givenScore > 50) {
      expected <- 1
   } else if(givenScore < 50) {
      expected <- -1
   } else expected <- 1
 
   if(verbose) {
      print(paste("Expected(",expected,"->",givenScore,") : Calculated(",result,"->",sentiment,"): ",reviews[i,3],sep=""))
   }

   if(result == expected) {
      if(result == 1) {
	    TP <- sum(TP,1)
      } else {
	    TN <- sum(TN,1)
      }
   } else {
      if(result == 1) {
	    FP <- sum(FP,1)
      } else {
	    FN <- sum(FN,1)
      }
   }
}
   precision <- sum(TP,TN)/n
   print(paste("P: ",precision, sep=""))
   thresScores[threshold+1] <- precision
}

plot(thresScores)

