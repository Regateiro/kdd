install.packages("stringr")
install.packages("SnowballC")
install.packages("tm")
library(stringr)
library(SnowballC)
library(tm)

###### VARIABLES ########
file <- "wrs"    # 'fro', 'wr' or 'wrs'
verbose <- FALSE # TRUE outputs calculated sentiment
threshold <- 0   # threashold for negative/positive decision
#########################
st <- stopwords()

remove_stopwords <- function(comment) {
   for(i in 1:length(stopwords())) {
      comment <- str_replace_all(comment, paste("([^[:alnum:]]|^)",st[i],"([^[:alnum:]]|$)",sep=""), " ")
   }
   return(comment)
}

reviews <- read.csv("ReviewList_processed.csv")
threshold <- 50 # mean(reviews[,2])

pos.csv = read.csv(paste(file,"_pos_terms.txt",sep=""), header = FALSE, sep = ",")
neg.csv = read.csv(paste(file,"_neg_terms.txt",sep=""), header = FALSE, sep = ",")
pos.terms <- wordStem(pos.csv[,1])
neg.terms <- wordStem(neg.csv[,1])
pos.weigths <- pos.csv[,2]
neg.weigths <- neg.csv[,2]

TP <- 0
TN <- 0
FP <- 0
FN <- 0
for(i in 1:length(reviews[,1])) {
   t <- str_split(reviews[i,3], "[ ]")[[1]]
   givenScore <- reviews[i,2]

   sentiment <- 0
   max_weigth <- 0
   for(j in 1:length(t)) {
      if(t[j] %in% pos.terms) {
         tw <- pos.weigths[which(pos.terms == t[j])]   
         sentiment <- sum(sentiment,tw)     
         max_weigth <- max(max_weigth, tw)
      } else if(t[j] %in% neg.terms) {
         tw <- neg.weigths[which(neg.terms == t[j])] 
         sentiment <- sum(sentiment,-tw)       
         max_weigth <- max(max_weigth, tw)
      }
   }

   sentiment <- sentiment / length(t)   # normalize the sentiment
   sentiment <- (sentiment + 1) * 50    # map to the interval [0,100]

   result <- 0
   if(sentiment > threshold) {
      result <- 1
   } else if(sentiment < threshold) {
      result <- -1
   }

   expected <- 0
   if(givenScore > threshold) {
      expected <- 1
   } else if(givenScore < threshold) {
      expected <- -1
   } else expected <- result
 
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

print(paste("P: ",sum(TP,TN)/length(reviews[,1]), sep=""))