install.packages("stringr")
install.packages("SnowballC")
install.packages("tm")
library(stringr)
library(SnowballC)
library(tm)


#setwd('C:\\Users\\BlueMoon\\Documents\\GitHub\\kdd\\textMining')

###### VARIABLES ########
file <- "wrs"    # 'fro', 'wr' or 'wrs'
verbose <- TRUE  # TRUE outputs calculated sentiment
#########################

remove_stopwords <- function(comment) {
   for(i in 1:length(stopwords())) {
      comment <- str_replace_all(comment, paste("([^[:alnum:]]|^)",stopwords()[i],"([^[:alnum:]]|$)",sep=""), " ")
   }
   return(comment)
}

reviews <- read.csv("ReviewList.csv")

pos.csv = read.csv(paste(file,"_pos_terms.txt",sep=""), header = FALSE, sep = ",")
neg.csv = read.csv(paste(file,"_neg_terms.txt",sep=""), header = FALSE, sep = ",")
pos.terms <- pos.csv[,1]
neg.terms <- neg.csv[,1]
pos.weigths <- pos.csv[,2]
neg.weigths <- neg.csv[,2]

for(i in 1:length(reviews[,1])) {
   t <- str_to_lower(reviews[i,3])
   t <- remove_stopwords(t)
   t <- str_replace_all(t, "[,]", " ")
   t <- str_replace_all(t, "[ ]+", " ")
   t <- str_replace_all(t, "[[:punct:]]", "")
   t <- str_trim(t)
   t <- str_split(t, "[ ]")[[1]]
   tStem <- wordStem(t, "english")

   result <- 0
   sentiment <- 0
   for(j in 1:length(t)) {
      if(t[j] %in% pos.terms) {
         sentiment <- sentiment + pos.weigths[which(pos.terms == t[j])]
      } else if(tStem[j] %in% pos.terms) {
         sentiment <- sentiment + pos.weigths[which(pos.terms == tStem[j])]
      } else if(t[j] %in% neg.terms) {
         sentiment <- sentiment - neg.weigths[which(neg.terms == t[j])]
      } else if(tStem[j] %in% neg.terms) {
         sentiment <- sentiment - neg.weigths[which(neg.terms == tStem[j])]
      }
   }

   if(sentiment > 0) {
      result <- 1
   } else if(sentiment < 0) {
      result <- -1
   }
 
   if(verbose) {
      print(paste(result,"(",sentiment,"): ",reviews[i,3],sep=""))
   } else {
      print(paste(result,": ",reviews[i,3],sep=""))
   }
}