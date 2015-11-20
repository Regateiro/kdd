install.packages("stringr")
install.packages("SnowballC")
install.packages("tm")
library(stringr)
library(SnowballC)
library(tm)

reviews <- read.csv("ReviewList.csv")

pos.terms = read.csv("wr_pos_terms.txt", header = FALSE)
neg.terms = read.csv("wr_neg_terms.txt", header = FALSE)
pos.terms <- pos.terms[,1]
neg.terms <- neg.terms[,1]

for(i in 1:length(reviews[,1])) {
   t <- str_to_lower(reviews[i,3])
   t <- remove_stopwords(t)
   t <- str_replace_all(t, "[,]", " ")
   t <- str_replace_all(t, "[ ]+", " ")
   t <- str_replace_all(t, "[[:punct:]]", "")
   t <- str_trim(t)
   t <- str_split(t, "[ ]")[[1]]
   tStem <- wordStem(t, "english")

   sentiment <- 0
   for(j in 1:length(t)) {
      if(t[j] %in% pos.terms || tStem[j] %in% pos.terms) {
         sentiment <- sentiment + 1
      } else if(t[j] %in% neg.terms || tStem[j] %in% neg.terms) {
         sentiment <- sentiment - 1
      }
   }

   if(sentiment > 0) {
      sentiment <- 1
   } else if(sentiment < 0) {
      sentiment <- -1
   }
 
   print(paste(sentiment,reviews[i,3],sep=" : "))
}

remove_stopwords <- function(comment) {
   for(i in 1:length(stopwords())) {
      comment <- str_replace_all(comment, paste("([^[:alnum:]]|^)",stopwords()[i],"([^[:alnum:]]|$)",sep=""), " ")
   }
   return(comment)
}