library(stringr)
library(SnowballC)
library(tm)

#setwd('C:\\Users\\BlueMoon\\Documents\\GitHub\\kdd\\textMining')

reviews <- read.csv("ReviewList.csv")
load("lScores.data", verbose = TRUE)
wordInfo <- read.csv("wordInfo.csv")

checkAverageError <- function(reviews) {
	# use tf-idf to attribute weights
	# w(t,d)= tf(t,d) * idf(t)
	# idf(t) = log 2 ( N / df(t) )
	# review is processed with sum(weight * wordScore) 

	diff=0
	for(i in 1:length(reviews[,1])) {
		testReview=reviews[i,3]
		t <- str_to_lower(testReview)
		t <- str_replace_all(t, "[[:punct:]]", "")
		t <- remove_stopwords(t) 	
		t <- str_replace_all(t, "[ ]+", " ")
		t <- str_trim(t)
		words <- str_split(t, "[ ]")[[1]]
		words <- wordStem(words, "english")
		sum=0
		counter=0
		for(j in 1:length(words)) {
			index = match(words[j],wordInfo$names)
	
			if(!is.na(index)){
				sum=sum+wordInfo$avgs[index]*wordInfo$idf[index]
				counter=counter+wordInfo$idf[index]
			}
	
		}
		diff=diff+abs((sum/counter)-reviews[i,2])
		#print(paste(diff,"+=",abs((sum/counter)-reviews[i,2])))
	}
	# diff=635681.2
	# diff/length(reviews[,1])=14.89273
	return diff/length(reviews[,1])
}

compute_avg_idf <- function(learnedScores) {
	# use avg rating to attribute wordScore
	names = names(learnedScores)
	avgs = c()
	df = c()
	for(i in 1:length(learnedScores)) {
		df=c(df,length(learnedScores[i][[1]]))
		avgs=c(avgs,mean(learnedScores[i][[1]]))
	}
	wordInfo= data.frame(names, avgs, df)
	wordInfo$idf = log2(length(reviews[[1]])/wordInfo$df)
	write.csv(wordInfo,"wordInfo.csv", row.names=FALSE)
}

remove_stopwords <- function(comment) {
   	sw=stopwords()
   	for(i in 1:length(sw)) {
      	comment <- str_replace_all(comment, paste("([^[:alnum:]]|^)",sw[i],"([^[:alnum:]]|$)",sep=""), " ")
   	}
   	return(comment)
}

compute_scores <- function(reviews) {
	learnedScores = c()
	learnedScores$lol = c(5)
	learnedScores$lol=NULL
	for(i in 1:length(reviews[,1])) {
		#print(reviews[i,3])
   		t <- str_to_lower(reviews[i,3])
		t <- str_replace_all(t, "[[:punct:]]", "")
   		t <- remove_stopwords(t)
		#maybe if we dont remove stopwords, we can see patterns with actual stopwords
		#and figure out useless words (like, 'movie')
   	
   		t <- str_replace_all(t, "[ ]+", " ")
		t <- str_trim(t)
   	
   		words <- str_split(t, "[ ]")[[1]]
   		words  <- wordStem(words, "english")

		for(j in 1:length(words)) {
			learnedScores[[words[j]]]=c(learnedScores[[words[j]]], reviews[i,2])
		}
		#print(words)
	}
	save(learnedScores, file="lScores.data")
}