library(stringr)
library(SnowballC)
library(tm)

setwd('C:\\Users\\BlueMoon\\Documents\\GitHub\\kdd\\textMining')

reviews <- read.csv("ReviewList_processed.csv",stringsAsFactors=FALSE)
load("lScores.data", verbose = TRUE)
wordInfo <- read.csv("wordInfo.csv")
load("featuresHistogram.data", verbose = TRUE)

stratHistoClass <- function(reviews, features, n=500){
	indexes = sample(1:length(reviews[,1]), n) 

	diff=0
	for(i in 1:n) {
		histogram=rep(0,101)
		
		words <- str_split(reviews[indexes[i],3], "[ ]")[[1]]

		for(j in 1:length(words)) {
			index = match(words[j],wordInfo$names)
	
			if(!is.na(index)){
				originalH = as.numeric(features[index,2:102])
				#print(originalH)
				histogram=histogram+originalH/sum(originalH)

				#for(k in 1:101){
				#	histogram[k]=histogram[k]+features[index,k+1]
				#}
			}
		}	
		#plot(histogram)
		prediction=which.max(histogram)-1
		#print(prediction)
		#print(reviews[indexes[i],2])
		diff=diff+abs(prediction-reviews[indexes[i],2])
	}
	#print(diff/n)
	return (diff/n)
}
#stratHistoClass(reviews, features, n=1000)

createHistograms <- function(wordInfo, learnedScores){
	features = data.frame(wordInfo$names)
	zeros=rep(0,length(wordInfo$names))
	for(i in 1:101){
		features[[1+i]]=zeros
	}

	for(i in 1:length(learnedScores)) {
		histogram = rep(0, 101)
		#histogramN = rep(0, 101)
		for(j in 1:length(learnedScores[i][[1]])){
			histogram[learnedScores[i][[1]][j]+1]=histogram[learnedScores[i][[1]][j]+1]+1
		}
		#print(histogram )

		for(b in 2:102){ features[i,b]=0 }
		# each point in the original histogram
		for(b in 1:101){
			if(histogram[b]!=0){
				a=histogram[b]	#height
				c=(sd(learnedScores[i][[1]])+1)/sqrt(length(unique(learnedScores[i][[1]])))	#sd
				if(is.na(c)) c=0.1

				#each point in the full histogram
				for(x in 0:100){
					#histogramN[x+1]=( a*exp(1)^( -(x-b+1)^2 / (2*c^2)) )
					features[i,x+2]=features[i,x+2]+( a*exp(1)^( -(x-b+1)^2 / (2*c^2)) )
				}
				#print(paste("with b=",b,"the histo goes",histogramN[1]))
			}
			
		}
		#plot(as.numeric(features[i,2:102]))
		#print(histogramN)
		#plot(histogramN)
		print(i)
	}
	save(features, file="featuresHistogramMod.data")
}

cleanAllReviews <- function(reviews) {
	sw = stopwords()
	for(i in 1:length(reviews$Value)) {
		reviews[i,3] <- str_to_lower(reviews[i,3])
		reviews[i,3] <- str_replace_all(reviews[i,3], "[[:punct:]]", "")
		reviews[i,3] <- remove_stopwords(reviews[i,3],sw) 	
		reviews[i,3] <- str_replace_all(reviews[i,3], "[ ]+", " ")
		reviews[i,3] <- str_trim(reviews[i,3])
		words <- str_split(reviews[i,3], "[ ]")[[1]]
		words <- wordStem(words, "english")
		reviews[i,3]<-paste(words, collapse = ' ')
		print(i)
	}
	write.csv(reviews,"ReviewList_processed.csv", row.names=FALSE)
}


createDataFrame <- function(reviews, wordInfo) {
	commonWords=wordInfo$names[wordInfo$df!=1]
	zz_rates=reviews$Value
	l=length(zz_rates)
	#halfLength=length(reviews$Value)/2
	features = data.frame(zz_rates)
	zeros=rep(0,l)
	for(i in 1:length(commonWords)){
		str=as.character(commonWords[i])
		features[[str]]=zeros
		print(i)
	}
	save(features, file="features.data")
	#load("features.data", verbose = TRUE)

	sw = stopwords()
	for(i in 1:halfLength) {
		words <- str_split(reviews[i,3], "[ ]")[[1]]

		for(j in 1:length(words)) {
			index = match(words[j], commonWords)
	
			if(!is.na(index)){
				features[i, index+1]=features[i, index+1]+1
			}
		}
		print(i)
	}
	save(features, file="featuresP.data")
	#load("featuresP.data", verbose = TRUE)
}

# so this picks 500 random reviews and classifies them using a avg/sd weight
# after that, the error from the original review score is calculated
# returns the average of the errors from all 500 reviews
stratifiedSDoverAvg <- function(reviews, wordInfo, n=500) {
	indexes = sample(1:length(reviews[,1]), n) 

	diff=0
	for(i in 1:n) {
		words <- str_split(reviews[indexes[i],3], "[ ]")[[1]]
		sum=0
		counter=0
		for(j in 1:length(words)) {
			index = match(words[j],wordInfo$names)
	
			if(!is.na(index)){
				sum=sum+wordInfo$avgs[index]*wordInfo$df[index]/wordInfo$sd[index]
				counter=counter+(wordInfo$df[index]/wordInfo$sd[index])
			}
		}
		diff=diff+abs((sum/counter)-reviews[indexes[i],2])
	}
	return (diff/n)
}

# so this picks 500 random reviews and classifies them using a tf/idf weight
# after that, the error from the original review score is calculated
# returns the average of the errors from all 500 reviews
checkAverageErrorTFIDF <- function(reviews, wordInfo, n=500) {
	indexes = sample(1:length(reviews[,1]), n) 

	# use tf-idf to attribute weights
	# w(t,d)= tf(t,d) * idf(t)
	# idf(t) = log 2 ( N / df(t) )
	# review is processed with sum(weight * wordScore) 

	diff=0
	for(i in 1:n) {
		words <- str_split(reviews[indexes[i],3], "[ ]")[[1]]
		sum=0
		counter=0
		for(j in 1:length(words)) {
			index = match(words[j],wordInfo$names)
	
			if(!is.na(index)){
				if(wordInfo$df[index]!=1){
					sum=sum+wordInfo$avgs[index]*wordInfo$idf[index]
					counter=counter+(wordInfo$idf[index])
				}
			}
		}


		diff=diff+abs((sum/counter)-reviews[indexes[i],2])
		#print(paste(diff,"+=",abs((sum/counter)-reviews[i,2])))
	}

	# If executed on all the reviews,
	# diff=635681.2
	# diff/length(reviews[,1])=14.89273
	return (diff/n)
}

compute_avg_idf <- function(learnedScores) {
	names = names(learnedScores)
	avgs = c()
	df = c()
	sd = c()
	for(i in 1:length(learnedScores)) {
		df=c(df,length(learnedScores[i][[1]]))
		avgs=c(avgs,mean(learnedScores[i][[1]]))
		sd=c(sd,sd(learnedScores[i][[1]]))
	}

	wordInfo= data.frame(names, avgs, df, sd)
	wordInfo$idf = log2(length(reviews[[1]])/wordInfo$df)
	wordInfo$sd[is.na(wordInfo$sd)] <- 0 
	wordInfo$sd <- wordInfo$sd + 1 
	write.csv(wordInfo,"wordInfo.csv", row.names=FALSE)
}

remove_stopwords <- function(comment,sw) {
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