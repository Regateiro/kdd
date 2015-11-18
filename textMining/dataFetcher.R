#install.packages("httr")
library(httr)

#URL and API keys for Import.IO movie list
url = "https://api.import.io/store/data/71e12311-6ce3-436e-8b5d-9c366a87276d/_query?input/webpage/url=http%3A%2F%2Fwww.imdb.com%2Fsearch%2Ftitle%3Flanguages%3Den%257C1%26num_votes%3D10000%2C%26sort%3Duser_rating%2Cdesc%26start%3D"
url2 = "%26title_type%3Dfeature&_user=4911226d-82c8-4f2f-9e06-e04bffad812e&_apikey=4911226d82c84f2f9e06e04bffad812e3ee3dd322ae4d7309f3751ce6913e2da924f27aabd67c1a8d8aee488dc392b80c9de4885cd3d16c6d630151818526d3b5b50b3d57bcf816bfa015eeeb4989a1f"
index = 1

ids = c()
while(index<5200){
	# HTTP Request
	r <- GET(paste(url, index, url2, sep=""))
	results = content(r)$result
	for(i in 1:length(results)){
		ids=c(ids, substr(results[[i]]$image_link, 27, 35))
	}
	index = index + 50
}

# URL and API keys for Import.IO reviews
url = "http://api.import.io/store/data/66ff6f91-93ef-4f86-8307-ba7e6dcbc628/_query?input/webpage/url=http%3A%2F%2Fwww.imdb.com%2Ftitle%2F"
url2 = "%2Fcriticreviews%3Fref_%3Dtt_ov_rt&_user=4911226d-82c8-4f2f-9e06-e04bffad812e&_apikey=4911226d82c84f2f9e06e04bffad812e3ee3dd322ae4d7309f3751ce6913e2da924f27aabd67c1a8d8aee488dc392b80c9de4885cd3d16c6d630151818526d3b5b50b3d57bcf816bfa015eeeb4989a1f"

# HTTP Request
#critNumber=c()
#critReview=c()
#critID=c()
critics=c()
#P=data.frame(Value=c("0"),Comment=c("sucks"))
for(j in 1:length(ids)){
	print(ids[j])
	r <- GET(paste(url, ids[j], url2, sep=""))
	while(r[2]!=200){
		r <- GET(paste(url, ids[j], url2, sep=""))
	}
	results = content(r)$result
	if(length(results)!=0){
		for(i in 1:length(results)){
			#P=rbind(P,c(results[i][[1]]$critscore_number, results[i][[1]]$summary_description))
			#critNumber=c(critNumber, results[i][[1]]$critscore_number)
			#critReview=c(critReview, results[i][[1]]$summary_description)
			#critID=c(critID, ids[j])
			critics=c(critics, ids[j], results[i][[1]]$critscore_number, results[i][[1]]$summary_description)
			#critic = c(content(r)$result[i][[1]]$critscore_number,
			#	content(r)$result[i][[1]]$summary_description)
			#print(critic)
		}
	}
}

P=data.frame(MovieId=c(0), Value=c(0),Comment=c(0))
j=1
while(j<length(critics)){
	P=rbind(P, c(critics[j], critics[j+1], critics[j+2]))
	j=j+3	
}

write.csv(P,"ReviewList.csv", row.names=FALSE)