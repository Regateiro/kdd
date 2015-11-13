#install.packages("httr")
library(httr)

# URL and API keys for Import.IO
url = "http://api.import.io/store/data/66ff6f91-93ef-4f86-8307-ba7e6dcbc628/_query?input/webpage/url=http%3A%2F%2Fwww.imdb.com%2Ftitle%2F"
url2 = "%2Fcriticreviews%3Fref_%3Dtt_ov_rt&_user=4911226d-82c8-4f2f-9e06-e04bffad812e&_apikey=4911226d82c84f2f9e06e04bffad812e3ee3dd322ae4d7309f3751ce6913e2da924f27aabd67c1a8d8aee488dc392b80c9de4885cd3d16c6d630151818526d3b5b50b3d57bcf816bfa015eeeb4989a1f"

# IMDB IDs (currently just jurassic world)
ids = c("tt0369610")

# HTTP Request
r <- GET(paste(url, ids[1], url2, sep=""))
results = content(r)$result
for(i in 1:length(results)){
	critic = c(content(r)$result[i][[1]]$critscore_number,
		content(r)$result[i][[1]]$summary_description)
	print(critic)
}