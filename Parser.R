setwd("C:\\Users\\Avva_2\\Desktop\\WebMining")
library(RSelenium)
library(qdap)
RSelenium::checkForServer()

## Start selenium remote browser (Firefox will come live)
RSelenium::startServer()
remDr <- remoteDriver(remoteServerAddr = "localhost" 
                      , port = 4444
                      , browserName = "firefox"
)
remDr$open()
tweets = data.frame(text=character(),
					date=character(),
                 sentiment=numeric(), 
                 count=numeric(), 
                 stringsAsFactors=FALSE) 
#queryTerms = scan('SearchTerms.txt', what='character',sep="\n")
queryTerms = "Indian Cricket Team"
finalDate = as.Date("1april2015", "%d%b%Y")
preProcess <- function(tweet){
 tweet = gsub("[^[:alpha:][:space:]']", "", tweet)
return(tweet)
}
for(index in 1: length(queryTerms))
{
	mindate = as.Date("1december2014", "%d%b%Y")
	maxdate = mindate+1
	while(mindate != finalDate)
	{
		for( j in 1 : 10)
		{
			tryCatch(
			{	
				url = paste("http://topsy.com/s?q=",queryTerms[index],"&offset=",j*10,"&mintime=",as.numeric(as.POSIXct(mindate)),"&maxtime=",as.numeric(as.POSIXct(maxdate)),sep="")
				#remDr$navigate("http://topsy.com/s?q=modi&offset=0&mintime=1425132057&maxtime=1425218456")
				remDr$navigate(url)
				webElem <- remDr$findElements(using = 'xpath', "//div[@class='media-body']")
				resHeaders <- unlist(lapply(webElem, function(x){x$getElementText()}))	  
				x = strsplit(resHeaders, "\\n")
				x = unique(x)
				for( i in 1:length(x))
				{
					z = unique(na.omit(as.numeric(unlist(strsplit(unlist(x[[i]][3]), "[^0-9]+")))))
					tex = preProcess(x[[i]][2])
					tweets = rbind(tweets,data.frame(text=x[[i]][2],sentiment= polarity(tex)$all$polarity, count = z[2],date = mindate))
				}
			},	
			error = function(cond) {
				 message(cond)
			}
			)
		}
		mindate = mindate+1
		maxdate = maxdate+1
	}
}
#save(tweets, file="OneYearTweets.Rdata")
save(tweets, file="IndianCricketTeam.Rdata")
