library(googleVis)
setwd(<Working Directory>)
  load("OneYearTweets.Rdata")
  URL <- "http://ichart.finance.yahoo.com/table.csv?s=^nsei&a=4&b=15&c=2014&d=02&e=1&f=2015"
dat <- read.csv(URL)
tim = dat$Date
t =tim
p_t = t-1
x= as.data.frame(cbind(p_t = p_t,inp))
	 sent=sqldf("SELECT sum(sentiment)/count(*) as a, date as d FROM tweets GROUP BY date")
	sqlQuery = paste("SELECT a,",	paste(n,collapse =",")," FROM x LEFT JOIN sent on (x.p_t) = sent.d",sep='')
inputs = sqldf(sqlQuery)	

	sentiments = inputs$a
	sentiments[1] = 1
	stock_prices = y
	stock_prices[1] = 1
	for(  i in 2:length(t))
	{
		if(((y[i]-y[i-1])/y[i-1]) > 0.1){
		stock_prices[i] = 1} else if(((y[i]-y[i-1])/y[i-1]) < -0.1  ) {
		stock_prices[i] = -1	}else { stock_prices[i] = 0 }
		sentiments[i] = ifelse(inputs$a[i]>0, 1 , -1 )
	}
	coRRelation = sentiments * stock_prices
	
	df_2 = data.frame(x = t,y = coRRelation)
	Line3 <- gvisLineChart(df_2, "x", "y")
	plot(Line3)  
	
	sum(coRRelation > 0)
	print("Hello")
	df = data.frame(Date = t,
	sent = inputs$a, stock = y)
	Line2 <- gvisLineChart(df, "Date", c("sent","stock"),
						   options=list(
							 series="[{targetAxisIndex: 0},
									 {targetAxisIndex:1}]",
							 vAxes="[{title:'Sentiments'}, {title:'Stock Prices'}]",
							 width=700, height=500
						   ))
	plot(Line2)




x= as.data.frame(cbind(p_t = t,inp))
	
	sqlQuery = paste("SELECT a,",	paste(n,collapse =",")," FROM x LEFT JOIN sent on (x.p_t) = sent.d",sep='')
inputs = sqldf(sqlQuery)	
	sentiments = inputs$a
	sentiments[1] = 1
	stock_prices = y
	stock_prices[1] = 1
	for(  i in 2:length(t))
	{
		if(((y[i]-y[i-1])/y[i-1]) > 0.1){
		stock_prices[i] = 1} else if(((y[i]-y[i-1])/y[i-1]) < -0.1  ) {
		stock_prices[i] = -1	}else { stock_prices[i] = 0 }
		sentiments[i] = ifelse(inputs$a[i]>0, 1 , -1 )
	}
	coRRelation = sentiments * stock_prices
	
	df_2 = data.frame(x = t,y = coRRelation)
	Line3 <- gvisLineChart(df_2, "x", "y")
	plot(Line3)  
	print("Hello")
	df = data.frame(Date = t,
	sent = inputs$a, stock = y)
	Line2 <- gvisLineChart(df, "Date", c("sent","stock"),
						   options=list(
							 series="[{targetAxisIndex: 0},
									 {targetAxisIndex:1}]",
							 vAxes="[{title:'Sentiments'}, {title:'Stock Prices'}]",
							 width=700, height=500
						   ))
	plot(Line2)

