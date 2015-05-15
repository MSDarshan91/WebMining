library(deepnet)
library(xts)
library(dygraphs)
library(sqldf)
library(RSNNS)
setwd(<Working Directory>)
  load("OneYearTweets.Rdata")
URL <- "http://ichart.finance.yahoo.com/table.csv?s=^nsei&a=4&b=15&c=2014&d=02&e=1&f=2015"
dat <- read.csv(URL)
tim = dat$Date
tim <- as.Date(as.character(tim),format="%Y-%m-%d")
tim = rev(tim)
tim = tim[-c(1:4)]

d = dat$Close
createMatrix <-function(data,k)
 {
	l = length(data)
	mat = data.frame(1:(l-k+1))
	for( i in 1:k)
	{
		li = 1:(k-1)
		for( j in 1:k-1)
		{	
			li[j] = (l - li[j])
			if(j < i)
				li[j] = j
		}
		d = data[-li]
		mat = cbind(mat,d)
	}
	mat = as.matrix(mat)
	mat = mat[,-1]
	n = NULL
	for( i in 1:k)
	{	
		n = c(n,paste("tim",k-i+1,sep = "_"))	
	}
	colnames(mat) = n
	return (mat)
 }
  zNorm_set<-function(data,window)
 {
	data = (data-mean(data))/sd(data)
	return (data)
 }
 
 zNorm_data<-function(data,window)
 {
	l = length(data)
	no_sets = ceiling(l/window)
	for( i in 0:(no_sets-2))
		data[ (i*window )+1 :((i+1)*window )] = zNorm_set(data[ (i*window )+1:((i+1)*window )])
	data[( ((no_sets-1)*window )+1) :l] = zNorm_set(data[( ((no_sets-1)*window )+1) :l])
	return (data)
 }
 #d = zNorm_data(d,length(d))
 k =4
 d = zNorm_set(d,length(d)) 
 inp = createMatrix(d,k)
 
 y = inp[,1]
 y=y[-1]
 inp = inp [-dim(inp)[1],]
  sent=sqldf("SELECT sum(sentiment)/count(*) as a, date as d FROM tweets GROUP BY date")
  n = NULL
	for( i in 1:k)
	{	
		n = c(n,paste("tim",k-i+1,sep = "_"))	
	}
	
	checkAccuracy <- function(y,yy)
	{
		acc = 0
		for( i in 2:length(y))
		if((y[i] > y[i-1]) && (yy[i] > yy[i-1])) {
			acc= acc+1 } else if ((y[i] < y[i-1]) && (yy[i] < yy[i-1])) {
			acc= acc+1 } 
		acc = acc/length(y)
		return (acc)	
	}
p_t = tim-1
	x= as.data.frame(cbind(p_t = p_t,inp))
	
	sqlQuery = paste("SELECT a,",	paste(n,collapse =",")," FROM x LEFT JOIN sent on (x.p_t) = sent.d",sep='')
inputs = sqldf(sqlQuery)	
# With Sentiments
		print("NN")
	  nn <- nn.train(as.matrix(inputs[1:100,]), y[1:100], hidden = c(15))

	yy <- nn.predict(nn, inputs[1:dim(inputs)[1],])
	actual <- xts(y,tim)
	predicted <- xts(yy,tim)
	comb = cbind(actual =  actual,predicted = predicted)
	  dygraph(comb, main = "Actual Vs Predicted") %>%
	  dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2"),strokeWidth= 6)  %>% dySeries("predicted", axis = 'y2')  %>%  dyShading(from = "2014-05-16", to = "2014-11-14")
	  checkAccuracy(y[1:100],yy[1:100])
	  checkAccuracy(y[101:140],yy[101:140])
	  checkAccuracy(y[141:length(y)],yy[141:length(yy)])
	  
	  print("Elman")
	  modelElman <- elman(as.matrix(inputs[1:100,]), y[1:100], size = c(15))

	yy <- predict(modelElman, inputs[1:dim(inputs)[1],])
	actual <- xts(y,tim)
	predicted <- xts(yy,tim)
	comb = cbind(actual =  actual,predicted = predicted)

	  dygraph(comb, main = "Actual Vs Predicted") %>%
	  dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2"),strokeWidth= 6)  %>% dySeries("predicted", axis = 'y2')  %>%  dyShading(from = "2014-05-16", to = "2014-11-14")
	  checkAccuracy(y[1:100],yy[1:100])
	  checkAccuracy(y[101:140],yy[101:140])
	  checkAccuracy(y[141:length(y)],yy[141:length(yy)])
	  print("Jordan")
	modelJordan <- jordan(as.matrix(inputs[1:100,]), y[1:100], size = c(15))

	yy <- predict(modelJordan, inputs[1:dim(inputs)[1],])
	actual <- xts(y,tim)
	predicted <- xts(yy,tim)
	predicted_jordan = predicted
	comb = cbind(actual =  actual,predicted = predicted)

	  dygraph(comb, main = "Actual Vs Predicted") %>%
	  dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2"),strokeWidth= 6)  %>% dySeries("predicted", axis = 'y2')  %>%  dyShading(from = "2014-05-16", to = "2014-11-14")
	checkAccuracy(y[1:100],yy[1:100])
	checkAccuracy(y[101:140],yy[101:140])
    checkAccuracy(y[141:length(y)],yy[141:length(yy)])

# Without Sentiments	
inputs = inp

	 print("NN")
	  nn <- nn.train(as.matrix(inputs[1:100,]), y[1:100], hidden = c(15))

	yy <- nn.predict(nn, inputs[1:dim(inputs)[1],])
	actual <- xts(y,tim)
	predicted <- xts(yy,tim)
	comb = cbind(actual =  actual,predicted = predicted)
	  dygraph(comb, main = "Actual Vs Predicted") %>%
	  dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2"),strokeWidth= 6)  %>% dySeries("predicted",  axis = 'y2')  %>%  dyShading(from = "2014-05-16", to = "2014-11-14")
	checkAccuracy(y[1:100],yy[1:100])
	checkAccuracy(y[101:140],yy[101:140])
    checkAccuracy(y[141:length(y)],yy[141:length(yy)])
	  print("Elman")
	  modelElman <- elman(as.matrix(inputs[1:100,]), y[1:100], size = c(15))
	yy <- predict(modelElman, inputs[1:dim(inputs)[1],])
	actual <- xts(y,tim)
	predicted <- xts(yy,tim)
	comb = cbind(actual =  actual,predicted = predicted)
	  dygraph(comb, main = "Actual Vs Predicted") %>%
	  dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2"),strokeWidth= 6)  %>% dySeries("predicted", axis = 'y2')  %>%  dyShading(from = "2014-05-16", to = "2014-11-14")
	checkAccuracy(y[1:100],yy[1:100])
	checkAccuracy(y[101:140],yy[101:140])
    checkAccuracy(y[141:length(y)],yy[141:length(yy)])
	  print("Jordan")
	modelJordan <- jordan(as.matrix(inputs[1:100,]), y[1:100], size = c(15))
	yy <- predict(modelJordan, inputs[1:dim(inputs)[1],])
	actual <- xts(y,tim)
	predicted <- xts(yy,tim)
	comb = cbind(actual =  actual,predicted_WO = predicted,predicted_W = predicted)
	  dygraph(comb, main = "Actual Vs Predicted") %>%
	  dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2"),strokeWidth= 3)  %>% dySeries("predicted_WO",  axis = 'y2') %>% dySeries("predicted_W",  axis = 'y2')  %>%  dyShading(from = "2014-05-16", to = "2014-11-14")
	checkAccuracy(y[1:100],yy[1:100])
	checkAccuracy(y[101:140],yy[101:140])
    checkAccuracy(y[141:length(y)],yy[141:length(yy)])
