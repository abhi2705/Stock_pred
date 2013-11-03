library('quantmod')
getSymbols("GOOG")
chartSeries(GOOG, subset='last 3 months')
addBBands()

library(tseries)
t.ibm <- get.hist.quote("IBM",start="2000-01-01",end="2013-03-07", quote=c("Open", "High", "Low", "Close","Volume"))
plot(t.ibm,main="IBM stock")

t.goog <- get.hist.quote("GOOG",start="2000-01-01",end="2013-03-07", quote=c("Open", "High", "Low", "Close","Volume"))
plot(t.goog,main="GOOGLE stock")

// load df.get.hist.quote.R

ibm <- df.get.hist.quote("IBM",start="2000-01-01",end="2013-10-5")
ts(rnorm(25), frequency = 12, start = c(2012, 4))

rm(t.ibm)
ibm <- df.get.hist.quote("IBM",start="2000-01-01",end="2013-10-5")
ibm[c(1,nrow(ibm)),]

goog <- df.get.hist.quote("GOOG",start="2000-01-01",end="2013-07-3")
ts(rnorm(25), frequency = 12, start = c(2012, 4))

rm(t.goog)
goog <- df.get.hist.quote("GOOG",start="2000-01-01",end="2013-07-1")
goog[c(1,nrow(goog)),]

h.returns <- function(x,h=1) {
 diff(x,lag=h)/x[1:(length(x)-h)]
 }
h.returns(c(45,23,4,56,-45,3),h=2)

library(xts)
embed(c(45,677,34,2,-76,-23,0,45,-3445),dim=3)


embeded.dataset <- function(data,quote='Close',hday=1,emb=10) {
ds <- data.frame(embed(h.returns(data[,quote],h=hday),emb+hday))
 ds <- ds[,c(1,(1+hday):(hday+emb))]
 names(ds) <- c(paste('r',hday,'.f',hday,sep=""),
 paste('r',hday,'.t',0:(emb-1),sep=""))
ds$Date <- data[(hday+emb):(nrow(data)-hday),'Date']
 ds
 }
ibm.data <- embeded.dataset(ibm,hday=1)
names(ibm.data)
ibm.data[1:2,]


ibm.train <- ibm.data[ibm.data$Date < '2013-06-20',]
ibm.test <- ibm.data[ibm.data$Date > '2013-06-20',]

library(nnet)
nn <- nnet(r1.f1 ~ .,data=ibm.train[,-ncol(ibm.train)],
 linout=T,size=10,decay=0.01,maxit=1000)

summary(nn)
nn.preds <- predict(nn,ibm.test)

plot(ibm.test[,1],nn.preds,ylim=c(-0.01,0.01),
 main='Neural Net Results',xlab='True',ylab='NN predictions')

abline(h=0,v=0); abline(0,1,lty=2)

