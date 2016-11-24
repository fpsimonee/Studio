
wdir = "/Volumes/500GB/RStudio/FinancialAnalytics"
setwd(wdir)
library(quantmod); library(xts)
getSymbols("AAPL",src='yahoo')
saveRDS(AAPL, file="Apple.rds")
Apple = readRDS("Apple.rds")

library("quantmod")
##retrieve historical price data for General Electric Co. from Yahoo Finance
getSymbols('GE',src='yahoo', from="2000-01-01", to="2009-12-30")
##to see headers of file (OHLCV type)
names(GE)
## extract Adjusted Close
geAdj = GE$GE.Adjusted["2000-01-01/2000-01-20"]
##compute max, min and mean
max(geAdj); min(geAdj); mean(geAdj)
## draw a chart
chartSeries(GE)

##to save data to your disk use
saveRDS(GE,file="GE.rds")


library("quantmod")
symbols=c('^VLIC','GE','KO','AAPL','MCD')
getSymbols(symbols,src='yahoo',from="2012-02-01",to="2013-02-01")
#obtain adjusted closed
VLICad = VLIC$VLIC.Adjusted; GEad= GE$GE.Adjusted;
KOad=KO$KO.Adjusted; AAPLad=AAPL$AAPL.Adjusted;
MCDad = MCD$MCD.Adjusted
#compute cumulative sum (cumsum) of daily returns (Delt)
#Remove first term of the series, with [-1,], since cumsum is not defined for it.
vl = cumsum((Delt(VLICad)*100)[-1,])
ge = cumsum((Delt(GEad)*100)[-1,])
ko = cumsum((Delt(KOad)*100)[-1,])
ap = cumsum((Delt(AAPLad)*100)[-1,])
md = cumsum((Delt(MCDad)*100)[-1,])

###range of values for the plot
lim = c(min(vl,ge,ko,ap,md),max(vl,ge,ko,ap,md))
###the plot
plot(vl,main="",ylim=lim,xlab="dates",ylab="% benefits")
lines(ge,col="green"); lines(ko,col="red")
lines(ap,col="violet"); lines(md,col="yellow")
legend(x="topleft",cex=0.4,c("VLIC","GE","KO","AAPL","MCD"),
       lty=1, col=c("black","green","red","violet","yellow"))



