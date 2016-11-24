## font: http://computationalfinance.lsi.upc.edu/?page_id=108

# Fiting an exponencial curve to the DJIA from 1978 to 2001
# The data is retrieved directly from Federal Reserve. 


require(quantmod); getSymbols("DJIA",src="FRED")
serie=DJIA["1978/2001"]
price=as.numeric(serie) #extract numeric values of price
time = index(serie) #extract the indices
x=1:length(price)
model=lm(log(price)~x)
expo=exp(model$coef[1]+model$coef[2]*x)
plot(x=time,y=price, main="Dow Jones",type="l")
lines(time,expo,col=2,lwd=2)



####An R demonstration for  doing some descriptive statistics 
## of financial returns. We  work with financial data from  
# Allianz (ALV), Bayerische Motoren Werke (BMW),
## Commerzbank (CBK) and Thyssenkrupp (TKA), all German business
## trading in the Frankfurt Stock Exchange and listed in 
## the main index DAX


### Part I: preprocessing the data ##########
wdir="path-to-your-working-directory"; setwd(wdir)
# load the financial data from wdir
ALV = read.csv(paste(wdir,"/ALV.csv", sep=""), header=T)
# extract 1 year of data from the AdjClose column
alvAC= ALV$AdjClose[1:252]
## repeat the previous instructions with BMW, CBK, TKA.
date= ALV$Date[1:252] # extract the column Date
date ##put all together into a data.frame
dax =data.frame(date,alvAC,bmwAC,cbkAC,tkaAC)

# plot Adjusted prices vs date for ALV
plot(dax$date,dax$alvAC, type="l",main="ALV.DE", xlab="dates",ylab="adj. close")

# Compute Returns. First define vectors of appropriate length
alvR for (i in 1:252){alvR[i] <-(alvAC[i]/alvAC[i+1]) -1 }
#same with bmwR, cbkR, tkaR
# Remember dates are ordered descending. Make table Returns
daxR =data.frame(dax$date,alvR,bmwR,cbkR,tkaR)
# Compute log returns (omit  column of dates)
daxRlog #plot returns and log returns (in red) and see coincidences:
plot(dax$date,daxR$alvR, type="l",xlab="dates",ylab="returns")
lines(dax$date,daxRlog$alvR, type="l",col="red")

#### Part II: Basic statistics ############
library(fBasics) ## load the library "fBasics"
basicStats(daxRlog$alvR)
## You can compute basic stats to a full data frame,
## omitting non numeric data
basicStats(na.omit(daxRlog[,2:5]))
##Use a boxplot to help visualising and interpret results
boxplot(daxRlog[,2:5])
##compute covariance matrix
cov(daxRlog[,2:5],use="complete.obs")

####Extras: To save your table in your working directory
write.table(dax,file="dax") ## or  as .csv use write.csv
##To read the data saved in working directory
dax = read.table("dax", header=T)


### Computing quartiles for the series of returns of Allianz 
## (ALV::Frankfurt) in period 06/01/2009 - 30/12/2009.

alv=na.omit(daxR$alvR)
quantile(alv,probs=c(0,1,0.25,0.5,0.75))


#### Histogram of ALV returns from 06/01/2009 - 30/12/2009, 
## with an estimate of its density from sample data (solid line),
## and adjusted normal distribution (dashed line).


alv=na.omit(daxR$alvR); DS = density(alv)
yl=c(min(DS$y),max(DS$y)) #set y limits
hist(alv,probability=T,xlab="ALV returns", main=NULL,ylim=yl)
rug(alv); lines(DS); a=seq(min(alv),max(alv),0.001)
points(a,dnorm(a,mean(alv),sd(alv)), type="l",lty=2)
# if you rather have a red line for the normal distribution do:
lines(a,dnorm(a,mean(alv), sd(alv)),col="red")

## Explore the possibility of aggregational normality in 
## stock returns (i.e. as the time scale increases - daily, 
## weekly and so on, the distribution of returns looks more 
## like a normal distribution).

require(quantmod)
appl = getSymbols("AAPL",src="yahoo")
apRd= periodReturn(appl,period="daily",type="log")
dsd=density(apRd) #estimate density of daily log ret
yl=c(min(dsd$y),max(dsd$y)) #set y limits
plot(dsd,main=NULL,ylim=yl)
##plot the normal density with mean, stdv of apRd
a=seq(min(apRd),max(apRd),0.001)
points(a,dnorm(a,mean(apRd),sd(apRd)), type="l",lty=2)

##Repeat above with period="weekly", "monthly".





