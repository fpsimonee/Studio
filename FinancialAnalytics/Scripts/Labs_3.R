# set your working directory with setwd()
wdir="/Users/felipe/Documents/RStudio/FinancialAnalytics"
setwd(wdir)
library(fBasics)


##Load to your workspace the data frame  daxRlog, containing 251 daily log-returns 
##(for the period 06/01/2009 - 30/12/2009) of  ALV, BMW,  CBK and  TKA, 
##stocks from the Frankfurt Stocks Exchange.
##Compute correlations with method "pearson" and "kendall".

##Load the data frame with  Allianz (ALV.DE), BMW (BMW.DE),
## Commerzbank (CBK.DE), Thyssenkrupp (TKA.DE)
daxRlog = read.table(paste(wdir,"/data/daxRlog",sep=""), header=T)

##Pearson rho correlation
cor(na.omit(daxRlog[,2:5]),method="pearson")
##Kendall tau correlation
cor(na.omit(daxRlog[,2:5]),method="kendall")

##plot the series for each company and observe for the correlations in the plot
date=as.Date(daxRlog$dax.date[1:(length(daxRlog$dax.date)-1)])
#omit last entry in date since there is only NA for logreturns (inspect the data)
plot(date,na.omit(daxRlog$alvR), type="l",main="ALV, BMW, CBK,TKA : 06/01/2009 - 30/12/2009",xlab="dates",ylab="returns") ##plot 1
lines(date,na.omit(daxRlog$tkaR), type="l",col="green") #TKA
lines(date,na.omit(daxRlog$bmwR), type="l",col="red") #BMW
lines(date,na.omit(daxRlog$cbkR), type="l",col="blue") #CBK


library("lmtest") ##load required library for grangertest
library("AER") ##Econometric data
data("USMoney") ##from AER
##A quarterly multiple time series from 1950 to 1983 with 3 variables: gnp, m1, deflator

##To know if m1 causes gnp
grangertest(gnp~m1, order=3,data=USMoney)
##To know if gnp causes m1
grangertest(m1~gnp, order=3,data=USMoney)

##alternative (to last test)
grangertest(USMoney[,1],USMoney[,2],order=3)
##gnp is in column 1, m1 in column 2, so it reads: gnp causes m1

## Clustering 
IBEX<-read.table(paste(wdir,"/data/Ibex0809",sep=""),header=T)
dd <-as.dist(2*(1-cor(IBEX)))
met="ward.D2" ##  complete,single,average,median
hc <-hclust(dd,method=met)
plot(hc,main=paste(met," method"),axes=TRUE,xlab="",sub="")

#compute the cut  at mean height K:
l<-length(hc$height); hh<-sort(hc$height); K<-mean(hh[1:l])
abline(h=K,lty=2,lwd=2) ##draw the cut
#branches below K make clusters, above go to singletons
groups <- cutree(hc, h = K)  ##obtain  clusters
numgp <- max(groups) #number of clusters.
#extract the names of each group and convert to list
W <- list(names(groups[groups==1]))
##recursively concatenate lists
for (i in 2:numgp){W <- c(W,list(names(groups[groups==i])))}
W



## K-means
k=12; ##number of clusters 
kc=kmeans(t(IBEX),k)
##note: transpose IBEX since kmeans works by rows
groups <- kc$cluster  ##obtain  clusters
numgp <- max(groups) #number of clusters. 
#extract the names of each group and convert to list
W <- list(names(groups[groups==1]))
for (i in 2:numgp){W <- c(W,list(names(groups[groups==i])))}
W
