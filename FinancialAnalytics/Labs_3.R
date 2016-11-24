# set your working directory with setwd()
wdir="your-working-directory"
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