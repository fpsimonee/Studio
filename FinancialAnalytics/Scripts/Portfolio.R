## Portifolio 

library(Metrics)
names(SMALLCAP)  #to see the contents of the data set
Data = SMALLCAP.RET #to get returns 
Data = Data[, c("BKE","FCEL","GG","OII","SEB")]

##Compute the sample mean of the returns of each stock 
## and the sample covariance matrix.
covData <- covEstimator(Data) ; covData

## Compute the unrestricted (long-short) Mean-Variance (MV) portfolio
shortSpec <- portfolioSpec()
setSolver(shortSpec) <- "solveRshortExact"
shortFrontier <- portfolioFrontier(Data,spec=shortSpec,
                                   constraints="Short")
print(shortFrontier) #report results for portfolio:1,13,25,37,50

##Plot the Efficient Frontier
Frontier <- shortFrontier  
frontierPlot(Frontier,frontier="both",risk="Sigma",type="l")

## Plot some portfolios 
minvariancePoints(Frontier,pch=19,col="red") #the MVP point
##Position of each asset in the sigma-mu plane
singleAssetPoints(Frontier,risk="Sigma",pch=19,cex=1.5,
                  col=topo.colors(6))

## To compute the minimum variance portfolio (MVP), 
## and a particular efficient portfolio for a given target return
##MVP: the minimum Variance. 
minvariancePortfolio(Data)
##EP(mu): an efficient portfolio for given target return
mu = 0.05; Spec = portfolioSpec()
setSolver(Spec) = "solveRshortExact"
setTargetReturn(Spec) = mu 
efficientPortfolio(Data, Spec) 

##To compute the global maximum return portfolio we use the 
##linear programming solver lp to resolve the optimization
## problem for a vector  of 5 unknown weights.
##MaxR: Global maximum return portfolio 
## maximize: (w1,w2,w3,w4,w5)*covData$mu
## subject to: w1+w2+w3+w4+w5 = 1
##Use the linear programming solver lp from lpSolve:
f.obj <- covData$mu
f.con <- matrix(c(1,1,1,1,1), nrow=1, byrow=TRUE)
f.dir <- "="
f.rhs <- 1
lp ("max", f.obj, f.con, f.dir, f.rhs)
lp ("max", f.obj, f.con, f.dir, f.rhs)$solution

## portfolio containing only FCEL