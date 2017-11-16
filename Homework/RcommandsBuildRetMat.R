library(quantmod)

# Read in all of the NYSE ETF symbols
# This was obtained from 
#    http://www.investorpoint.com/exchange/NYE-New+York+Stock+Exchange/etf/

nyse_etfs = read.table("http://www.stat.cmu.edu/~cschafer/MSCF/NYSEETFlist.txt", sep="\t")


# Build the matrix of returns

startdate = "2016-1-1"
enddate = "2016-12-31"

# Get the data for the stock under consideration

stockdata = getSymbols("MSFT", auto.assign=FALSE, from=startdate, to=enddate)
y = dailyReturn(stockdata[,6])

retmat = NULL
namelist = NULL

pos = 1
for(etf in as.character(nyse_etfs$V1))
{
  holdout = try(getSymbols(etf, auto.assign=FALSE, from=startdate, to=enddate), TRUE)
  if("try-error" %in% class(holdout))
  {
    next
  }
  
  if(ndays(holdout) != ndays(stockdata))
  {
    next
  }
  
  retmat = cbind(retmat, dailyReturn(holdout[,6]))
  namelist = c(namelist,etf) 
  pos = pos + 1
  print(pos)
}

retmat = data.frame(retmat)
names(retmat) = namelist

save(retmat,file="retmat.Robj")