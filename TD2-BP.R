
library(jsonlite)

exchangeA = fromJSON(txt = "https://api.kraken.com/0/public/Assets")
names(exchangeA$result)

exchangeB = fromJSON(txt = "https://poloniex.com/public?command=returnTicker")
names(exchangeB)

exchangeB$BTC_BCN$lowestAsk
exchangeB$BTC_BCN$highestBid


askB = exchangeB$USDT_XMR$lowestAsk
bidB = exchangeB$USDT_XMR$highestBid

pairKraken = fromJSON(txt = "https://api.kraken.com/0/public/Ticker?pair=xmreur%2Cxmrusd&fbclid=IwAR2VsUz3lRce1418ACZWfUwCOVKNOgORPkq-1odkDbb5CM5AshWUiQJsLJ4")

askA = pairKraken$result$XXMRZUSD$a[1]
bidA = pairKraken$result$XXMRZUSD$b[1]


max(bidA,askB)
max(bidB,askA)

B = FALSE
dif = as.numeric(bidA) - as.numeric(askB)

if(dif > 0)
{
  B==TRUE
}
B

if(B==FALSE)
{
  print("Il n'y a pas d'opportunité")
}
if(B== TRUE)
{
  print("Il y'a une opportunité")
}

dif2 = as.numeric(bidB) - as.numeric(askA)
A = FALSE
if(dif2 > 0)
{
  A==TRUE
}
A

if(A==FALSE)
{
  print("Il n'y a pas d'opportunité")
}
if(A== TRUE)
{
  print("Il y'a une opportunité")
}

opportunité = function(bid,ask)
{
  B = FALSE
  dif = as.numeric(bid) - as.numeric(ask)
  
  if(dif > 0)
  {
    B==TRUE
  }
  B
  
  if(B==FALSE)
  {
    print("Il n'y a pas d'opportunité")
  }
  if(B== TRUE)
  {
    print("Il y'a une opportunité")
  }
  return(B)
}

opportunité(bidA,askB)
opportunité(bidB,askA)


i=0
n = 30
while(i<=n){
  
  
  exchange1 = fromJSON(txt = "https://api.kraken.com/0/public/Assets")
  #names(exchange1$result)
  
  exchange2 = fromJSON(txt = "https://poloniex.com/public?command=returnTicker")
  #names(exchange2)
  
  exchange2$BTC_BCN$lowestAsk
  exchange2$BTC_BCN$highestBid
  
  
  ask1 = exchangeB$USDT_XMR$lowestAsk
  bidB = exchangeB$USDT_XMR$highestBid
  
  pairKraken = fromJSON(txt = "https://api.kraken.com/0/public/Ticker?pair=xmreur%2Cxmrusd&fbclid=IwAR2VsUz3lRce1418ACZWfUwCOVKNOgORPkq-1odkDbb5CM5AshWUiQJsLJ4")
  
  ask1 = pairKraken$result$XXMRZUSD$a[1]
  bid1 = pairKraken$result$XXMRZUSD$b[1]
  
  opportunité(bidA,askB)
  opportunité(bidB,askA)
  
  ##On désincrémente la variable i pour qu'elle ne soit jamais supérieur a n
  i=i-30
}

#Historicaldata
h_data_exchangeA = fromJSON(txt = "https://api.kraken.com/0/public/OHLC?pair=xmrusd&interval=1440")


#CALCUL SMA 20
SMA_20 = function()
{
  n = 20
  result = 0
  for (i in c(1:n))
  {
    result = (result + as.numeric(h_data_exchangeA$result$XXMRZUSD[i,5]))
  }
  return(result/n)
}

SMA_20()

#CALCUL EMA
EMA_50 = function()
{
n = 50
multiplier = (2/(n+1))
ema = matrix(nrow = n, ncol = 1)
ema[1,] = (as.numeric(h_data_exchangeA$result$XXMRZUSD[1,5]))*multiplier
for (i in c(2:n))
  {
  
    ema[i,] = (as.numeric(h_data_exchangeA$result$XXMRZUSD[1,5])-ema[i-1,])*multiplier+ema[i-1,]
    
  }
return(ema[50,])

}

EMA_50()


#CALCUL SMA x
XSMA = function(n)
{
  result = 0
  for (i in c(1:n))
  {
    result = ((result + as.numeric(h_data_exchangeA$result$XXMRZUSD[i,5])))
  }
  return(result/n)
}

XSMA(20)


#CALCUL XEMA
XEMA = function(n)
{
  
  multiplier = (2/(n+1))
  ema = matrix(nrow = n, ncol = 1)
  ema[1,] = (as.numeric(h_data_exchangeA$result$XXMRZUSD[1,5]))*multiplier
  for (i in c(2:n))
  {
    
    ema[i,] = (as.numeric(h_data_exchangeA$result$XXMRZUSD[1,5])-ema[i-1,])*multiplier+ema[i-1,]
    
  }
  return(ema[n,])
  
}

XEMA(50)

#CALCUL MACD

MACD = XEMA(12)-XEMA(26)
MACD

#CALCUL RSI
n=14
return_all = matrix(nrow = n, ncol = 1)
return_gain = matrix(nrow = n, ncol=1)
return_loss = matrix(nrow = n, ncol=1)
result = 0
for(i in c(1:n))
{
  return_all[i,] = ((as.numeric(h_data_exchangeA$result$XXMRZUSD[i+1,5])- as.numeric(h_data_exchangeA$result$XXMRZUSD[i,5]))/as.numeric(h_data_exchangeA$result$XXMRZUSD[i,5]))
  if (return_all[i,]<0)
  {
    return_loss[i,] =return_all[i,] 
  }
  if (return_all[i,]>0)
  {
    return_gain[i,] = return_all[i,]
  }
}

# average_gain = function()
# {
#   n= 14
#   result = 0
#   for (i in c(1:n))
#   {
#     
#     result = (result + as.numeric(return_gain[i,]))
#     
#   }
#   return(result/8)
#   
# }
# 
# average_loss = function()
# {
#   n= 14
#   result = 0
#   for (i in c(1:n))
#   {
#     
#     result = (result + as.numeric(return_loss[i,]))
#     
#   }
#   return(result/6)
#   
# }

average_gain = (as.numeric(return_gain[1,]) + as.numeric(return_gain[3,]) + as.numeric(return_gain[4,]) + as.numeric(return_gain[7,]) + as.numeric(return_gain[8,]) + as.numeric(return_gain[10,]) + as.numeric(return_gain[11,]) + as.numeric(return_gain[14,]))/8

average_loss = (as.numeric(return_loss[2,]) + as.numeric(return_loss[5,]) + as.numeric(return_loss[6,]) + as.numeric(return_loss[9,]) + as.numeric(return_loss[12,]) + as.numeric(return_loss[13,]))/6

RSI = 100 - (100/(1+(average_gain/average_loss)))
RSI
