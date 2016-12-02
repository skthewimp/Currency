
probsExp <- function(maxamount, meanamount) 
  # assign probabilities to a bill being of the amount <maxamount>, assuming that prices follow an 
  # exponential distribution with mean <meanamount>
{
  lbounds <- pexp(1:maxamount-0.5,1/meanamount)
  ubounds <- pexp(1:maxamount+0.5,1/meanamount)
  probs <- ubounds - lbounds
  probs <- probs/sum(probs)
  return(probs)
}

probsPowerLaw <- function(maxamount, meanamount, threshold, alpha) 
# assume that the amount follows a power law distribution with lower threshold <threshold> and exponent
# alpha. The average value is meanamount. This returns probabilities of every rupee value between threshold and
# maxamount
  
{
  lbounds <- 1 -  ((threshold:maxamount - 0.5)/threshold)^(-alpha)
  ubounds <- 1 -  ((threshold:maxamount + 0.5)/threshold)^(-alpha)
  probs <- ubounds - lbounds
  probs <- probs/sum(probs)
  return(probs)
}

distrib <- function(maxamount, meantrans, coins, alpha)
  # given a maximum amount to transact, and a mean transaction value, calculate the relative use of 
  # different currencies
{
  threshold <- round(meantrans * (alpha-1)/alpha)
  print(threshold)
  amt <- mincoinsDijkstra(maxamount, coins) # number of coins for each transaction amount
  amt <- amt[amt$Amount <= maxamount & amt$Amount >= threshold,]
  amt$Prob <- probsPowerLaw(maxamount,meantrans,threshold,alpha) # assume an exponential distribution in probaibility
  
  # next few lines of code are to determine the number of coins of each type in the optimal path for each
  # amount 
  combos <- sapply(amt$Path, function(x) table(as.numeric(unlist(strsplit(x,'\\,')))))
  combos <- lapply(combos, function(x) as.data.frame(x))
  combos <- Map(cbind,combos,amt$Prob)
  combos <- lapply(combos,setNames,c("Coin","Freq","Prob"))
  
  allcoins <- do.call(rbind,combos)
  allcoins$Totprob <- allcoins$Freq * allcoins$Prob
  
  allcoins$Coin <- as.numeric(as.character(allcoins$Coin)) # when table, it would've become a factor
  allcoins$Coin <- abs(allcoins$Coin) # doesn't matter if it's given or taken
  
  bycoin <- aggregate(Totprob~Coin,allcoins,sum) # total probability of a coin being picked
  bycoin$Totprob <- bycoin$Totprob/sum(bycoin$Totprob) # normalising 
  return(bycoin)
}