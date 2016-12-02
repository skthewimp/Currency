
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

distrib <- function(maxamount, meantrans, coins)
  # given a maximum amount to transact, and a mean transaction value, calculate the relative use of 
  # different currencies
{
  amt <- mincoinsDijkstra(maxamount, coins) # number of coins for each transaction amount
  amt <- amt[amt$Amount <= maxamount,]
  amt$Prob <- probsExp(maxamount,meantrans) # assume an exponential distribution in probaibility
  
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