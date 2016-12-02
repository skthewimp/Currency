mincoinsDijkstra <- function(maxamount, coins)
  # given a set of coins, this computes the optimal way to pay each amount from 1 to maxamount.
  # Important difference from the DP implementation of the coin change problem is that we allow negatives
  # That is, the receiver of cash can give back some "change" to the payer if that can decrease the total 
  # number of coins involved
{
  amounts <- c(1:(ceiling(maxamount/max(coins))*max(coins)),-(1:max(coins)))
  amtable <- as.data.frame(amounts)
  names(amtable)[1] <- "Amount"
  amtable$NumSteps <- NA 
  amtable$Step <- NA 
  allcoins <- c(coins,-coins)
  amts <- c(0)
  i <- 1
  while(sum(is.na(amtable$NumSteps)) > 0)
  {
    sums <- expand.grid(amts,allcoins)
    names(sums) <- c("Prior","Coins")
    sums$Amount <- sums$Prior + sums$Coins
    sums <- sums[with(sums,order(Amount,Coins)),] # the smallest coin to contribute to this is the "Winner"
    sums <- sums[sums$Amount %in% amtable[is.na(amtable$NumSteps),"Amount"],] # if something has already been done, we have a shorter path
    sums$Index <- 1:nrow(sums)
    minsums <- aggregate(Index~Amount,sums,min)
    minsums <- merge(minsums,sums)
    amtable2 <- amtable[amtable$Amount %in% minsums$Amount,]
    amtable2$NumSteps <- i
    amtable2 <- merge(amtable2[,1:2],minsums[,c("Amount","Coins")])
    names(amtable2)[3] <- "Step"
    amtable <- rbind(amtable[!(amtable$Amount %in% minsums$Amount),],amtable2)
    i <- i + 1 
    amts <- minsums$Amount
  }
  amtable <- amtable[with(amtable,order(Amount)),]
  
  firststep <- amtable[,c("Amount","Step")]
  names(firststep)[2] <- "Step1"
  firststep[nrow(firststep)+1,] <- c(0,0) # terminating
  amtable$Path <- amtable$Step
  amtable$Current <- amtable$Amount - amtable$Step
  
  for(j in 1:i) 
  {
    amtable <- merge(amtable,firststep,by.x="Current",by.y="Amount",all.x=T)
    amtable$Step <- amtable$Step1
    amtable$Path <- ifelse(amtable$Step==0,amtable$Path,paste(amtable$Path,amtable$Step,sep=','))
    amtable$Current <- amtable$Current - amtable$Step
    amtable <- amtable[,c("Amount","NumSteps","Path","Current","Step")]
  }
  
  amtable <- amtable[with(amtable,order(Amount)),]
  
  amtable <- amtable[amtable$Amount > 0,c("Amount","NumSteps","Path")]
  return(amtable)
}