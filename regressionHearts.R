source("settings.R")

# This is all regression
N <- 10000
Z <- lapply(as.list(1:N), simulate.a.round)

# Get rounds where people don't start, and the suit in play is spades
Y <- numeric(N)

# Model1: If I am the first player in the trick
#x1 = card suit I play
#x2 = card value I play (as factor)
#x3 = # of cards in suit I hold
#x4 = # of cards in suit others hold

S <- lapply(Z,function(z) {
  out <- matrix(0,0,ncol(z))
  for (i in 1:13) {
    if (any(get.suit(z[i,2:5])=="S")) {
      if (z[i,1] %in% which(get.suit(z[i,2:5])=="S")){
        out <- rbind(out,z[i,])
      }
    }
  }
  out
})

