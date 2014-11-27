source("settings.R")

# This is all regression
N <- 10000
Z <- lapply(as.list(1:N), simulate.a.round)

# Get rounds where people don't start, and the suit in play is spades
Y <- numeric(N)

#x1 = I am 2nd
#x2 = I am 3rd
#x3 = I am 4th
#x2 = suit in play is S
#x3 = suit in play is H
#x4 = suit in play is C
#x5 = 
#x6 = 
#x7 =
#x8 =


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

