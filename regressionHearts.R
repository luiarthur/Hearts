source("lrbind.R")
source("rapply.R")
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

new.Z <- lapply(Z,move)
X <- lrbind(new.Z)
dim(X)

temp <- apply(X,2,function(x) as.numeric(get.value(x)))
y <- ifelse(temp[,5]==1,1,0)
X1 <- temp[,1]
X2 <- get.suit(X[,1])
X2 <- as.factor(X2)
X3 <- unlist(lapply(new.Z,function(x) {
  out <- NULL
  out[1] <- 13
  for (i in 2:13) {
    out[i] <- sum(get.suit(x[0:(i-1),2:4]) == get.suit(x[i,1]))
  }
  out
}))
X4 <- apply(matrix(1:length(X2)),1,function(s)sum(get.suit(played[-(0:ifelse(s%%13==0,13,s%%13)),1])==X2[s]))

mod <- glm(y~X1+X2+X3+X4,family="binomial")
#new <- data.frame(X1=X1[1:3],X2=X2[1:3],X4=X4[1:3])
pred <- predict(mod,type="response")
pred.0.1 <- ifelse(pred>.5,1,0)
mean(pred.0.1==y)
summary(mod)
