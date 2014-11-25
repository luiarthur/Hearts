original.width <- options("width")
options("width"=100)

# Create a matrix of 4-column player hands (cards)
hand <- matrix(0,13,4)

# Create a deck of cards
deck <- paste0(rep(2:14,4),rep(c("S","H","C","D"),each=13))

# Shuffle the deck of cards
shuffle <- function(x=deck) { sample(x) }

# Deal the cards
deal <- function(x=deck) {
  cards <- shuffle(x)
  out <- matrix(cards,13,4)
}

one.play.one.trick <- function(i,j,hand,m=c(.25,.25,.25,.25),p=c(0,0,0,0)) {
  # Returns the card played by player j in trick i

  # i = the trick number
  # j = the player
  # hand = the hands matrix storing
  #        what cards were dealt
  # m = moon shooting probability
  # p = points that each player currently has. p_j in {1,...,100}

  # Strategy: Always play high cards first,
  #           if it is too risky, play low.
  # I want the fewest points.
  # Make sure no one shoots the moon.
  # Beat Mickeys player.

  if (i==1 && j==1) {
    starter <- which(apply(hand,2,function(x) "2C" %in% x))
    if (starter > 1) {
      hand <- cbind(hand[,starter:4],hand[1:(starter-1)])
    }
    card <- "2C"
  }
  
  list("card"=card,"hand"=hand)
}
