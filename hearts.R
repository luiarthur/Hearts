original.width <- options("width")
options("width"=100)

# Define the suits & values
suits <- c("S","H","C","D")
values <- 2:14

# Create a matrix of 4-column player hands (cards)
hand <- matrix(0,13,4)

# Create a deck of cards
deck <- paste0(rep(values,4),rep(suits,each=13))

# Shuffle the deck of cards
shuffle <- function(x=deck) { sample(x) }

# Deal the cards
deal <- function(x=deck) {
  cards <- shuffle(x)
  out <- matrix(cards,13,4)
  out
}

# Get the suit of a card
get.suit <- function(card) {
  substr(card,nchar(card),nchar(card))
}

# Get the value of a card
get.value <- function(card) {
  substr(card,1,nchar(card)-1)
}

# Sort a hand
sort.hand <- function(hand) {
  sort.in.suit <- function(in.suit) { # in.suit = cards of same suit
    suit <- get.suit(in.suit)
    vals <- as.numeric(get.value(in.suit))
    sorted.in.suit <- paste0(sort(vals),suit)
    sorted.in.suit 
  }

  hand.suits <- lapply(as.list(suits), function(x)
                       hand[which(get.suit(hand)==x)])
  hand.list <- lapply(hand.suits,sort.in.suit)
  hand <- c(hand.list[[1]],hand.list[[2]],hand.list[[3]],hand.list[[4]])
  hand
}

# Is this card a point card?
is.point.card <- function(card) {
  card == "12S" || get.suit(card) == "H"
}

#Assume just want to win one round. Not to 100 points
one.play.one.trick <- function(hand,played,i,m=c(.25,.25,.25,.25),p=c(0,0,0,0)) {
  # Returns the card played by player

  # i = the trick number
  # j = the player
  # hand = the hands matrix storing the player's hand
  # m = moon shooting probability
  # p = points that each player currently has. p_j in {1,...,100}
  # played = the cards already played in the round. 13 x 4, i = current round

  # Strategy: Always play high cards first,
  #           if it is too risky, play low.
  # I want the fewest points.
  # Make sure no one shoots the moon.
  # Beat Mickeys player.
  
 
  # Verion1: Assume no moon shots are allowed
  card <- NULL
  i.am.first <- ifelse(all(played[i,]==0),T,F)

  # play a card
  if (i==1) {
    if ("2C" %in% hand){
      card <- "2C"
    } else {

      # Stopped HERE!     
      # if !(i.am.first)
      # 1. play 0-pt card IN suit
      # 3. play pt card IN suit
      # 2. play 0-pt card OUT of suit
      # 4. play pt card OUT of suit
      # if i.am.first
      # 5. start trick with 0-pt card
      # 6. start trick with pt card
    }
  }
  
  list("card"=card,"hand"=hand)
}


