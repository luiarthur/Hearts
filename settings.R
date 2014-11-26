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
  card == "12S" | get.suit(card) == "H"
}

# hearts have benn broken?
hearts.broken <- function(played) {
  any(get.suit(played)=="H")
}


