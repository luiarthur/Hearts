original.width <- options("width")
options("width"=100)

# Define the suits & values
suits <- c("C","D","S","H")
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
is.qs <- function(card) card == "12S"
is.heart <- function(card) get.suit(card) == "H"

is.point.card <- function(card) {
  is.qs(card) | is.heart(card)
}

# hearts have benn broken?
hearts.broken <- function(plays) {
  any(get.suit(plays)=="H")
}

short.suited <- function(suit.in.play,unplayed.hand) {
  all(get.suit(unplayed.hand) != suit.in.play)
}

#Simulate a Round 
simulate.a.round <- function(its) {
  evaluate.a.trick <- function(plays,i,starter) {
    # 1. plays is a matrix containing the cards that each player plays
    #    in each round (row)
    # 2. starter is the player that starts the trick
    play <- plays[i,]
    suit.in.play <- get.suit(play[starter])
    players.in.suit <- which(get.suit(play)==suit.in.play)
    vals.in.suit <- as.numeric(get.value(play[players.in.suit]))

    starter <- players.in.suit[which.max(vals.in.suit)]
    points <- sum(get.suit(play)=="H") + ifelse("12S" %in% play,13,0)

    list("points"=points,"starter"=starter)
  }

  # Something wrong here
  play.a.card <- function(plays,hands,i,j,starter,random=T) {
    hand <- hands[,j]
    played <- plays[,j]
    possible.cards.to.play <- hand
    suit.in.play <- get.suit(play[starter])

    if (i>1) possible.cards.to.play <- hand[-which(hand %in% played)] 
    if (starter==j && !hearts.broken(plays) && 
        any(get.suit(possible.cards.to.play)!="H")) {
      # you can't start with a heart if hearts haven't been broken, and 
      # you have cards that are not hearts
      possible.cards.to.play <- possible.cards.to.play[-which(get.suit(hand)=="H")]
    }

    if (!(short.suited(suit.in.play,possible.cards.to.play))) {
      #if not short suited, play in suit
      pos <- possible.cards.to.play
      possible.cards.to.play <- pos[which(get.suit(pos)==suit.in.play)]
      if (!hearts.broken(plays) && suit.in.play != "H") {
        if (any(is.heart(possible.cards.to.play))){
          # if hearts not broken and not in hearts and you have non hearts
          pos <- possible.cards.to.play
          hearts <- which(get.suit(pos) == "H")
          possible.cards.to.play <- pos[-hearts]
        }
      }
    }
    if (random) sample(possible.cards.to.play,1)
  }

  hands <- apply(deal(),2,sort.hand)
  starter <- NULL
  starter[1] <- which(hands[1,]=="2C")
  plays <- matrix("",13,4)
  points <- matrix(0,13,4)

  for (i in 1:13) {
    play.order <- 1:4
    if (starter[i]!=1) {
      play.order <- c(starter[i]:4,1:(starter[i]-1))
    }

    for (j in play.order) {
      if (i==1 && j==play.order[1]) {
        plays[i,j] <- "2C"
      } else {
        plays[i,j] <- play.a.card(plays,hands,i,j,starter[i])
      }  
    }

    trick <- evaluate.a.trick(plays,i,starter[i])
    points[i,trick$starter] <- trick$points
    if (i<13) starter[i+1] <- trick$starter
  }
  
  data <- cbind(starter,plays)
  list("points"=points,"data"=data)
}

X <- lapply(as.list(1:100), simulate.a.round)
