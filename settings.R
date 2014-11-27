#original.width <- options("width")
#options("width"=100)

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

# is the player short suited?
short.suited <- function(suit.in.play,unplayed.hand) {
  all(get.suit(unplayed.hand) != suit.in.play)
}


# Evaluates the number of points taken by each player, and who starts next trick.
evaluate.a.trick <- function(plays,i,starter,list=T) {
  # 1. plays is a matrix containing the cards that each player plays
  #    in each round (row)
  # 2. starter is the player that starts the trick
  play <- plays[i,]
  suit.in.play <- get.suit(play[starter])
  players.in.suit <- which(get.suit(play)==suit.in.play)
  vals.in.suit <- as.numeric(get.value(play[players.in.suit]))

  starter <- players.in.suit[which.max(vals.in.suit)]
  points <- sum(get.suit(play)=="H") + ifelse("12S" %in% play,13,0)

  if (list) {
    out <- list("points"=points,"starter"=starter)
  } else {
    out <- paste0(starter,".",points)
  }

  out
}


# Determines what cards in a players hand are valid to play each round.
possible.cards.to.play  <- function(plays,hands,i,j,starter) {
  # pos = possible cards to play
  # plays = cards already played (13 x 4 matrix)
  # hands = the hands of each player (13 x 4 matrix). hands can have only
  #         one known column
  # i = the round number
  # j = the player number
  # starter = the player that started / will start the round
           
  pos <- hands[,j]
  if (i==1) {
    if (j==starter) {
      pos <- "2C"
    } else if (any(!(is.point.card(pos)))) {
        if (any(is.point.card(pos))) pos <- pos[-which(is.point.card(pos))]
        if (!short.suited("C",pos)) {
          pos <- pos[which(get.suit(pos)=="C")]
        }
      }
    } else { #i.e. if i>1
    pos <- pos[-which(pos %in% plays[,j])]
    if (j==starter) {
      if (any(!(is.heart(pos))) && !hearts.broken(plays)) {
        if (any(is.heart(pos))) pos <- pos[-which(is.heart(pos))]
      }
    } else {
      suit.in.play <- get.suit(plays[i,starter])
      if (!short.suited(suit.in.play,pos)) {
        pos <- pos[which(get.suit(pos)==suit.in.play)]
      } 
    }
  }
  
  pos
}

#Simulate a Round 
simulate.a.round <- function(its) {
  play.a.card <- function(plays,hands,i,j,starter,random=T) {
    pos <- possible.cards.to.play(plays,hands,i,j,starter)
    if (random) sample(pos,1)
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
  
  data <- cbind(starter,plays,points)
  data
}

col13 <- matrix(1:13)
move <- function(z,start=1) {
  # Reorders the data so that the first player of each trick 
  # is in the first column, etc.

  out <- NULL
  played <- z[,2:5]

  game <- apply(col13,1,function(x) {
    z1x <- as.numeric(z[x,1])
    play <- played[x,]
    if (z1x==1) {
      out <- play
    } else {
      out <- c(play[z1x:4],play[1:(z1x-1)])
    }
    out <- c(out,evaluate.a.trick(played,x,start,F))
    out
  })
  
  game <- t(game)
  colnames(game) <- c(paste0(c("1st","2nd","3rd","4th"),".player"),"player.points")

  game
}



# Assume no moons
calc.winner.of.round <- function(data) {
  points <- data[,6:9]
  points <- t(apply(points,1,as.numeric))
  summed.points <- apply(points,2,sum)
  which.min(summed.points)
}

#winner.count <- as.numeric(lapply(Z,calc.winner.of.round))
#table(winner.count)/N

