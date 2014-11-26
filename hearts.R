source("settings.R")

#Assume just want to win one round. Not to 100 points
one.play.one.trick <- function(hand,played,i,j,m=c(.25,.25,.25,.25),p=c(0,0,0,0)) {
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
  original.colnames <- colnames(played)
  colnames(played) <- 1:4

  card <- NULL
  i.am.first <- ifelse(all(played[i,]==0),T,F)

  # play a card
  if (i==1) {
    if ("2C" %in% hand){
      card <- "2C"
    } else {
      # Stopped HERE!     
      # if !(i.am.first)
      #   1. play 0-pt card IN suit
      #   3. play pt card IN suit
      #   2. play 0-pt card OUT of suit
      #   4. play pt card OUT of suit
      # if i.am.first
      #   5. start trick with 0-pt card
      #   6. start trick with pt card
      if (i.am.first) {
      }
    }
  }
  
  list("card"=card,"hand"=hand)
}


