# cards is a vector of strings in which each string represents a card
cards <- c("2","3","4","5","6","7","8","9","10","j","q","k","a",
           "2","3","4","5","6","7","8","9","10","j","q","k","a",
           "2","3","4","5","6","7","8","9","10","j","q","k","a",
           "2","3","4","5","6","7","8","9","10","j","q","k","a")

# dealer returns 5 cards, each of which is taken from the cards array
# using the sample function, 5 cards are taken from cards and they are not replaced
dealer <- function(cards) {
  return(sample(cards, 5, replace = FALSE))
}

# stores a dataframe of what is returned from a frequency table of one hand
# stores the frequency column of the datafram as a vector for manipulation, sorted descending
# if the vector has:
#                    4 followed by 1 = four of a kind
#                    3 followed by 2 = full hand
#                    3 followed by 1 = three of a kind
#                    2 followed by 2 = two pairs
#                    2 = one pair
simulation <- function(x, cards) {
  onepair = 0
  twopair = 0
  threekind = 0
  fourkind = 0
  fullhouse = 0 
  for (i in 1:x) {
    table <- as.data.frame(table(c(dealer(cards))))
    vector <- sort(table[,2], decreasing = TRUE)
    if (vector[1] == 4) {
      fourkind = fourkind + 1
    }
    else if ((vector[1] == 3) && (vector[2] == 2)) {
      fullhouse = fullhouse + 1
    }
    else if (vector[1] == 3) {
      threekind = threekind + 1
    }
    else if ((vector[1] == 2) && (vector[2] == 2)) {
      twopair = twopair + 1
    }
    else if (vector[1] == 2) {
      onepair = onepair + 1
    }
  }
  cat("One pairs: ", onepair, "\n")
  cat("Two pairs: ", twopair, "\n")
  cat("Three of a kinds: ", threekind, "\n")
  cat("Fullhouses: ", fullhouse, "\n")
  cat("Four of a kinds: ", fourkind, "\n")
}

# testing
cat("-- 10 hands --\n")
simulation(10, cards)
cat("-- 100 hands --\n")
simulation(100, cards)
cat("-- 1000 hands --\n")
simulation(1000, cards)
cat("-- 5000 hands --\n")
simulation(5000, cards)
cat("-- 10000 hands --\n")
simulation(10000, cards)
cat("-- 20000 hands --\n")
simulation(20000, cards)
