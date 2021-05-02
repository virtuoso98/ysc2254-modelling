# Koa Zhao Yuan, A0149963M
# Term Project file for Snakes and Ladders

dice_prob <- c(1/36, 2/36, 3/36, 4/36, 5/36, 6/36, 
               5/36, 4/36, 3/36, 2/36, 1/36)

# section 17.6
# initial matrix w/o snakes and ladders
make_trans <- function(){
  trans <- matrix(0, nrow = 100, ncol = 100)
  for (i in 1:99){
    for (j in 1:11){
      if (i + j + 1 > 100){
        # if counter goes beyond the grid
        trans[200 - (i + j + 1), i] <- trans[200 - (i + j + 1), i] + dice_prob[j]
      } else {
        # if counter does not go beyond the grid
        trans[i + j + 1, i] <- trans[i + j + 1, i] + dice_prob[j]
      }
    }
  }
  return(trans)
}

transition <- make_trans()

# last square is absorbing
transition[100, 100] <- 1

# default state matrix, at square 1
state <- rep(0, 100)
state[1] = 1

# run markov chain 100 times
res_initial <- c()
for (i in 1:90){
  state <- transition %*% state
  res_initial <- append(res_initial, state[100])
}

plot(res_initial, 
     main = "Number of Rolls to win w/o Snakes & Ladder",
     ylab = "Probability of hitting last square",
     xlab = "Number of Dice Rolls",
     pch = 19, cex = 0.5)


# swapping function for snakes and ladder
transfer_aux <- function(square, old, new, trans){
  temp <- trans[old, square]
  trans[old, square] <- 0
  trans[new, square] <- trans[new, square] + temp
  return (trans)
}

# function to swap states of all 100 squares
# inefficient but gets the job done nicely
transfer <- function(old, new, trans){
  for (i in 1:100){
    trans <- transfer_aux(i, old, new, trans)   
  }
  return (trans)
}

# snakes 
transition <- transfer(34, 1, transition)
transition <- transfer(25, 5, transition)
transition <- transfer(47, 19, transition)
transition <- transfer(65, 52, transition)
transition <- transfer(87, 57, transition)
transition <- transfer(91, 61, transition)
transition <- transfer(99, 69, transition)

# ladders
transition <- transfer(3, 51, transition)
transition <- transfer(6, 27, transition)
transition <- transfer(20, 70, transition)
transition <- transfer(36, 55, transition)
transition <- transfer(63, 95, transition)
transition <- transfer(68, 98, transition)

state_new <- rep(0, 100)
state_new[1] = 1

# run markov chain 100 times
res_new <- c()
for (i in 1:90){
  state_new <- transition %*% state_new
  res_new <- append(res_new, state_new[100])
}


plot(res_new, 
     main = "Plot of Win probability against dice rolls",
     ylab = "Probability of hitting last square",
     xlab = "Number of Dice Rolls",
     pch = 19, cex = 0.7)

lines(res_new, 
      pch = 18, 
      col = "black", 
      lty = 1,
      cex = 2)


points(res_initial, col = "red", pch = 19, cex = 0.7)

lines(res_initial, 
      pch = 18, 
      col = "red", 
      lty = 2,
      cex = 2)


legend("bottomright", 
       legend = c("No Snakes and Ladder", 
                  "Snakes and Ladder"),
       col = c("Black", "Red"),
       cex = 1.5,
       lty = 1:2,
       pch = 19)
