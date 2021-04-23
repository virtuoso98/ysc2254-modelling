dice_prob <- c(1/36, 2/36, 3/36, 4/36, 5/36, 6/36, 5/36, 4/36, 3/36, 2/36, 1/36)

state <- matrix(0, 100, 100)

for (i in 1:199){
  for (j in 1:11){
    state[i, i + j + 1] <- state[i, i + j + 1] + dice_prob[j]
  }
}

state[100, 100] <- 1
state[100, 100]
