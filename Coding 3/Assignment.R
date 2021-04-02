# A0149963M
# Coding 3 

library('Rsolnp')
# Exercise 11.8.3
opt_func <- function(x){
  return(2*x[1] + x[2])
}
  
constr <- function(x){
  return(x[1]^(2/3) * x[2]^(1/3))
}

solnp(c(3,3),
      opt_func,
      eqfun = constr,
      eqB = 6,
      LB = c(0, 0),
      UB = c(100, 100)
      )

# Exercise 11.8.4
opt_func <- function(x){
  return(30*(x[1]**(0.5)) + 20*(x[2]**(0.5)) - x[1] - x[2])
}

constr <- function(x){
  return(x[1] + x[2])
}

solnp(c(20,30),
      fun =opt_func,
      eqfun = constr,
      eqB = 100,
      LB = c(0, 0),
      UB = c(100, 100)
)

# Exercise 11.9.1
# Kuhn-Tucker conditions
library('optextras')
function(x){
  a = x[1]
  b = x[2]
  c = x[3]
}

# Exercise 11.9.6
# Kuhn-Tucker conditions