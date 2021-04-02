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
opt_func_2 <- function(x){
  return(-((30*(x[1]**(0.5))) + (20*(x[2]**(0.5))) - x[1] - x[2]))
}

constr_2 <- function(x){
  return(x[1] + x[2])
}

solnp(c(55,35),
      fun = opt_func_2,
      eqfun = constr_2,
      eqB = 100,
      LB = c(0, 0),
      UB = c(100, 100),
      control = (delta = 1))

# Exercise 11.9.1
# Kuhn-Tucker conditions
library('nloptr')
eval_f <- function(x){
  return (-(x[1]*(60 - 0.5*x[1]) + x[2]*(40 - x[2]) - 10 * x[3]))
}

lb <- c(0,0,0)
ub <- c(200, 200, 200)
eval_g_ineq <- function(x){
  return(-x[3]+(60 - 0.5*x[1]))
}

x0 <- c(5, 5, 5)

local_opts <- list( "algorithm" = "NLOPT_LD_MMA", 
                    "xtol_rel" = 1.0e-15 )
opts <- list( "algorithm"= "NLOPT_GN_ISRES",
              "xtol_rel"= 1.0e-15,
              "maxeval"= 160000,
              "local_opts" = local_opts,
              "print_level" = 0 )

res <- nloptr(x0 = x0,
                eval_f = eval_f,
                lb = lb,
                ub = ub,
                eval_g_ineq = eval_g_ineq,
                opts = opts)
print(res)
# Exercise 11.9.6
# Kuhn-Tucker conditions

# Objective Function
eval_f <- function(x)
{
  return (x[1]^2-2*x[1]+x[2]^2-4*x[2]+5)
}

# Inequality constraints
eval_g_ineq <- function(x)
{
  return (x[1]+x[2]-2)
}

# Equality constraints
eval_g_eq <- function(x)
{
  return (x[1]-x[2]+1)
}

# Lower and upper bounds
lb <- c(0,0)
ub <- c(10000,10000) #arbitrarily large
#initial values
x0 <- c(10,10)


# Set optimization options.
local_opts <- list( "algorithm" = "NLOPT_LD_MMA", "xtol_rel" = 1.0e-15 )
opts <- list( "algorithm"= "NLOPT_GN_ISRES",
              "xtol_rel"= 1.0e-10,
              "maxeval"= 300000,
              "local_opts" = local_opts,
              "print_level" = 0 )

res4 <- nloptr ( x0 = x0,
                 eval_f = eval_f,
                 lb = lb,
                 ub = ub,
                 eval_g_ineq = eval_g_ineq,
                 eval_g_eq = eval_g_eq,
                 opts = opts
)
res4

# Linear Regression Assignment

# In this part, I use the default implementation
# first before moving to the more interesting one
data("USArrests")
R_Model <- lm(USArrests$Rape ~ USArrests$UrbanPop)
R_Model
plot(USArrests$Rape ~ USArrests$UrbanPop, 
     pch = 16, 
     cex = 1.3,
     col = "red",
     main = "Plot of Murder Against Urban Population",
     xlab = "Urban Population (By percentage)",
     ylab = "Numeric Rape arrests (Per 100000)")

abline(R_Model, col = "blue")

# Now, I move unto the linear algebra way of 
# Solving such a problem

x_val <- (USArrests$UrbanPop)
y <- (USArrests$Rape)
intercept <- rep(1, 50) 
plot(y ~ x1,
     pch = 16, 
     cex = 1.3,
     col = "red",
     main = "Plot of Murder Against Urban Population",
     xlab = "Urban Population (By percentage)",
     ylab = "Numeric Rape arrests (Per 100000)")


A <- cbind(intercept,x_val)
B <- solve(t(A)%*%A)
M <- B%*%t(A)
b <- M%*%y

# The 2 linear lines overlap
abline(b, col ="black", cex = 1)
abline(R_Model, col = "blue")

# As you can see, the coefficients are also the same.
b
R_Model
