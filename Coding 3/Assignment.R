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

result <- solnp(c(3,3),
                opt_func,
                eqfun = constr,
                eqB = 6,
                LB = c(0, 0),
                UB = c(100, 100))

# 6 units of x1 (Labour) and 6 units of x2 (Capital)
result$pars

# Cost of $18
result$values

# Exercise 11.8.4
# package does minimization by default. Hence, to turn
# Max problem to min problem, objective function must be
# multiplied by -1 and this be minimized
opt_func_2 <- function(x){
  return(-((30*(x[1]**(0.5))) + (20*(x[2]**(0.5))) - x[1] - x[2]))
}

constr_2 <- function(x){
  return(x[1] + x[2])
}

result <- solnp(c(55,35),
                fun = opt_func_2,
                eqfun = constr_2,
                eqB = 100,
                LB = c(0, 0),
                UB = c(100, 100),
                control = (delta = 1))

# profit of $261 (multiply obj by -1 again)
result$values

# $69.23076 spent on advertising in territory 1
# $30.76924 spent on advertising in territory 2
result$pars

# what if $101 was available for promotion instead?
result_alt <- solnp(c(55,35),
                    fun = opt_func_2,
                    eqfun = constr_2,
                    eqB = 101,
                    LB = c(0, 0),
                    UB = c(100, 100),
                    control = (delta = 1))

# profit of $261 
result_alt$values

# $69.92306 spent on advertising in territory 1
# $31.07694 spent on advertising in territory 2
result_alt$pars

# how much would profits increase?
# by $0.80, variable denoted by profit_inc
profit_inc <- -(opt_func_2(result_alt$pars) - opt_func_2(result$pars))

# how much would revenue increase? 
revenue_fun <- function(x){
  return((60*(x[1]**(0.5))) + (36*(x[2]**(0.5))))
}

# denoted by revenue_inc, profits increase by about $3.49
revenue_inc <- revenue_fun(result_alt$pars) - revenue_fun(result$pars)

# Exercise 11.9.1
# Kuhn-Tucker conditions

# based on the normal problem, denote x[1] as p1 dollars
# denote x[2] as p2 dollars and x[3] as power demand

library('nloptr')

# as usual, nloptr does minimization by default. To 
# convert max problem to equivalent min problem, 
# multiply obj function by -1

eval_f <- function(x){
  return (-(x[1]*(60 - 0.5*x[1]) + x[2]*(40 - x[2]) - 10 * x[3]))
}

# upper and lower bounds
lb <- c(0,0,0)
ub <- c(200, 200, 200)

# inequality constrain
eval_g_ineq <- function(x){
  return((60 - 0.5*x[1]) - x[3])
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

# profit would be about 1910 based on obj function
# power demand is approx 27.5 kwh
# p1 = 65 approx, p2 = 20 approx

print(res)

# Exercise 11.9.6
# Kuhn-Tucker conditions

# Objective Function
eval_f <- function(x){
  return (x[1]^2 - 2*x[1] + x[2]^2 - 4*x[2] + 5)
}

# Inequality constraints
eval_g_ineq <- function(x){
  return (x[1] + x[2] - 2)
}

# Equality constraints
eval_g_eq <- function(x){
  return (x[1] - x[2] + 1)
}

# upper and lower bounds
lb <- c(0,0)
ub <- c(100,100) 

# initial values
x0 <- c(10,10)


# Set optimization options.
local_opts <- list("algorithm" = "NLOPT_LD_MMA", "xtol_rel" = 1.0e-15)

opts <- list( "algorithm"= "NLOPT_GN_ISRES",
              "xtol_rel"= 1.0e-10,
              "maxeval"= 300000,
              "local_opts" = local_opts,
              "print_level" = 0 )

# run optimizer
res4 <- nloptr (x0 = x0,
                 eval_f = eval_f,
                 lb = lb,
                 ub = ub,
                 eval_g_ineq = eval_g_ineq,
                 eval_g_eq = eval_g_eq,
                 opts = opts)

# x1= 0.5, x2 = 1.5, minimum value = 0.5
print(res4)

  
