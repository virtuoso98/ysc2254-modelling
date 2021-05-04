# Koa Zhao Yuan, A0149963M 
# Reviewing my Midterm Submission

library("nloptr")



# objective function minimizes variance in utility
obj <- function(x){
  var = 0
  mean_util = 0
  for (i in 1:35){
    mean_util <- mean_util + (x[i] * (1 + 0.05 * ((i - 1) %% 5)))
  }
  mean_util <- mean_util / 7
  for (j in 0:6){
    util_day <- 0
    for (k in 1:5){
      util_day <- util_day + x[k + 5*j] * (1 + (0.05 * (k - 1))) 
    }
    var <- var + (util_day - mean_util)^2
  }
  return (var / 7)
}

eval_g_eq <- function(x){
  constr <- c (
    x[1] + x[2] + x[3] + x[4] + x[5] - 24, 
    x[6] + x[7] + x[8] + x[9] + x[10] - 24,
    x[11] + x[12] + x[13] + x[14] + x[15] - 24,
    x[16] + x[17] + x[18] + x[19] + x[20] - 24,
    x[21] + x[22] + x[23] + x[24] + x[25] - 24,
    x[26] + x[27] + x[28] + x[29] + x[30] - 24,
    x[31] + x[32] + x[33] + x[34] + x[35] - 24
  )
  return(constr)
}

eval_g_ineq <- function(x){
  constr <- c(
    3
  )
  return((60 - 0.5*x[1]) - x[3])
}

lb <- rep(0, 35)
ub <- rep(24, 35)

local_opts <- list( "algorithm" = "NLOPT_LD_MMA", 
                    "xtol_rel" = 1.0e-15 )

opts <- list( "algorithm"= "NLOPT_GN_ISRES",
              "xtol_rel"= 1.0e-15,
              "maxeval"= 10000,
              "local_opts" = local_opts,
              "print_level" = 0 )

x0 = rep(4, 35)

res <- nloptr(x0 = x0,
              eval_f = obj,
              lb = lb,
              ub = ub,
              eval_g_ineq = eval_g_ineq,
              eval_g_eq = eval_g_eq,
              opts = opts)
print(res)
