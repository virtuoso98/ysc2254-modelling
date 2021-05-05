# Koa Zhao Yuan, A0149963M 
# Reviewing my Midterm Submission

library("nloptr")

# objective function maximizes utility function
obj <- function(x){
  total = 0
  for (i in 1:35){
    total <- total + (x[i] * (1 + 0.05 * ((i - 1) %% 5)))
  }
  return(total * (-1))
}

eval_g_eq <- function(x){
  constr <- c (
    # day equality constraints (24 hours)
    24 - (x[1] + x[2] + x[3] + x[4] + x[5]), 
    24 - (x[6] + x[7] + x[8] + x[9] + x[10]),
    24 - (x[11] + x[12] + x[13] + x[14] + x[15]),
    24 - (x[16] + x[17] + x[18] + x[19] + x[20]),
    24 - (x[21] + x[22] + x[23] + x[24] + x[25]),
    24 - (x[26] + x[27] + x[28] + x[29] + x[30]),
    24 - (x[31] + x[32] + x[33] + x[34] + x[35])
  )
  return(constr)
}

sleep_var <- function(x){
  # function to calculate sleep variance
  mean_sleep <- (x[5] + x[10] + x[15] + x[20] + x[35] + x[30] + x[35]) / 7
  var <- 0
  for (j in 1:7){
    var <- var + (x[5*j] - mean_sleep) ** 2
  }
  return (var / 7)
}

meal_var <- function(x){
  # function to calculate sleep variance
  mean_meal <- (x[1] + x[6] + x[11] + x[16] + x[21] + x[26] + x[31]) / 7
  var <- 0
  for (j in 1:7){
    var <- var + (x[5*j - 4] - mean_meal) ** 2
  }
  return (var / 7)
}

sleep_minus_work <- function(x){
  # function to calculate difference in 
  # sum of squares of sleep and work
  sleep_total_sq <- 0
  work_total_sq <- 0
  for (i in 1:7){
    sleep_total_sq <- x[5*i] ^ 2
    work_total_sq <- x[5*i - 1] ^ 2
  }
  return(work_total_sq - 0.95 * sleep_total_sq)
}

eval_g_ineq <- function(x){
  constr <- c(
    # constraint on mealtime
    # lower limit
    1.0 - x[1],
    1.0 - x[6],
    1.0 - x[11],
    1.0 - x[16],
    1.0 - x[21],
    1.5 - x[26],
    1.5 - x[31],
    # upper limit
    x[1] - 1.5,
    x[6] - 1.5,
    x[11] - 1.5,
    x[16] - 1.5,
    x[21] - 1.5,
    x[26] - 2.0,
    x[31] - 2.0,
    # constraint on sleep
    #lower limit
    7.5 - x[5],
    7.5 - x[10],
    7.5 - x[15],
    7.5 - x[20],
    7.5 - x[25],
    8.0 - x[30],
    8.0 - x[35],
    # upper limit
    x[5] -  10.0,
    x[10] - 10.0,
    x[15] - 10.0,
    x[20] - 10.0,
    x[25] - 10.0,
    x[30] - 11.0,
    x[35] - 11.0,
    # constraint on work
    # lower limit
    8 - x[4], 
    8 - x[9], 
    8 - x[14], 
    8 - x[19], 
    8 - x[24], 
    # upper limit
    x[4] - 10, 
    x[9] - 10, 
    x[14] - 10, 
    x[19] - 10, 
    x[24] - 10,
    x[29],
    x[34],
    # constraint on leisure
    #lower limit
    1 - x[3],
    1 - x[8],
    1 - x[13],
    1 - x[18],
    1 - x[23],
    5 - x[28],
    5 - x[33],
    # upper limit
    x[3] -  2,
    x[8] -  2,
    x[13] - 2,
    x[18] - 2,
    x[23] - 2,
    x[28] - 8,
    x[33] - 8,
    # constraint on commitments
    #lower limit
    2 - x[2],
    0 - x[7],
    0 - x[12],
    2 - x[17],
    0 - x[22],
    5 - x[27],
    5 - x[32],
    # upper limit
    x[2] -  3,
    x[7] -  2,
    x[12] - 2,
    x[17] - 3,
    x[22] - 2,
    x[27] - 7,
    x[32] - 7,
    # leisure constraint on weekends and total leisure
    x[29] + x[30] + x[26] + x[27] - x[28]^2,
    x[34] + x[35] + x[31] + x[32] - x[33]^2,
    # minimum number of leisure hours in a hour
    20 - (x[33] + x[28] + x[23] + x[18] + x[13] + x[8] + x[3]),
    # upper limit constraint on sleep and meal variance
    sleep_var(x) - 8,
    meal_var(x) - 6,
    # sleep constraint vs work (function above)
    sleep_minus_work(x),
    # Tuesday, Weds, Friday constraint
    # on Meal, Commitment and Leisure
    x[7]^2 - x[8]^2 - x[6]^2,
    x[12]^2 - x[13]^2 - x[11]^2,
    x[22]^2 - x[23]^2 - x[21]^2,
    # Mon, Thursday constraint
    # on Meal, Commitment and Leisure
    0.6 * x[2]^2 - x[3]^2 - x[1]^2,
    0.6 * x[17]^2 - x[18]^2 - x[16]^2
  )
  return (constr)
}

# defining upper and lower bounds
lb <- rep(0, 35)
ub <- rep(12, 35)

local_opts <- list( "algorithm" = "NLOPT_LD_MMA", 
                    "xtol_rel" = 1.0e-15 )

opts <- list( "algorithm"= "NLOPT_GN_ISRES",
              "xtol_rel"= 1.0e-15,
              "maxeval"= 200000,
              "local_opts" = local_opts,
              "print_level" = 0 )


x0 = rep(3, 35)

res <- nloptr(x0 = x0,
              eval_f = obj,
              lb = lb,
              ub = ub,
              eval_g_ineq = eval_g_ineq,
              eval_g_eq = eval_g_eq,
              opts = opts)

print(res)

# same representation as seen
# in my term project report
matrix_rep <- matrix(res$solution,
                     nrow = 5,
                     ncol = 7)
