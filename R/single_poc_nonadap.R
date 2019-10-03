#' Single Arm Calculator (Non-Adaptive)
#'
#' This function to calculate n given a desired alpha and power in a non-adaptive setting. By default considers the 25% vs 40% case for alpha = 0.05 and power = 0.80
#' @soc Standard of care efficacy
#' @exp Expected experimental treatment efficacy
#' @alpha Desired alpha level
#' @pwr Desired power
#'
#' @example
#' single_poc()
#' single_poc(soc = 0.30, exp = 0.42, alpha = 0.03, pwr = 0.85)

single_poc <- function(soc = 0.25, exp = 0.40 , alpha = 0.05, pwr = 0.80){
  cond1 <- FALSE
  n <- 10
  z_temp <- 0
  while(cond1 == FALSE){
    z <- seq(1:n)
    for(i in 1:n){
      check1<- 1 - pbinom(z[i],n,soc)
      check2<- 1 - pbinom(z[i],n,exp)
      if(check1 < alpha & check2 > pwr){
        z_temp <- z[i]
        cond1 <- TRUE
        break
      }
    }
    n <- n + 1
  }
  print(paste(paste(paste(paste("Need", z_temp), "patient responses from sample size"), n)))
}
