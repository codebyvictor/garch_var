
f_forecast_var <- function(y, level) {
  ### Compute the VaR forecast of a GARCH(1,1) model with Normal errors at the desired risk level
  #  INPUTS
  #   y     : [vector] (T x 1) of observations (log-returns)
  #   level : [scalar] risk level (e.g. 0.95 for a VaR at the 95# risk level)
  #  OUTPUTS
  #   VaR   : [scalar] VaR forecast 
  #   sig2  : [vector] (T+1 x 1) conditional variances
  #   theta : [vector] GARCH parameters
  #  NOTE
  #   o the estimation is done by maximum likelihood
  
  # Fit a GARCH(1,1) model with Normal errors
  # Starting values and bounds
  # The parameters must satisfy a0>=0, a1>0, b1>0
  theta0 <- c(0.1 * var(y), 0.1, 0.8)
  LB     <- c(0, 1e-5, 1e-5) # Bounds for the a0, a1, b1
  
  # ConstrOptim function expects the form Ax >= b
  # The implementation of the value 1e-5 which is slightly greater than zero, is employed to guarantee the satisfaction of the strictly "> b" or "< b" constraints
  # For the =< and < constraints, we need to multiply both sides by -1 to flip the inequality
  
  
  # Stationarity condition
  # The GARCH(1,1) is stationary if the sum of a1 + b1 < 1
  # The implementation of the value -1+1e-5 (slightly greater than -1) is to ensure the satisfaction of a1+b1 < 1
  # Now combining the stationarity condition and the lower bound in a system of linear inequalities such that : Ax >=b 
  # Vector b size : size(LB)+1 , as it includes the elements of the lowe bounds (LB) and the additional stationarity condition
  
  A <- matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1, 0, -1, -1), nrow=4, byrow=TRUE)
  b      <- c(LB,(-1+1e-5))  
  
  # Each row of A corresponds to a linear inequality, and the corresponding entry in b is the constant term for that inequality
  
  # Run the optimization
  optimization <- constrOptim(theta = theta0,
                     f = f_nll,
                     y = y,
                     grad = NULL,
                     ui = A,
                     ci = b,)
  
  theta = c(optimization$par[1], optimization$par[2], optimization$par[3])    # Saving the resulting parameters 
  
  # Recompute the conditional variance
  sig2 <- f_ht(theta, y) 
  
  # Compute the next-day ahead VaR for the Normal model
  VaR <- qnorm(1-level,0,1)*sqrt(sig2) 
  
  out <- list(VaR_Forecast = VaR, 
              ConditionalVariances = sig2, 
              GARCH_param = theta)
  
  out
}

f_nll <- function(theta, y) {
  ### Fonction which computes the negative log likelihood value 
  ### of a GARCH model with Normal errors
  #  INPUTS
  #   theta  : [vector] of parameters
  #   y      : [vector] (T x 1) of observations
  #  OUTPUTS
  #   nll    : [scalar] negative log likelihood value
  
  T <- length(y) # number of observations
  
  # Compute the conditional variance of a GARCH(1,1) model
  sig2 <- f_ht(theta, y) 
  
  # Consider the T values
  sig2 <- sig2[1:T]
  
  # Compute the loglikelihood
  # Initialization
  ll<- 0 
  for (t in 1:T){
    ll <- ll + dnorm(y[t], mean = 0, sd = sqrt(sig2[t]), log = TRUE)
  }
  
  # Output the negative value
  nll <- -ll
  
  # Returns the negative log-likelihood value
  nll
}

f_ht <- function(theta, y)  {
  ### Function which computes the vector of conditional variance
  #  INPUTS
  #   x0 : [vector] (3 x 1)
  #   y     : [vector] (T x 1) log-returns
  #  OUTPUTS 
  #   sig2  : [vector] (T+1 x 1) conditional variances
  
  # Extract the parameters
  a0 <- theta[1]
  a1 <- theta[2]
  b1 <- theta[3]
  
  T <- length(y)
  
  # Initialize the conditional variances
  sig2 <- rep(NA, T + 1)
  
  # Start with unconditional variances
  sig2[1] <- a0 / (1 - a1 - b1)
  
  # Compute conditional variance at each step
  for(t in 2:(T + 1)) {
    sig2[t] <- a0 + a1 * y[t-1]^2 + b1 * sig2[t-1] #formula of conditional variance
  }
  
  # Returns the conditional variance values
  sig2
}