library("here")
library("PerformanceAnalytics")
source(here("Function","f_forecast_var.R"))

# Load the data and include the index values since January 2005
indices <- load(file = here("Data","indices.rda"))

SP500_p <- prices$SP500["2005-01-01/"]
FTSE100_p <- prices$FTSE100["2005-01-01/"]

# Compute the log returns for both series
log_ret_SP500 <- PerformanceAnalytics::CalculateReturns(SP500_p, method = "log")
log_ret_FTSE100 <- PerformanceAnalytics::CalculateReturns(FTSE100_p, method = "log")

log_ret_SP500 <- log_ret_SP500[-1,]
log_ret_FTSE100 <- log_ret_FTSE100[-1,]

# Vectorization
dates_log_ret_SP500 <- (log_ret_SP500[, 1])
log_ret_SP500 <- as.numeric(log_ret_SP500)

dates_log_ret_FTSE100 <- (log_ret_FTSE100[, 1])
log_ret_FTSE100 <- as.numeric(log_ret_FTSE100)

### Static estimation of the VaR ###

# Using the first 1000 log-returns 
T = 1000
# Setting the risk level
level = 0.95 
# Computing the static VaR Forecast
staticForecast_SP500 <- f_forecast_var(log_ret_SP500[1 : T], level)
staticForecast_FTSE100 <- f_forecast_var(log_ret_FTSE100[1 : T], level)
# Extracting the variable of interest
staticForecast_SP500$VaR_Forecast[T + 1]
staticForecast_FTSE100$VaR_Forecast[T + 1]



### Backtesting ###

# Setting the rolling window
T = 1000
# Setting the risk level
level = 0.95 
# Setting a progress bar for the backtest
cat("Progress: ", 0, "%", sep="")
# Initialization
VaR_backtest_SP500 <- vector("numeric", length(log_ret_SP500) - T)
VaR_backtest_FTSE100 <- vector("numeric", length(log_ret_FTSE100) - T)
# Rolling window for backtesting, VaR forecasts
for (i in (T + 1) : length(log_ret_SP500)) {
  
  backtest_SP500 <- f_forecast_var(log_ret_SP500[(i-T):(i-1)], level)
  VaR_backtest_SP500[i-T] <- backtest_SP500$VaR_Forecast[T+1]
  backtest_FTSE100 <- f_forecast_var(log_ret_FTSE100[(i-T):(i-1)], level)
  VaR_backtest_FTSE100[i-T] <- backtest_FTSE100$VaR_Forecast[T+1]
  
  # Update the progress of the backtest
  progress <- (i- T) /(length(log_ret_SP500) - T) * 100
  cat("\rProgress: ", progress, "%", sep="")
  flush.console()
  
}


### Plotting realized returns vs VaR estimates ###

# Organizing the data
data_SP500_plot <- data.frame(RealizedReturns = log_ret_SP500[(T+1):length(log_ret_SP500)], 
                              VaR_Forecasts = VaR_backtest_SP500[1:1215])
data_FTSE100_plot <- data.frame(RealizedReturns = log_ret_FTSE100[(T+1):length(log_ret_FTSE100)], 
                                VaR_Forecasts = VaR_backtest_FTSE100[1:1215])
dates_backtest_SP500 <- dates_log_ret_SP500[(T+1):length(log_ret_SP500)]
dates_backtest_FTSE100 <- dates_log_ret_FTSE100[(T+1):length(log_ret_FTSE100)]
# Plots as png 
png(file=here("Output", "VaR_Backtest_SP500.png"))
plot(as.Date(index(dates_backtest_SP500)), data_SP500_plot$RealizedReturns, 
     type = "l", 
     col = "blue", 
     ylim = range(c(data_SP500_plot$RealizedReturns, data_SP500_plot$VaR_Forecasts)), 
     main = "SP500 log-returns and forecasted VaR", 
     xlab = "Date",
     ylab = "log (%)")
lines(as.Date(index(dates_backtest_SP500)), data_SP500_plot$VaR_Forecasts, 
      type = "l", 
      col = "red")
# Add a legend
legend("topright", legend=c("Realized Returns", "VaR Forecasts"), col=c("blue", "red"), lty=1)
dev.off()


png(file=here("Output", "VaR_Backtest_FTSE100.png"))
plot(as.Date(index(dates_backtest_SP500)), data_FTSE100_plot$RealizedReturns, 
     type = "l", 
     col = "blue", 
     ylim = range(c(data_FTSE100_plot$RealizedReturns, data_FTSE100_plot$VaR_Forecasts)), 
     main = "FTSE100 log-returns and forecasted VaR", 
     xlab = "Date", 
     ylab = "log (%)")
lines(as.Date(index(dates_backtest_SP500)), data_FTSE100_plot$VaR_Forecasts, 
      type = "l", 
      col = "red")
# Add a legend
legend("topright", legend=c("Realized Returns", "VaR Forecasts"), col=c("blue", "red"), lty=1)
dev.off()

# Save backtest results to a rda file
save(VaR_backtest_SP500, VaR_backtest_FTSE100, file = here("Output", "Backtest_Results.RData"))
