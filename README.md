# VaR Backtesting Project

This project centers on the backtesting of Value at Risk (VaR) measures for the S&P 500 and FTSE 100 financial indices using the GARCH(1,1)-Normal model. The approach entails computing log returns for these indices, estimating VaR using a static approach, then conducting a rolling window backtest.

## Prerequisites

For this project to run successfully on your machine you need to have R installed as well as the following packages in R:

- `here`

- `PerformanceAnalytics`

## Files Description

- `Var_Backtest.R`: This is the main script that undertakes VaR computations and backtesting. It consists of loading data, calculating log returns, making VaR estimates, doing backtesting, plotting results, and saving outcomes.

- `f_forecast_var.R`: Contains the `f_forecast_var`, `f_nll` and `f_ht` functions that computes the VaR forecast of a GARCH(1,1) model with normal errors at the desired risk level, computes the negative log likelihood value and computes the vector of conditional variance, respectively. These functions are important for both static Var estimation and rolling window backtesting process.

- `Data/`: This directory has the file known as ‘indices.rda’ which contains historical data on S&P 500 and FTSE 100 indices.


- `Output/`: Plots of the backtesting results will be stored in this folder along with a data set containing all relevant information about each one’s performance.
