### TOF PS3 Task 2

library(ggplot2)
library(xts)
library(dplyr)
library(fBasics)
library(timeSeries)
library(PerformanceAnalytics)
library(quantmod)
library(NMOF)
library(stats)
library(tseries)
library(reshape2)

data_task2 <- read.csv("PS3_task2.csv", header = T, nrows = 1130, skip = 15)

## a) Time_Series regression of each portfolio on the three Farma French factors

#Writing each portfolio
portfolio_1 <- ts(data_task2$SMALL.LoBM)
portfolio_2 <- ts(data_task2$ME1.BM2)
portfolio_3 <- ts(data_task2$SMALL.HiBM)
portfolio_4 <- ts(data_task2$BIG.LoBM)
portfolio_5 <- ts(data_task2$ME2.BM2)
portfolio_6 <- ts(data_task2$BIG.HiBM)

#Writing the Farma French factors as a time series
ff_1_market_excess_return <- ts(data_task2$Mkt.RF)
ff_2_SMB <- ts(data_task2$SMB)
ff_3_HML <- ts(data_task2$HML)

#Writing the Risk Free rate as a time series
rf <- (data_task2$RF)

#Regressions
regression_1 <- lm(portfolio_1-rf ~ ff_1_market_excess_return + ff_2_SMB + ff_3_HML)
regression_2 <- lm(portfolio_2-rf ~ ff_1_market_excess_return + ff_2_SMB + ff_3_HML)
regression_3 <- lm(portfolio_3-rf ~ ff_1_market_excess_return + ff_2_SMB + ff_3_HML)
regression_4 <- lm(portfolio_4-rf ~ ff_1_market_excess_return + ff_2_SMB + ff_3_HML)
regression_5 <- lm(portfolio_5-rf ~ ff_1_market_excess_return + ff_2_SMB + ff_3_HML)
regression_6 <- lm(portfolio_6-rf ~ ff_1_market_excess_return + ff_2_SMB + ff_3_HML)

## b) Plot of the mean excess returns versus the mean excess returns predicted by the model

#Computing the excess returns for each portfolio
excess_returns_1 <- portfolio_1 - rf
excess_returns_2 <- portfolio_2 - rf
excess_returns_3 <- portfolio_3 - rf
excess_returns_4 <- portfolio_4 - rf
excess_returns_5 <- portfolio_5 - rf
excess_returns_6 <- portfolio_6 - rf

#Computing the mean excess returns
mean_excess_1 <- mean(excess_returns_1)
mean_excess_2 <- mean(excess_returns_2)
mean_excess_3 <- mean(excess_returns_3)
mean_excess_4 <- mean(excess_returns_4)
mean_excess_5 <- mean(excess_returns_5)
mean_excess_6 <- mean(excess_returns_6)

#Computing the predicted excess returns for each portfolio
predicted_returns_1 <- (regression_1$coefficients[2]*ff_1_market_excess_return+
                          regression_1$coefficients[3]*ff_2_SMB+
                          regression_1$coefficients[4]*ff_3_HML)
predicted_returns_2 <- (regression_2$coefficients[2]*ff_1_market_excess_return+
                          regression_2$coefficients[3]*ff_2_SMB+
                          regression_2$coefficients[4]*ff_3_HML)
predicted_returns_3 <- (regression_3$coefficients[2]*ff_1_market_excess_return+
                          regression_3$coefficients[3]*ff_2_SMB+
                          regression_3$coefficients[4]*ff_3_HML)
predicted_returns_4 <- (regression_4$coefficients[2]*ff_1_market_excess_return+
                          regression_4$coefficients[3]*ff_2_SMB+
                          regression_4$coefficients[4]*ff_3_HML)
predicted_returns_5 <- (regression_5$coefficients[2]*ff_1_market_excess_return+
                          regression_5$coefficients[3]*ff_2_SMB+
                          regression_5$coefficients[4]*ff_3_HML)
predicted_returns_6 <- (regression_6$coefficients[2]*ff_1_market_excess_return+
                          regression_6$coefficients[3]*ff_2_SMB+
                          regression_6$coefficients[4]*ff_3_HML)

#Computing the mean predicted excess returns
mean_predicted_1 <- mean(predicted_returns_1)
mean_predicted_2 <- mean(predicted_returns_2)
mean_predicted_3 <- mean(predicted_returns_3)
mean_predicted_4 <- mean(predicted_returns_4)
mean_predicted_5 <- mean(predicted_returns_5)
mean_predicted_6 <- mean(predicted_returns_6)

#Plotting the excess returns and the predicted excess returns
excess_returns <- c(mean_excess_1, mean_excess_2, mean_excess_3, mean_excess_4,
                    mean_excess_5, mean_excess_6)
predicted_returns <- c(mean_predicted_1, mean_predicted_2, mean_predicted_3, 
                       mean_predicted_4, mean_predicted_5, mean_predicted_6)

Portfolios <- c("SMALL.LoBM", "ME1.BM2", "SMALL.HiBM", "BIG.LoBM", "ME2.BM2", "BIG.HiBM")

returns_table <- data.frame(excess_returns, predicted_returns, Portfolios)
rownames(returns_table) <- c("SMALL.LoBM", "ME1.BM2", "SMALL.HiBM", "BIG.LoBM", "ME2.BM2", "BIG.HiBM")

ggplot(melt(returns_table, id.vars = c("predicted_returns", "Portfolios"))) + 
  aes(x = predicted_returns, y = value, shape = Portfolios) +
  geom_point(size = 4) + 
  geom_abline(a = 0, b= 1, colour = "blue") +
  labs(title = "FF Model") +
  xlab("Predicted avg excess returns in %") + ylab("Actual avg excess returns in %")

     