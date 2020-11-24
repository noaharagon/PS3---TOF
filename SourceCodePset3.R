#=======================================
#Source Code for PSet3
#Authors: Noah Angara, Giovanni Magagnin
#November 2020
#=======================================
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

####'* =======PROBLEM 1======== *####

## Reading Data
data_task1 <- read.csv("PS3_task1.csv", header = T, sep = ",")
data_task1$Date <- as.Date(data_task1$Date, format = "%d.%m.%Y")
data_task1 <- xts(data_task1, order.by = data_task1$Date)
data_task1 <- data_task1[, 2:ncol(data_task1)]
storage.mode(data_task1) <- "numeric"

stock_names <- c("AMD_PX", "BA_PX", "BBVA_PX", "GILD_PX", "Maersk_PX", "RWE_PX", "WMT_PX", "DAX30_PX", "IBEX35_PX", "OMXC25_PX", "SPX_PX")
return_df <- Return.calculate(data_task1[, stock_names], method = "discrete")
excess_return_df <- return_df-0.001/12

## a)
## Running CAPM regressions for AMD, Boeing, BBVA, Gilead Sciences, Maersk, RWE & Walmart
AMD <- lm(formula = excess_return_df$AMD_PX~excess_return_df$SPX_PX, data = excess_return_df)
Boeing <- lm(formula = excess_return_df$BA_PX~excess_return_df$SPX_PX, data = excess_return_df)
BBVA <- lm(formula = excess_return_df$BBVA_PX~excess_return_df$IBEX35_PX, data = excess_return_df)
Gilead <- lm(formula = excess_return_df$GILD_PX~excess_return_df$SPX_PX, data = excess_return_df)
Maersk <- lm(formula = excess_return_df$Maersk_PX~excess_return_df$OMXC25_PX, data = excess_return_df)
RWE <- lm(formula = excess_return_df$RWE_PX~excess_return_df$DAX30_PX, data = excess_return_df)
Walmart <- lm(formula = excess_return_df$WMT_PX~excess_return_df$SPX_PX, data = excess_return_df)

## b)
## Since SML is just a line we only need 2 points: risk-free asset and market portfolio
SML_slope <- mean(return_df$SPX_PX, na.rm = T)/(1-0)
SML <- ggplot()+ geom_abline(intercept = 0.001/12, slope = SML_slope, color = "blue")+
  geom_point(aes(x = AMD$coefficients[2], y = mean(return_df$AMD_PX, na.rm = T)), color = "red", size = 3, shape = 1)+
  geom_point(aes(x = Boeing$coefficients[2], y = mean(return_df$BA_PX, na.rm = T)), color = "red", size = 3, shape = 1)+
  geom_point(aes(x = Gilead$coefficients[2], y = mean(return_df$GILD_PX, na.rm = T)), color = "red", size = 3, shape = 1)+
  geom_point(aes(x = Walmart$coefficients[2], y = mean(return_df$WMT_PX, na.rm = T)), color = "red", size = 3, shape = 1)+
  geom_text(aes(x = AMD$coefficients[2], y = mean(return_df$AMD_PX, na.rm = T)-0.003), label = "AMD", color = "black")+
  geom_text(aes(x = Boeing$coefficients[2], y = mean(return_df$BA_PX, na.rm = T)-0.003), label = "Boeing", color = "black")+
  geom_text(aes(x = Gilead$coefficients[2], y = mean(return_df$GILD_PX, na.rm = T)-0.003), label = "Gilead", color = "black")+
  geom_text(aes(x = Walmart$coefficients[2], y = mean(return_df$WMT_PX, na.rm = T)-0.003), label = "Walmart", color = "black")+
  xlim(0,2.5) + ylim(-0.01, 0.07) + ylab("Monthly Mean Return") + xlab("Beta")+ 
  ggtitle("Security Market Line for US Market") +
  theme(plot.title = element_text(hjust = 0.5))

## Since CML is just a line we only need 2 points: risk-free asset and market portfolio
CML_slope <- mean(return_df$SPX_PX, na.rm = T)/(StdDev(return_df$SPX_PX)-0)
CML <- ggplot() + geom_abline(intercept = 0.001/12, slope = CML_slope, color = "blue")+
  geom_point(aes(x = StdDev(return_df$AMD_PX), y = mean(return_df$AMD_PX, na.rm = T)), color = "red", size = 3, shape = 1)+
  geom_point(aes(x = StdDev(return_df$BA_PX), y = mean(return_df$BA_PX, na.rm = T)), color = "red", size = 3, shape = 1)+
  geom_point(aes(x = StdDev(return_df$GILD_PX), y = mean(return_df$GILD_PX, na.rm = T)), color = "red", size = 3, shape = 1)+
  geom_point(aes(x = StdDev(return_df$WMT_PX), y = mean(return_df$WMT_PX, na.rm = T)), color = "red", size = 3, shape = 1)+
  geom_text(aes(x = StdDev(return_df$AMD_PX), y = mean(return_df$AMD_PX, na.rm = T)-0.003), label = "AMD", color = "black")+
  geom_text(aes(x = StdDev(return_df$BA_PX), y = mean(return_df$BA_PX, na.rm = T)-0.003), label = "Boeing", color = "black")+
  geom_text(aes(x = StdDev(return_df$GILD_PX), y = mean(return_df$GILD_PX, na.rm = T)-0.003), label = "Gilead", color = "black")+
  geom_text(aes(x = StdDev(return_df$WMT_PX), y = mean(return_df$WMT_PX, na.rm = T)-0.003), label = "Walmart", color = "black")+
  xlim(0,0.25) + ylim(-0.01, 0.07) + ylab("Monthly Mean Return") + xlab("Volatility")+ 
  ggtitle("Capital Market Line for US Market") +
  theme(plot.title = element_text(hjust = 0.5))

## c)
## Calculating Beta of initial endowment
AMD_weight <- 12000*data_task1["2020-09-30", "AMD_PX"]/(12000*data_task1["2020-09-30", "AMD_PX"]+3500*data_task1["2020-09-30", "WMT_PX"])
WMT_weight <- 3500*data_task1["2020-09-30", "WMT_PX"]/(12000*data_task1["2020-09-30", "AMD_PX"]+3500*data_task1["2020-09-30", "WMT_PX"])
endowment_beta <- AMD_weight*AMD$coefficients[2]+WMT_weight*Walmart$coefficients[2]              

## since beta is positive we most likely need to short the market to bring down beta
endowment_weight <- 1
market_weight <- -(endowment_beta)
risk_free_weight <- 1-endowment_weight-market_weight

## d)
## assuming the same beta as above
expected_boeing <- c(Boeing$coefficients[2]*(0.05-0.001/4), Boeing$coefficients[2]*(-0.12-0.001/4))#need to take risk-free rate divided by 4 since the expected return is for 3-months
price_boeing <- c((1+(expected_boeing[1]+(0.001/4)))*data_task1["2020-09-30", "BA_PX"], (1+(expected_boeing[2]+(0.001/4)))*data_task1["2020-09-30", "BA_PX"])

expected_AMD <- c(AMD$coefficients[2]*(0.05-0.001/4), AMD$coefficients[2]*(-0.12-0.001/4))
price_AMD <- c((1+(expected_AMD[1]+(0.001/4)))*data_task1["2020-09-30", "AMD_PX"], (1+(expected_AMD[2]+(0.001/4)))*data_task1["2020-09-30", "AMD_PX"])


data_task2 <- read.csv("PS3_task2.csv", header = T, nrows = 1130, skip = 15)

####'* =======PROBLEM 2======== *####

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


####'* =======PROBLEM 3======== *####

## reading data and formatting percentages
data_task3 <- read.csv("PS3_task3.csv", header = T, sep = ";")
data_task3$DATE <- as.Date(data_task3$DATE, format = "%m/%d/%Y")
ff_ts <- xts(data_task3, order.by = data_task3$DATE)
ff_ts <- ff_ts[,2:ncol(ff_ts)]
ff_ts <- period.apply(ff_ts, endpoints(ff_ts, on = "months"), function(x) as.numeric(gsub("%", "", x)))
ff_ts <- ff_ts/100
storage.mode(ff_ts) <- "numeric"
ff_ts$MKT = ff_ts$MKT.RF+ff_ts$RF

## a)

er_ff <- c(Return.annualized(ff_ts$HML.FF+ff_ts$RF, scale = 12, geometric = F), Return.annualized(ff_ts$HML.Devil+ff_ts$RF, scale = 12, geometric = F), Return.annualized(ff_ts$MKT, scale = 12, geometric = F))
sd_ff <- c(StdDev(ff_ts$HML.FF+ff_ts$RF), StdDev(ff_ts$HML.Devil+ff_ts$RF), StdDev(ff_ts$MKT))*sqrt(12)#need to scale by sqrt(12) to get annual value
sharpe_ff <- sqrt(12)*c(
  mean(ff_ts$HML.FF)/StdDev(ff_ts$HML.FF+ff_ts$RF),
  mean(ff_ts$HML.Devil)/StdDev(ff_ts$HML.Devil+ff_ts$RF),
  mean(ff_ts$MKT.RF)/StdDev(ff_ts$MKT.RF)
) #need to scale by sqrt(12) since we get monthly sharpe ratio


## b)
## Running 2 regressions using HML_Standard and HML_Devil
ff_standard <- lm(MKT~SMB+HML.FF+RMW+CMA, data = ff_ts)
ff_devil <- lm(MKT~SMB+HML.Devil+RMW+CMA, data = ff_ts)


## c)
## calculating B/P ratio for each stock
book_price <- data.frame(data_task1$AMD_Book_val_per_sh/data_task1$AMD_PX,
                         data_task1$BA_Book_val_per_sh/data_task1$BA_PX,
                         data_task1$GILD_Book_val_per_sh/data_task1$GILD_PX,
                         data_task1$WMT_Book_val_per_sh/data_task1$WMT_PX)

## looping through book_price and get lowest value every 16 months
ep <- endpoints(data_task1, on = "months", k = 16)+1 # need to lag by 1 month to get holding period
hml_stocks <- list()
j <- 1
for (i in ep) {
  stock <- which.min(book_price[i,])
  hml_stocks[j] <- stock
  j <- j+1
}

## it looks like our strategy is BA, AMD, BA, BA, BA, lets get the return
log_return_df <- Return.calculate(data_task1[, stock_names], method = "log")

first_16 <- log_return_df[1:ep[2], "BA_PX"]
second_16 <- log_return_df[(ep[2]+1):ep[3], "AMD_PX"]
third_16 <- log_return_df[(ep[3]+1):nrow(log_return_df), "BA_PX"]

hml_standard_ts <- rbind(first_16, second_16, third_16)
hml_return <- Return.annualized(hml_standard_ts, scale = 12, geometric = F)

## getting annual return from buying and holding S&P500
SP_return <- Return.annualized(Return.calculate(data_task1$SPX_PX, method = "log"), scale = 12, geometric = F)

## d)
## Investing in stock with lowest B/P ratio each month
devil_list <- list()
l <-1
for (k in 1:nrow(book_price)) {
  small_bp <- which.min(book_price[k,])
  devil_list[l] <- small_bp
  l<- l+1
}

## now creating time series of returns based on the lowest B/P stocks we own
j<-2 #need to lag return by one month to get return of holding stock for one period
devil_strategy <- data.frame()
for (q in 1:length(devil_list)) {
  devil_strategy[j,1] <- log_return_df[j,devil_list[q]]
  j <- j+1
}

## creating xts object to calculate returns
devil_strategy$date <- index(data_task1)
devil_strategy <- xts(devil_strategy, order.by = devil_strategy$date)
devil_strategy <- devil_strategy[,1]
storage.mode(devil_strategy) <- "numeric"
Return.annualized(devil_strategy$V1, scale = 12, geometric = F)

