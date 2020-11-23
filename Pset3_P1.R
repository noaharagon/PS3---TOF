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
library(JFE)
library(stargazer)

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
endowment_weight <- (1/(endowment_beta+1))
market_weight <- -(1-endowment_weight)
risk_free_weight <- 1-endowment_weight-market_weight

## d)
## assuming the same beta as above
expected_boeing <- c(Boeing$coefficients[2]*(0.05-0.001/12), Boeing$coefficients[2]*(-0.12-0.001/12))
price_boeing <- c((1+(expected_boeing[1]+(0.001/12)))*data_task1["2020-09-30", "BA_PX"], (1+(expected_boeing[2]+(0.001/12)))*data_task1["2020-09-30", "BA_PX"])

expected_AMD <- c(AMD$coefficients[2]*(0.05-0.001/12), AMD$coefficients[2]*(-0.12-0.001/12))
price_AMD <- c((1+(expected_AMD[1]+(0.001/12)))*data_task1["2020-09-30", "AMD_PX"], (1+(expected_AMD[2]+(0.001/12)))*data_task1["2020-09-30", "AMD_PX"])
