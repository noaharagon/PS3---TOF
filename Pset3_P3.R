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


