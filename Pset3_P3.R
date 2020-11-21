

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
er_ff <- c(Return.annualized(ff_ts$HML.FF, scale = 12, geometric = F), Return.annualized(ff_ts$HML.Devil, scale = 12, geometric = F), Return.annualized(ff_ts$MKT, scale = 12, geometric = F))
sd_ff <- c(StdDev(ff_ts$HML.FF), StdDev(ff_ts$HML.Devil), StdDev(ff_ts$MKT))
sharpe_ff <- c(
  (Return.annualized(ff_ts$HML.FF, scale = 12)-(mean(ff_ts$RF)*12))/StdDev(ff_ts$HML.FF),
  (Return.annualized(ff_ts$HML.Devil, scale = 12)-(mean(ff_ts$RF)*12))/StdDev(ff_ts$HML.Devil),
  (Return.annualized(ff_ts$HML.MKT, scale = 12)-(mean(ff_ts$RF)*12))/StdDev(ff_ts$HML.MKT),
)

## b)
## Running 2 regressions using HML_Standard and HML_Devil
ff_standard <- lm(MKT~SMB+HML.FF+RMW+CMA, data = ff_ts)
ff_devil <- lm(MKT~SMB+HML.Devil+RMW+CMA, data = ff_ts)

## c) 
## buying stock with lowest B/P ratio and holding for 16 months
lowest_BP <- min(t(c(data_task1[1, "AMD_Book_val_per_sh"]/data_task1[1,"AMD_PX"], data_task1[1, "BA_Book_val_per_sh"]/data_task1[1,"BA_PX"], data_task1[1, "GILD_Book_val_per_sh"]/data_task1[1,"GILD_PX"], data_task1[1, "WMT_Book_val_per_sh"]/data_task1[1,"WMT_PX"])))

## Boeing has lowest B/P ration at start of time series

