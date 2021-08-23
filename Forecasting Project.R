library(fpp2)

# importing and viewing data
ur <- read_excel("Desktop/econ_138/Unemployment Rate in LA.xls")
View(ur)
lf <- read_excel("Desktop/econ_138/Civilian Labor Force in LA.xls")
View(lf)
gp <- read_excel("Desktop/econ_138/Gasoline Prices Per Gallon in LA.xls")
View(gp)

# viewing summary statistics
summary_ur <- summary(ur)
summary(lf)
summary(gp)

# plotting graphs to see how they look
autoplot(ts(ur['urla'], start = 1990, frequency = 12)) +
  ggtitle('Unemployment rate in Los Angeles vs. Time') +
  xlab('Time') + ylab('Unemployment rate')
autoplot(ts(lf['clb'], start = 1990, frequency = 12)) +
  ggtitle('Labor Force in Los Angeles vs. Time') +
  xlab('Time') + ylab('Labor Force')
autoplot(ts(gp['gp'], start = 1990, frequency = 12)) +
  ggtitle('Gas Prices in Los Angeles vs. Time') +
  xlab('Time') + ylab('Gas Prices Per Gallon')

# loging gp graph to reduce variance
new_gp <- log(gp['gp'])
new_gp         
autoplot(ts(new_gp)) + 
  ggtitle(' Log Gas Prices in Los Angeles vs. Time') +
  xlab('Time') + ylab('Gas Prices Per Gallon')

# creating time series or ur and creating test and training data
ur_ts <- ts(ur['urla'], start = 1990, frequency = 12)
lf_ts <- ts(lf['clb'], start = 1990, frequency = 12)
gp_ts <- ts(gp['gp'], start = 1990, frequency = 12)

ur_train <- window(ur_ts, end = c(2013, 12))
View(ur_train)
ur_test <- window(ur_ts, start = 2014)
View(ur_test)

# plotting to make sure I did it okay
autoplot(ur_ts) +
  autolayer(ur_train, series = 'Training') +
  autolayer(ur_test, series = 'Test')

ur_train_snaive <- snaive(ur_train, h = 12 * 7)
autoplot(ur_train) + 
  autolayer(ur_train_snaive)

accuracy(ur_, ur_test)
checkresiduals(ur_ts)
checkresiduals(ur_train_snaive)

# Time series decmposition
ur_ts_mult = decompose(ur_ts, type="multiplicative")
autoplot(ur_ts)

datastl = stl(ur_ts, t.window=13, s.window="periodic", robust=TRUE)
autoplot(ur_ts_stl)
View(ur_ts)

# ses
ur_ts_ses = ses(ur_ts, h = 4)
ur_ts_ses
autoplot(ur_ts_ses)

# arima
ur_ts_arima <- auto.arima(ur_ts, stepwise = FALSE,  approximation = FALSE)
ur_ts_arima
checkresiduals(ur_ts_arima)
autoplot(forecast(ur_ts_arima, h = 10))

# dynamic regression
regressors <- cbind(lf_ts, gp_ts)

fit <- auto.arima(ur_ts, xreg = regressors, stepwise = FALSE, approximation = FALSE)
fit
checkresiduals(fit)

xpred <- cbind(rep(mean(lf_ts),12),rep(mean(gp_ts),12))
xpred
autoplot(forecast(fit, xreg = xpred))
(forecast(fit, xreg = xpred))
