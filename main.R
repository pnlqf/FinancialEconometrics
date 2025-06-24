library(readr)
library(lubridate)
library(readxl)
library(xts)
library(zoo)
library(ggplot2)
library(tidyr)
library(forecast)
library(gets)
library(gridExtra)
library(forecast)
library(forecast)
library(dynlm)
library(xts)
library(utils)
library(urca)
library(quantmod)
library(car)
library(forecast)
library(lmtest)
library(tseries)
library(FinTS)
library(fGarch)
library(dplyr)
library(patchwork)
library(rugarch)
library(vars)
library(tsDyn)
library(lmtest)
library(strucchange)
library(psych)
library(skimr)
library(tidyverse)

rm(list=ls())
setwd("C:/Users/pnata/OneDrive/Dokumenter/FinancialEconometrics")

# Load and Wrangle data
df <- read_excel("fe_dta.xlsx")
df$Date <- as.Date(df$Date, format = "%y-%m-%d")
df$Price <- as.numeric(df$Price)
df$Volume <- as.numeric(df$Volume)

# Replace 0 with NA
df$Price[df$Price == 0] <- NA
df$Volume[df$Volume == 0] <- NA

# Backfill NA values
df$Price <- na.locf(df$Price, na.rm = FALSE)
df$Volume <- na.locf(df$Volume, na.rm = FALSE)

df$logprice <- log(df$Price)
df$logvolume <- log(df$Volume)

df$logreturn <- c(NA, diff(df$logprice))
df$logvolreturn <- c(NA, diff(df$logvolume))

df <- df[-1, ]
df_xts <- xts(df[, -1], order.by = df$Date, frequency=252)

describe(df)

# Plot of price for the paper - ggplot2 formatting
pricegraph <- ggplot(df, aes(x = Date, y = logprice)) +
  geom_line(color = "black", linewidth = 0.7) +
  labs(title = "Log Price of Nikkei 225", x = "Date", y = "Log Price") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, face = "plain"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

returngraph <- ggplot(df, aes(x = Date, y = logreturn)) +
  geom_line(color = "black", linewidth = 0.7) +
  labs(title = "Return of Nikkei 225", x = "Date", y = "Log Volume") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, face = "plain"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

pricegraph / returngraph

# Which dummy to use?
model1 <- lm(formula = logreturn ~ 0 + Covid, data=df_xts)
model2 <- lm(formula = logreturn ~ 0 +Covid_L, data=df_xts)
summary(model1)
summary(model2)

# Tests for stationarity
df <- df[df$Date >= as.Date("2016-01-01") & df$Date <= as.Date("2022-12-31"), ]
df_xts <- xts(df[, -1], order.by = df$Date, frequency=252)

# Test for stationarity on both sides of the break
df1 <- df[df$Date >= as.Date("2016-01-05") & df$Date <= as.Date("2020-02-20"), ]
df2 <- df[df$Date >= as.Date("2020-03-24") & df$Date <= as.Date("2022-12-31"), ]

df_xts1 <- xts(df1[, -1], order.by = df1$Date, frequency=252)
df_xts2 <- xts(df2[, -1], order.by = df2$Date, frequency=252)

adf1 <- ur.df(df_xts1$logprice, type = "drift", selectlags = "BIC")
summary(adf1)

adf2 <- ur.df(df_xts2$logprice, type = "drift", selectlags = "BIC")
summary(adf2)

adf1 <- ur.df(df_xts1$logprice, type = "trend", selectlags = "BIC")
summary(adf1)

adf2 <- ur.df(df_xts2$logprice, type = "trend", selectlags = "BIC")
summary(adf2)

adf1 <- ur.df(df_xts1$logprice, type = "none", selectlags = "BIC")
summary(adf1)

adf2 <- ur.df(df_xts2$logprice, type = "none", selectlags = "BIC")
summary(adf2)

kpss_test <- ur.kpss(df_xts1$logprice, type = "mu")
summary(kpss_test)

kpss_test <- ur.kpss(df_xts2$logprice, type = "mu")
summary(kpss_test)

acf(df_xts1$logprice)
pacf(df_xts1$logprice)

acf(df_xts2$logprice)
pacf(df_xts2$logprice)

# Clearly non stationary in each sub-sample. 
# Test for stationarity in the full sample:
acf1 <- ggAcf(df_xts$logprice, lag.max = 50) +
  labs(title = "ACF of Log Price", x = "Lag", y = "ACF") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, face = "plain"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

pacf1 <- ggPacf(df_xts$logprice, lag.max = 50) +
  labs(title = "PACF of Log Price", x = "Lag", y = "PACF") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, face = "plain"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

acf1 / pacf1

# ADF and KPSS
adf_test <- ur.df(df_xts$logprice, type = "drift", selectlags = "BIC")
summary(adf_test)

adf_test <- ur.df(df_xts$logprice, type = "trend", selectlags = "BIC")
summary(adf_test)

adf_test <- ur.df(df_xts$logprice, type = "none", selectlags = "BIC")
summary(adf_test)

kpss_test <- ur.kpss(df_xts$logprice, type = "mu")
summary(kpss_test)

# ACF and PACF of first difference
acf2 <- ggAcf(df_xts$logreturn, lag.max = 50) +
  labs(title = "ACF of Log Returns", x = "Lag", y = "ACF") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, face = "plain"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

pacf2 <- ggPacf(df_xts$logreturn, lag.max = 50) +
  labs(title = "PACF of Log Returns", x = "Lag", y = "PACF") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, face = "plain"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

acf2 / pacf2
# Looks stationary. 

# Return graph
returngraph <- ggplot(df, aes(x = Date, y = logreturn)) +
  geom_line(color = "black", linewidth = 0.7) +
  labs(title = "Log Return of Nikkei 225", x = "Date", y = "Log Return") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, face = "plain"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

# For the assignment:
returngraph / (acf2 | pacf2)

# ADF and KPSS again, for subsample and full sample on returns

adf1 <- ur.df(df_xts1$logreturn, type = "drift", selectlags = "BIC")
summary(adf1)

adf2 <- ur.df(df_xts2$logreturn, type = "drift", selectlags = "BIC")
summary(adf2)

adf1 <- ur.df(df_xts1$logreturn, type = "trend", selectlags = "BIC")
summary(adf1)

adf2 <- ur.df(df_xts2$logreturn, type = "trend", selectlags = "BIC")
summary(adf2)

adf1 <- ur.df(df_xts1$logreturn, type = "none", selectlags = "BIC")
summary(adf1)

adf2 <- ur.df(df_xts2$logreturn, type = "none", selectlags = "BIC")
summary(adf2)

kpss_test <- ur.kpss(df_xts1$logreturn, type = "mu")
summary(kpss_test)

kpss_test <- ur.kpss(df_xts2$logreturn, type = "mu")
summary(kpss_test)

adf_test <- ur.df(df_xts$logreturn, type = "drift", selectlags = "BIC")
summary(adf_test)

adf_test <- ur.df(df_xts$logreturn, type = "trend", selectlags = "BIC")
summary(adf_test)

adf_test <- ur.df(df_xts$logreturn, type = "none", selectlags = "BIC")
summary(adf_test)

kpss_test <- ur.kpss(df_xts$logreturn, type = "mu")
summary(kpss_test)

model2 <- auto.arima(df_xts$logprice, 
                     ic = "bic",
                     seasonal = FALSE, 
                     stepwise = FALSE, 
                     approximation = FALSE, 
                     trace=FALSE,
                     xreg = df_xts$Covid)
summary(model2)

# Random walk seems to be the better choice! We proceed with the residuals
acf3 <- ggAcf(model2$residuals, lag.max = 30) +
  labs(title = "ACF of Residuals", x = "Lag", y = "ACF") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, face = "plain"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))


pacf3 <- ggPacf(model2$residuals, lag.max = 30) +
  labs(title = "PACF of Residuals", x = "Lag", y = "PACF") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, face = "plain"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

acf3 / pacf3

# White noise?
Box.test(model2$residuals, lag = 10, type = "Ljung-Box")
Box.test(model2$residuals, lag = 20, type = "Ljung-Box")
Box.test(model2$residuals, lag = 30, type = "Ljung-Box")
Box.test(model2$residuals, lag = 40, type = "Ljung-Box")
Box.test(model2$residuals, lag = 50, type = "Ljung-Box")

# ARCH Effects in the errors?
df$logretsq <- df$logreturn^2
resid <- model2$residuals
resid2 <- resid^2

logretsqplot <- ggplot(df, aes(x = Date, y = logretsq)) +
  geom_line(color = "black", linewidth = 0.7) +
  labs(title = "Log Return Squared", x = "Date", y = "Log Return Squared") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, face = "plain"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

logretplot <- ggplot(df, aes(x = Date, y = logreturn)) +
  geom_line(color = "black", linewidth = 0.7) +
  labs(title = "Log Return", x = "Date", y = "Log Return ") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, face = "plain"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

logretplot / logretsqplot
# Clearly evidence of "tranquil and volatility"

resid2plot <- ggplot(df, aes(x = Date, y = resid2)) +
  geom_line(color = "black", linewidth = 0.7) +
  labs(title = "Squared Residuals over Time", x = "Date", y = "Squared Residuals") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, face = "plain"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))


acf4 <- ggAcf(resid2 , lag.max = 30) +
  labs(title = "ACF of Squared Residuals", x = "Lag", y = "ACF") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, face = "plain"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

pacf4 <- ggPacf(resid2 , lag.max = 30) +
  labs(title = "PACF of Squared Residuals", x = "Lag", y = "PACF") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, face = "plain"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

acf4 / pacf4

resid2plot / (acf4 | pacf4)

# ARCH 1,2,3 7,8?

# Potentially ARCH3 and ARCH6 from the PACF. Try both vs. GARCH specifications
xreg <- as.matrix(df_xts$Covid)

arch1 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 0)), 
                    mean.model = list(armaOrder = c(0, 0), include.mean = FALSE, external.regressors = xreg), 
                    distribution.model = "norm")
arch1fit<-ugarchfit(spec=arch1,data=df_xts$logreturn, solver = "hybrid")

arch2 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2, 0)), 
                    mean.model = list(armaOrder = c(0, 0), include.mean = FALSE, external.regressors = xreg), 
                    distribution.model = "norm")
arch2fit<-ugarchfit(spec=arch2,data=df_xts$logreturn, solver = "hybrid")

arch3 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3, 0)), 
                    mean.model = list(armaOrder = c(0, 0), include.mean = FALSE, external.regressors = xreg), 
                    distribution.model = "norm")
arch3fit<-ugarchfit(spec=arch3,data=df_xts$logreturn, solver = "hybrid")

arch7 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(7, 0)), 
                    mean.model = list(armaOrder = c(0, 0), include.mean = FALSE, external.regressors = xreg), 
                    distribution.model = "norm")
arch7fit<-ugarchfit(spec=arch7,data=df_xts$logreturn, solver = "hybrid")

arch8 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(8, 0)), 
                    mean.model = list(armaOrder = c(0, 0), include.mean = FALSE, external.regressors = xreg), 
                    distribution.model = "norm")
arch8fit<-ugarchfit(spec=arch8,data=df_xts$logreturn, solver = "hybrid")

garch11 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                    mean.model = list(armaOrder = c(0, 0), include.mean = FALSE, external.regressors = xreg), 
                    distribution.model = "norm")
garch11fit<-ugarchfit(spec=garch11,data=df_xts$logreturn, solver = "hybrid")

garch12 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 2)), 
                      mean.model = list(armaOrder = c(0, 0), include.mean = FALSE, external.regressors = xreg), 
                      distribution.model = "norm")
garch12fit<-ugarchfit(spec=garch12,data=df_xts$logreturn, solver = "hybrid")

garch21 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2, 1)), 
                      mean.model = list(armaOrder = c(0, 0), include.mean = FALSE, external.regressors = xreg), 
                      distribution.model = "norm")
garch21fit<-ugarchfit(spec=garch21,data=df_xts$logreturn, solver = "hybrid")

garch22 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2, 2)), 
                      mean.model = list(armaOrder = c(0, 0), include.mean = FALSE, external.regressors = xreg), 
                      distribution.model = "norm")
garch22fit<-ugarchfit(spec=garch22,data=df_xts$logreturn, solver = "hybrid")

tarch_spec <- ugarchspec(
   variance.model = list(model = "fGARCH", garchOrder = c(1, 1), submodel = "TGARCH"),
   mean.model = list(armaOrder = c(0, 0), include.mean = FALSE, external.regressors = xreg),
   distribution.model = "norm"
 )

tarchfit <- ugarchfit(spec = tarch_spec, data = df_xts$logreturn, solver = "hybrid")
print(tarchfit)

# External reg is insignificant
tarch_spec <- ugarchspec(
  variance.model = list(model = "fGARCH", garchOrder = c(1, 1), submodel = "TGARCH"),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = "norm"
)

tarchfit <- ugarchfit(spec = tarch_spec, data = df_xts$logreturn, solver = "hybrid")
print(tarchfit)

# Fix normality
tarch_spec <- ugarchspec(
  variance.model = list(model = "fGARCH", garchOrder = c(1, 1), submodel = "TGARCH"),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = "sstd"
)

tarchfit <- ugarchfit(spec = tarch_spec, data = df_xts$logreturn, solver = "hybrid")
print(tarchfit)

# Compare Results
print(arch1fit)
print(arch2fit)
print(arch3fit)
print(arch7fit)
print(arch8fit)
print(garch11fit)
print(garch12fit)
print(garch21fit)
print(garch22fit)
print(tarchfit)

########################
# MULTIVARIATE TESTING #
########################

# We now want to see if Volume can help explain the returns. 
ggplot(df, aes(x = Date, y = logvolume)) +
  geom_line(color = "black", linewidth = 0.7) +
  labs(title = "Log Volume of Nikkei 225", x = "Date", y = "Log Volume") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, face = "plain"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

acf5 <- ggAcf(df_xts$logvolume, lag.max = 100) +
  labs(title = "ACF of Log Volume", x = "Lag", y = "ACF") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, face = "plain"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

pacf5 <- ggPacf(df_xts$logvolume, lag.max = 100) +
  labs(title = "PACF of Log Volume", x = "Lag", y = "PACF") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, face = "plain"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

acf5 / pacf5

adf_test <- ur.df(df_xts$logvolume, type = "none", selectlags = "BIC")
summary(adf_test)

kpss_test <- ur.kpss(df_xts$logvolume, type = "mu")
summary(kpss_test)

# Volume is not stationary. According to literature, it's trend stat:
df$trend <- as.numeric(time(df_xts))  
df$trendsq <- df$trend^2

model <- lm(logvolume ~ trend + trendsq, data = df)
summary(model)
df$logvolume_ <- residuals(model)
df_xts$logvolume_ <- df$logvolume_

plot(df_xts$logvolume_)
# Looks stationary! Let's test:
adf_test <-  ur.df(df_xts$logvolume_, type = "none", selectlags = "BIC")
summary(adf_test)

# Looks good!

# VAR (From theory, relationship is endogenous)
data <- data.frame(n225 = df_xts$logvolume_,
                   vol = df_xts$logreturn)

covid_dummy <- data.frame(Covid = df_xts$Covid)
covid_dummy_L <- data.frame(Covid = df_xts$Covid_L)

  
# Choose appropriate number of lags
lag_selection <- VARselect(data[,1:2], lag.max = 10)  
lag_selection

# 5 Seems correct - corresponds to a full week of trading! Let's try two and three
var <- VAR(data, p=2, exogen = covid_dummy)
serial.test(var)

var <- VAR(data, p=3, exogen = covid_dummy)
serial.test(var)

var <- VAR(data, p=5, exogen = covid_dummy)
serial.test(var)

# We need 5!
# Roots are stable. Very little explanation for returns unfortunately (but unexpectedly).
# returns seem to be driving volume and not the other way around. Consistent with literature

# Autocorrelation
plot(var)

# Cross-correlations as well
acf(residuals(var))
pacf(residuals(var))

# Diagnostics
serial.test(var)
arch.test(var)
normality.test(var)

# No serial correlation, but ARCH and Normality violated. This was expected
# Granger causality tests
causality(var, cause = colnames(data)[1])$Granger
causality(var, cause = colnames(data)[2])$Granger

# Volume Granger cause returns.
# Impulse response with bootstrapped confidence intervals and Cholesky decomp
irf_return_to_vol <- irf(var, impulse = "logreturn", response = "logvolume_", 
                         n.ahead = 20, ortho = TRUE, boot = TRUE)

irf_vol_to_return <- irf(var, impulse = "logvolume_", response = "logreturn",
                         n.ahead = 20, ortho = TRUE, boot = TRUE)

plot(irf_return_to_vol)
plot(irf_vol_to_return)

# Extract IRF for return -> vol
df_return_to_vol <- tibble(
  step = 0:20,
  irf = irf_return_to_vol$irf$logreturn[, "logvolume_"],
  lower = irf_return_to_vol$Lower$logreturn[, "logvolume_"],
  upper = irf_return_to_vol$Upper$logreturn[, "logvolume_"]
)

# Extract IRF for vol -> return
df_vol_to_return <- tibble(
  step = 0:20,
  irf = irf_vol_to_return$irf$logvolume_[, "logreturn"],
  lower = irf_vol_to_return$Lower$logvolume_[, "logreturn"],
  upper = irf_vol_to_return$Upper$logvolume_[, "logreturn"]
)

irf_return_to_vol_plot <- ggplot(df_return_to_vol, aes(x = step)) +
  geom_line(aes(y = irf), color = "black", linewidth = 0.7) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "blue", alpha = 0.35) +
  labs(
    title = "Return Impulse on Volume Response",
    x = "Steps ahead",
    y = "Response"
  ) +
  scale_x_continuous(breaks = seq(0, 20, by = 1)) +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "plain"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )


irf_vol_to_return_plot <- ggplot(df_vol_to_return, aes(x = step)) +
  geom_line(aes(y = irf), color = "black", linewidth = 0.7) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "blue", alpha = 0.35) +
  labs(
    title = "Volume Impulse on Return Response",
    x = "Steps ahead",
    y = "Response"
  ) +
  scale_x_continuous(breaks = seq(0, 20, by = 1)) +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "plain"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

irf_vol_to_return_plot / irf_return_to_vol_plot

#################################################################
########################### FORECASTING ########################
################################################################

df <- read_excel("fe_dta.xlsx")
df$Date <- as.Date(df$Date, format = "%y-%m-%d")
df$Price <- as.numeric(df$Price)
df$Volume <- as.numeric(df$Volume)
df$Price[df$Price == 0] <- NA
df$Volume[df$Volume == 0] <- NA
df$Price <- na.locf(df$Price)
df$Volume <- na.locf(df$Volume)
df$logprice <- log(df$Price)
df$logvolume <- log(df$Volume)
df$logreturn <- c(NA, diff(df$logprice))
df <- df[-1, ]

train <- df[df$Date >= as.Date("2016-01-01") & df$Date <= as.Date("2022-12-31"), ]
test <- df[df$Date >= as.Date("2022-12-31") & df$Date <= as.Date("2025-03-31"), ]
train_xts <- xts(train[, -1], order.by = train$Date, frequency=252)
test_xts <- xts(test[, -1], order.by = test$Date, frequency=252)

# Recreate VAR
train$trend <- as.numeric(time(train_xts))  
train$trendsq <- train$trend^2

model <- lm(logvolume ~ trend + trendsq, data = train)

train$logvolume_ <- residuals(model)
train_xts$logvolume_ <- train$logvolume_

data <- data.frame(
  logreturn = train_xts$logreturn,
  logvolume = train_xts$logvolume_
)
covid_dummy <- data.frame(Covid = train_xts$Covid)
var_model <- VAR(data, p=5, exogen = covid_dummy)
summary(var_model)


# Recreate TARCH
# Fit the EGARCH model on the full sample, with n_roll held out
tarch_spec <- ugarchspec(
  variance.model = list(model = "fGARCH", garchOrder = c(1, 1), submodel = "TGARCH"),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE), 
  distribution.model = "sstd"
)

tarch_fit <- ugarchfit(spec = tarch_spec, data = train_xts$logreturn)

# Forecasting
full_returns <- xts(df$logreturn, order.by = df$Date)

# Rolling forecast
roll <- ugarchroll(
  spec = tarch_spec,
  data = full_returns,
  n.ahead = 1,
  forecast.length = nrow(test),
  refit.every = 1,
  refit.window = "moving",  
  solver = "hybrid",
  n.start = nrow(train),   
  keep.coef = TRUE
)

roll_df <- as.data.frame(roll)

forecast_dates <- as.Date(rownames(roll_df))
actual_returns <- df$logreturn[match(forecast_dates, df$Date)]

# Forecast df
forecast_df <- data.frame(
  Date = forecast_dates,
  Return = actual_returns,
  ForecastedReturn = roll_df$Mu,
  LowerCI = roll_df$Mu - 1.96 * roll_df$Sigma,
  UpperCI = roll_df$Mu + 1.96 * roll_df$Sigma
)

# Training df
train_df <- data.frame(
  Date = train$Date,
  Return = train$logreturn,
  ForecastedReturn = NA,
  LowerCI = NA,
  UpperCI = NA
)

# Combined
combined_df <- rbind(train_df, forecast_df)

######## Rolling VAR Forecast #########
n_total <- nrow(df)
n_train <- nrow(train)
n_test <- n_total - n_train

forecasted_returns <- rep(NA, n_test)
lower_ci <- rep(NA, n_test)
upper_ci <- rep(NA, n_test)
actual_returns <- df$logreturn[(n_train + 1):n_total]
forecast_dates <- df$Date[(n_train + 1):n_total]

# Rolling loop
for (i in 1:n_test) {
  # Define window
  start_idx <- i
  end_idx <- n_train + i - 1
  
  # Get window data
  roll_data <- df[start_idx:end_idx, ]
  
  # Detrend logvolume
  roll_data$trend <- 1:nrow(roll_data)
  roll_data$trendsq <- roll_data$trend^2
  lm_fit <- lm(logvolume ~ trend + trendsq, data = roll_data)
  roll_data$logvolume_ <- residuals(lm_fit)
  
  # VAR inputs
  var_input <- data.frame(
    logreturn = roll_data$logreturn,
    logvolume = roll_data$logvolume_
  )
  
  covid_dummy <- data.frame(Covid = roll_data$Covid)
  
  # Fit VAR
  var_fit <- VAR(var_input, p = 5, exogen = covid_dummy)
  
  # Get 1-step-ahead forecast
  next_covid <- df$Covid[n_train + i]
  fcast <- predict(var_fit, n.ahead = 1, dumvar = matrix(next_covid, ncol = 1))
  
  # Extract forecast + 95% confidence interval
  forecasted_returns[i] <- fcast$fcst$logreturn[1, 1]
  lower_ci[i] <- fcast$fcst$logreturn[1, 2]
  upper_ci[i] <- fcast$fcst$logreturn[1, 3]
}

# Out-of-sample forecast df
var_forecast_df <- data.frame(
  Date = forecast_dates,
  Return = actual_returns,
  ForecastedReturn = forecasted_returns,
  LowerCI = lower_ci,
  UpperCI = upper_ci
)

# Training df
var_train_df <- data.frame(
  Date = df$Date[1:n_train],
  Return = df$logreturn[1:n_train],
  ForecastedReturn = NA,
  LowerCI = NA,
  UpperCI = NA
)

# Combined
combined_var_df <- rbind(var_train_df, var_forecast_df)

########### Visualization ##############

tarch <- combined_df %>%
  filter(Date > as.Date("2019-12-31"))

var <- combined_var_df %>%
  filter(Date > as.Date("2019-12-31"))

tarchplot <- ggplot(tarch, aes(x = Date)) +
  geom_line(aes(y = Return), color = "black", linewidth = 0.7) +
  geom_line(aes(y = ForecastedReturn), color = "blue", linewidth = 0.7) +
  geom_ribbon(aes(ymin = LowerCI, ymax = UpperCI), fill = "purple", alpha = 0.35) +
  labs(
    title = "TARCH(1,1) Out-of-Sample Forecast",
    x = "Date", y = "Log Return"
  ) +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "plain"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

varplot <- ggplot(var, aes(x = Date)) +
  geom_line(aes(y = Return), color = "black", linewidth = 0.7) +
  geom_line(aes(y = ForecastedReturn), color = "blue", linewidth = 0.7) +
  geom_ribbon(aes(ymin = LowerCI, ymax = UpperCI), fill = "purple", alpha = 0.35) +
  labs(
    title = "VAR(5) Out-of-Sample Forecast",
    x = "Date", y = "Log Return"
  ) +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "plain"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

tarchplot / varplot

# Drop in-sample rows
tarch_out <- na.omit(combined_df[, c("Date", "Return", "ForecastedReturn")])
var_out <- na.omit(combined_var_df[, c("Date", "Return", "ForecastedReturn")])

tarch_mse <- mean((tarch_out$Return - tarch_out$ForecastedReturn)^2)
tarch_rmse <- sqrt(tarch_mse)
tarch_mae <- mean(abs(tarch_out$Return - tarch_out$ForecastedReturn))
var_mse <- mean((var_out$Return - var_out$ForecastedReturn)^2)
var_rmse <- sqrt(var_mse)
var_mae <- mean(abs(var_out$Return - var_out$ForecastedReturn))

print(tarch_mse)
print(tarch_rmse)
print(tarch_mae)
print(var_mse)
print(var_rmse)
print(var_mae)

# Diebold Mariano
e1 <- tarch_out$Return - tarch_out$ForecastedReturn  # Model 1 errors
e2 <- var_out$Return - var_out$ForecastedReturn      # Model 2 errors

# DM test (e.g., using squared error loss)
dm_result <- dm.test(e1, e2, h = 1, power = 2)  
print(dm_result)

e1sq <- e1^2
e2sq <- e2^2

d <- e1sq - e2sq
lm <- lm(d ~ 1)
coeftest(lm, df=Inf, vcov=NeweyWest)[,1:4]
coeftest(lm, df=Inf, vcov=vcovHAC)[,1:4]