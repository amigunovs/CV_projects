setwd("C:/Users/Андрей/Desktop")

source("BoL data.R")

if(!"seasonal" %in% installed.packages()) install.packages("seasonal")
if(!"ggplot2" %in% installed.packages()) install.packages("ggplot2")
if(!"ggfortify" %in% installed.packages()) install.packages("ggfortify")
if(!"urca" %in% installed.packages()) install.packages("urca")
if(!"forecast" %in% installed.packages()) install.packages("forecast")
if(!"dynlm" %in% installed.packages()) install.packages("dynlm")
if(!"stargazer" %in% installed.packages()) install.packages("stargazer")

### Seasonal adjustment package
if(!"seasonal" %in% search()) library("seasonal")

### Visualization packages
if(!"ggplot2" %in% search()) library("ggplot2")
if(!"ggfortify" %in% search()) library("ggfortify")

### Stationarity checks (df test)
if(!"urca" %in% search()) library("urca")

### for ACF
if(!"forecast" %in% search()) library("forecast")
if(!"dynlm" %in% search()) library("dynlm") #Time-series models
 
### For summary
if(!"stargazer" %in% search()) library("stargazer")

#### Setting ts format and seasonal adjustment 

data <- data %>%
  mutate(date = as.Date(time, format = "%Y.%m.%d"))

data_ts <- data %>%
  select(p_lv, p_ea, m_ea, x_lv_ea)%>%
  ts(start = c(1999, 1), frequency = 12)
m <- seas(data_ts)

data$p_lv_adj <- unlist(as.data.frame(m$p_lv$data)[1])

data$p_ea_adj <- unlist(as.data.frame(m$p_ea$data)[1])

data_ts <- data %>%
  select(p_lv_adj, p_ea_adj, m_ea, x_lv_ea) %>%
  ts(start = c(1999, 1), frequency = 12)


#### Viewing the data

#ggplot(data, aes(x = time, y = p_lv, colour = "red")) + geom_line() + theme(panel.grid.major = element_line(colour = "grey50"))
#lines(data$time, data$p_lv_adj, col="green" )
#ggplot(data, aes(x = time, y = p_lv_adj)) + geom_line() + theme(panel.grid.major = element_line(colour = "grey50"))

price_index <- data %>%
  filter(time >= "2010-01-01") %>%
    ggplot(aes(time)) + 
      geom_line(aes(y=p_lv), colour="red") +
      geom_line(aes(y=p_lv_adj), colour="green")

print(price_index)


##### Stationarity checks


log_data_ts <- log(data_ts)

log_data_ts[, "p_lv_adj"] %>%
  ur.df(type = "drift", selectlags = "AIC")%>%
  summary
# Non-stationary

log_data_ts[, "p_ea_adj"] %>%
  ur.df(type = "drift", selectlags = "AIC")%>%
  summary
# Stationary with 10% significance (I assume it is not enough)

log_data_ts[, "x_lv_ea"] %>%
  ur.df(type = "trend", selectlags = "AIC")%>%
  summary
# Non-stationary

log_data_ts[, "m_ea"] %>%
  ur.df(type = "drift", selectlags = "AIC")%>%
  summary
# Non-stationary

#### The data is non-stationary, which means that the one should start differentiating the data until it is stationary

# Taking the first difference
dlog_data_ts <- log_data_ts - stats::lag(log_data_ts, -1)
colnames(dlog_data_ts) <- colnames(log_data_ts)


#### Checking the stationarity of the new variables

dlog_data_ts[, "p_lv_adj"] %>%
  ur.df(type = "drift", selectlags = "AIC")%>%
  summary
# Stationary

dlog_data_ts[, "p_ea_adj"] %>%
  ur.df(type = "drift", selectlags = "AIC")%>%
  summary
# Stationary

dlog_data_ts[, "x_lv_ea"] %>%
  ur.df(type = "drift", selectlags = "AIC")%>%
  summary
# Stationary

dlog_data_ts[, "m_ea"] %>%
  ur.df(type = "drift", selectlags = "AIC")%>%
  summary
# Stationary

# The one can see that all the data is stationary


### Now it is time to check which AR model for Latvian exports to eurozone has a better forecasting power
# The autocorrelation plot
ggAcf(dlog_data_ts [, "x_lv_ea"])
pacf(dlog_data_ts [,"x_lv_ea"], lag.max = 10)

##### Simple AR(1) and AR(2) models for forecasting (check)
# AR(1)

ar_1 <- dlog_data_ts %>%
  dynlm(x_lv_ea ~ L(x_lv_ea, 1), data=., start = c(1999, 3)) %>%
  summary

print(ar_1)
# If the growth of Latvian exports increases by 1pp in quarter t, its growth rate in the next quarter will be lower by 0.35pp keeping everything else constant

dlog_data_ts %>%
  dynlm(x_lv_ea ~ L(x_lv_ea, 1), data=., start = c(1999, 3)) %>%
  AIC
dlog_data_ts %>%
  dynlm(x_lv_ea ~ L(x_lv_ea, 1), data=., start = c(1999, 3)) %>%
  BIC

autoplot(ar_1$residuals)
ggAcf(ar_1$residuals)

#AR(2)
#Use "end" parameter to specify the end date
ar_2 <- dlog_data_ts %>%
  dynlm(x_lv_ea ~ L(x_lv_ea, 1:2), data=., start = c(1999, 3)) %>%
  summary

print(ar_2)
# If the growth of Latvian exports increases by 1pp in quarter t, its growth rate in the next quarter will be lower by 0.41pp, keeping everything else constant
# If the growth of Latvian exports increases by 1pp in quarter t, its growth rate in quarter t+2 will be lower by 0.15pp, keeping everything else constant

dlog_data_ts %>%
  dynlm(x_lv_ea ~ L(x_lv_ea, 1:2), data=., start = c(1999, 3)) %>%
  AIC
dlog_data_ts %>%
  dynlm(x_lv_ea ~ L(x_lv_ea, 1:2), data=., start = c(1999, 3),) %>%
  BIC

autoplot(ar_2$residuals)
ggAcf(ar_2$residuals)


# Dynamic forecasting 

data_forecasted <- as.data.frame(dlog_data_ts)
colnames(data_forecasted) <- colnames(dlog_data_ts)
data_forecasted$date <- data$time[2:252]
data_forecasted$date1 <- as.numeric(data_forecasted$date)

AR1_x_lv_ea <- data_forecasted %>%
  filter( between(date, 10651, 17897))
AR1_x_lv_ea <- AR1_x_lv_ea[-c(1,2,3,6)]
AR2_x_lv_ea <- AR1_x_lv_ea
colnames(AR1_x_lv_ea) <- c("AR1_x_lv_ea", "date")
colnames(AR2_x_lv_ea) <- c("AR2_x_lv_ea", "date")

ar1_2019 <- AR1_x_lv_ea %>%
  lm(AR1_x_lv_ea ~ dplyr::lag(AR1_x_lv_ea), data=.)
summary(ar1_2019)

ar2_2019 <- AR2_x_lv_ea %>%
  lm(AR2_x_lv_ea ~ dplyr::lag(AR2_x_lv_ea) + dplyr::lag(AR2_x_lv_ea, 2), data=.)
summary(ar2_2019)

i <- 0
while (i < 12) {
  AR1_x_lv_ea <- AR1_x_lv_ea %>%
    add_row()
  n <- length(AR1_x_lv_ea$AR1_x_lv_ea)
  prediction <- last(predict(ar1_2019, AR1_x_lv_ea))
  AR1_x_lv_ea$AR1_x_lv_ea[n] <- prediction
  AR1_x_lv_ea$date[n] <- data_forecasted$date[n]
  i <- i+1
}


i <- 0
while (i < 12) {
  AR2_x_lv_ea <- AR2_x_lv_ea %>%
    add_row()
  n <- length(AR2_x_lv_ea$AR2_x_lv_ea)
  prediction <- last(predict(ar2_2019, AR2_x_lv_ea))
  AR2_x_lv_ea$AR2_x_lv_ea[n] <- prediction
  AR2_x_lv_ea$date[n] <- data_forecasted$date[n]
  i <- i+1
}

graphs <- megajoin_6("date", data_forecasted, AR1_x_lv_ea, AR2_x_lv_ea)

forecasts <- graphs %>%
  filter(date >= "2018-01-01") %>%
  ggplot(aes(date)) + 
  geom_line(aes(y=AR1_x_lv_ea), colour="red") +
  geom_line(aes(y=AR2_x_lv_ea), colour="green") +
  geom_line(aes(y=x_lv_ea), colour="blue")

print(forecasts)

# We can see that both models have approximately the same forecasting power (there's roughly no forecasting power in the long run)
# AR1 is better using BIC criteria (which is more strict), while AR2 is better using AIC criteria
# I also could have applied a more correct Newey regression tool from sandwich package to account for HAC, however, this is not important if I use the model only for forecasting
# In applied method, the true significance level and standard errors might differ

#### ADL (explaining the Latvian exports with its own lag and eurozone imports (the number of lags to be selected))

# Truncation parameter:
0.75*251^(1/3)
# This means that it makes sense to test the model with up to approximately 5 lags

# Selecting the optimal number of lags
# The number of observations should be the same
# Both AIC and BIC show that m_ea model with 1 lag is the best of considered

# I also could have considered different number of lags for exports, however, the methodology is the same

dlog_data_ts %>%
  dynlm(x_lv_ea ~ L(x_lv_ea) + L(m_ea, 0), data=., start = c(1999, 6))%>%
  AIC()

dlog_data_ts %>%
  dynlm(x_lv_ea ~ L(x_lv_ea) + L(m_ea, 0:1), data=., start = c(1999, 6))%>%
  AIC()

dlog_data_ts %>%
  dynlm(x_lv_ea ~ L(x_lv_ea) + L(m_ea, 0:2), data=., start = c(1999, 6))%>%
  AIC()

dlog_data_ts %>%
  dynlm(x_lv_ea ~ L(x_lv_ea) + L(m_ea, 0:3), data=., start = c(1999, 6))%>%
  AIC()

dlog_data_ts %>%
  dynlm(x_lv_ea ~ L(x_lv_ea) + L(m_ea, 0:4), data=., start = c(1999, 6))%>%
  AIC()

dlog_data_ts %>%
  dynlm(x_lv_ea ~ L(x_lv_ea) + L(m_ea, 0:5), data=., start = c(1999, 6))%>%
  AIC()

dlog_data_ts %>%
  dynlm(x_lv_ea ~ L(x_lv_ea) + L(m_ea, 0), data=., start = c(1999, 6))%>%
  BIC()

dlog_data_ts %>%
  dynlm(x_lv_ea ~ L(x_lv_ea) + L(m_ea, 0:1), data=., start = c(1999, 6))%>%
  BIC()

dlog_data_ts %>%
  dynlm(x_lv_ea ~ L(x_lv_ea) + L(m_ea, 0:2), data=., start = c(1999, 6))%>%
  BIC()

dlog_data_ts %>%
  dynlm(x_lv_ea ~ L(x_lv_ea) + L(m_ea, 0:3), data=., start = c(1999, 6))%>%
  BIC()

dlog_data_ts %>%
  dynlm(x_lv_ea ~ L(x_lv_ea) + L(m_ea, 0:4), data=., start = c(1999, 6))%>%
  BIC()

dlog_data_ts %>%
  dynlm(x_lv_ea ~ L(x_lv_ea) + L(m_ea, 0:5), data=., start = c(1999, 6))%>%
  BIC()

# Running the model

ADL1 <- dlog_data_ts %>%
  dynlm(x_lv_ea ~ L(x_lv_ea) + L(m_ea, 0:1), data=.)
summary(ADL1)

#Short-term coefficients
# If the growth of Latvian exports increases by 1pp. in quarter t, the growth of exports in quarter t+1 decreases by 0.38pp. , keeping everything else constant
# If the growth of eurozone imports increases by 1pp. in quarter t, the growth of Latvian exports increases by 0.50pp during the same quarter, keeping everything else constant
# If the growth of eurozone imports increases by 1pp. in quarter t, the growth of Latvian exports increases by 0.66pp in the next quarter, keeping everything else constant

# Long-term coefficient
sum(ADL1$coefficients[3:4])/(1-ADL1$coefficients[2])
# If the growth of eurozone imports increases by 1pp., the growth of Latvian exports increases by 0.84pp in the long-run 

#### Cointegration

cointegration <- log_data_ts %>%
  dynlm(x_lv_ea ~ m_ea + p_lv_adj + p_ea_adj, data=.)
summary(cointegration)

# The regression shows that if the eurozone imports increase by 1%, the Latvian exports will grow by 0.67%, keeping other factors constant
# If the CPI of the eurozone increases by 1%, the Latvian exports will grow by 4.88%, keeping other factors constant
# If Latvian CPI increases by 1pp, the Latvian exports to the eurozone decrease by 0.03% (not significant)

# ADF test for the residual, the correct critical values for 200 obs and 3+1 variables are 3.89, 4.18, 4.70 
# Since there are even more observations, it can be concluded that the cointegration exists with 1% significance
cointegration$residuals %>%
  ur.df(type = "none", lags = 1, selectlags = "AIC") %>%
  summary

# I also try excluding the insignificant variable
# The correct critical values are 3.47, 3.78, 4.35, the test still shows significant results, meaning that the cointegration exists and the regression is not spurious
cointegration2 <- log_data_ts %>%
  dynlm(x_lv_ea ~ m_ea + p_ea_adj, data=.)
summary(cointegration2)

# This can be interpreted as the previous regression

cointegration2$residuals %>%
  ur.df(type = "none", selectlags = "AIC") %>%
  summary
# Both approaches show that there is a cointegration (the regressions are not spurious)

# These are basic error correction models. However, they also can contain different variables
# Only L(x_lv_ea) and the residual
data_cointegration <- cbind(dlog_data_ts, cointegration$residuals)
colnames(data_cointegration) <- c(colnames(data_ts), "residual")

a <- data_cointegration %>%
  dynlm(x_lv_ea ~ L(x_lv_ea) + m_ea + p_ea_adj + L(residual), data=.)
summary(a)

# This shows that about 24% of disequilibrium is corrected during the next quarter (lag of the residual)

data_cointegration <- cbind(data_cointegration, cointegration2$residuals)
colnames(data_cointegration) <- c(colnames(data_ts), "residual1", "residual2")

b <- data_cointegration %>%
  dynlm(x_lv_ea ~ L(x_lv_ea) + m_ea + p_ea_adj + L(residual2), data=.)
summary(b)

