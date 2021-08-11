setwd("C:/Users/Андрей/Desktop")

source("BoL data.R")

### Seasonal adjustment package
library(seasonal)

### Visualization packages
library(ggplot2)
library(ggfortify)

### Stationarity checks (df test)
library(urca)

### for ACF
library(forecast)


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


log_data_ts[, "p_ea_adj"] %>%
  ur.df(type = "drift", selectlags = "AIC")%>%
  summary


log_data_ts[, "x_lv_ea"] %>%
  ur.df(type = "drift", selectlags = "AIC")%>%
  summary


log_data_ts[, "m_ea"] %>%
  ur.df(type = "drift", selectlags = "AIC")%>%
  summary

#### The data is non-stationary, which means that the one should start differentiating the data until it is stationary

# Taking the first difference
dlog_data_ts <- log_data_ts - stats::lag(log_data_ts, -1)
colnames(dlog_data_ts) <- colnames(log_data_ts)


#### Checking the stationarity of the new variables

dlog_data_ts[, "p_lv_adj"] %>%
  ur.df(type = "drift", selectlags = "AIC")%>%
  summary


dlog_data_ts[, "p_ea_adj"] %>%
  ur.df(type = "drift", selectlags = "AIC")%>%
  summary


dlog_data_ts[, "x_lv_ea"] %>%
  ur.df(type = "drift", selectlags = "AIC")%>%
  summary


dlog_data_ts[, "m_ea"] %>%
  ur.df(type = "drift", selectlags = "AIC")%>%
  summary

# The one can see that the data is stationary

# The autocorrelation plot
ggAcf(dlog_data_ts)

##### Simple AR(1) and AR(2) models for 


