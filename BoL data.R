if(!"eurostat" %in% installed.packages()) install.packages("eurostat")
if(!"dplyr" %in% installed.packages()) install.packages("dplyr")
if(!"tidyr" %in% installed.packages()) install.packages("tidyr")


if(!"eurostat" %in% search()) library("eurostat")
if(!"dplyr" %in% search()) library("dplyr")
if(!"tidyr" %in% search()) library("tidyr")

##### Functions

megajoin_6 <- function(join_parameter, df1, df2, df3, df4, df5, df6){
  megajoined <- left_join(df1, df2, by=join_parameter)
  if(missing(df3) == FALSE){
    megajoined <- left_join(megajoined, df3, by=join_parameter)
  }
  if(missing(df4) == FALSE){
    megajoined <- left_join(megajoined, df4, by=join_parameter)
  }
  if(missing(df5) == FALSE){
    megajoined <- left_join(megajoined, df5, by=join_parameter)
  }
  if(missing(df6) == FALSE){
    megajoined <- left_join(megajoined, df6, by=join_parameter)
  }
  
  return(megajoined)
}

##### Getting the data

### Imports and exports of the Euro zone
  
trade_data = get_eurostat("ext_st_ea19sitc", filters = list(stk_flow = c("EXP", "IMP"), partner = c("EA19", "EXT_EA19"), sitc06 = c("TOTAL"), indic_et = c("TRD_VAL", "TRD_VAL_SCA")))

a <- trade_data %>%
  filter(stk_flow =="IMP")
a1 <- a %>%
  filter(indic_et == "TRD_VAL")
a2 <- a %>%
  filter(indic_et != "TRD_VAL")
a11<- a1 %>%
  filter(partner == "EA19")
a11 <- a11[-c(1:5)]
names(a11)[2] <- "IMP_Unadj_internal"
a12 <- a1 %>%
  filter(partner != "EA19")
a12 <- a12[-c(1:5)]
names(a12)[2] <- "IMP_Unadj_external"
a21 <- a2 %>%
  filter(partner == "EA19")
a21 <- a21[-c(1:5)]
names(a21)[2] <- "IMP_Adj_internal"
a22 <- a2 %>%
  filter(partner != "EA19")
a22 <- a22[-c(1:5)]
names(a22)[2] <- "IMP_Adj_external"


b <- trade_data %>%
  filter(stk_flow != "IMP")
b1 <- b %>%
  filter(indic_et == "TRD_VAL")
b2 <- b %>%
  filter(indic_et != "TRD_VAL")
b11<- b1 %>%
  filter(partner == "EA19")
b12 <- b1 %>%
  filter(partner != "EA19")
b21 <- b2 %>%
  filter(partner == "EA19")
b22 <- b2 %>%
  filter(partner != "EA19")
b11 <- b11[-c(1:5)]
names(b11)[2] <- "EXP_Unadj_internal"
b12 <- b12[-c(1:5)]
names(b12)[2] <- "EXP_Unadj_external"
b21 <- b21[-c(1:5)]
names(b21)[2] <- "EXP_Adj_internal"
b22 <- b22[-c(1:5)]
names(b22)[2] <- "EXP_Adj_external"


a <- megajoin_6("time", a11, a12, a21, a22)
b <- megajoin_6("time", b11, b12, b21, b22)
data <- megajoin_6("time", a, b)
remove(trade_data, a11, a12, a21, a22, b11, b12, b21, b22, a1, a2, b1, b2, a, b)


### Price indexes
price_index = get_eurostat("prc_hicp_midx", filters = list(unit = "I15", coicop = "CP00", geo = c("EA", "LV")))
a <- price_index %>%
  filter(geo == "LV")
b <- price_index %>%
  filter(geo != "LV")
a <- a[-c(1:3)]
names(a)[2] <- "p_lv"
b <- b[-c(1:3)]
names(b)[2] <- "p_ea"

data <- megajoin_6("time", data, a, b)
remove(price_index, a, b)

### Latvian exports
lv_exports = get_eurostat("ei_etea19_m", filters = list(stk_flow = "EXP", indic = "ET-T", geo = "LV", unit = "MIO-EUR-SA"))
lv_exports <- lv_exports[-c(1:5)]
names(lv_exports)[2] <- "x_lv_ea"
data <- megajoin_6("time", data, lv_exports)
remove(lv_exports)

data <- data %>%
  select(time, IMP_Adj_external, IMP_Unadj_internal) %>%
  mutate(data, m_ea = IMP_Adj_internal + IMP_Adj_external)
data <- data[-c(2:9)]

data <- data %>%
  filter(time <= "2019-12-01")