##### Libraries and wd #####


if(!"readxl" %in% installed.packages()) install.packages("readxl")
if(!"ggplot2" %in% installed.packages()) install.packages("ggplot2")
if(!"dplyr" %in% installed.packages()) install.packages("dplyr")
if(!"lmtest" %in% installed.packages()) install.packages("lmtest")
if(!"sandwich" %in% installed.packages()) install.packages("sandwich")
if(!"stargazer" %in% installed.packages()) install.packages("stargazer")
if(!"car" %in% installed.packages()) install.packages("car")
if(!"tidyverse" %in% installed.packages()) install.packages("tidyverse")
if(!"magrittr" %in% installed.packages()) install.packages("magrittr")
if(!"modelsummary" %in% installed.packages()) install.packages("modelsummary")
if(!"FactorAssumptions" %in% installed.packages()) install.packages("FactorAssumptions")
if(!"BMS" %in% installed.packages()) install.packages("BMS")

if(!"readxl" %in% search()) library("readxl")
if(!"ggplot2" %in% search()) library("ggplot2")
if(!"dplyr" %in% search()) library("dplyr")
if(!"lmtest" %in% search()) library("lmtest")
if(!"sandwich" %in% search()) library("sandwich")
if(!"stargazer" %in% search()) library("stargazer")
if(!"car" %in% search()) library("car")
if(!"tidyverse" %in% search()) library("tidyverse")
if(!"magrittr" %in% search()) library("magrittr")
if(!"modelsummary" %in% search()) library("modelsummary")
if(!"FactorAssumptions" %in% search()) library("FactorAssumptions")
if(!"BMS" %in% search()) library("BMS")

setwd("C:/Users/Àíäðåé/Desktop/MMM")
data2 <- read_excel("data.xlsx", sheet = "Sheet2")
names(data2)[1] <- "city"
data <- read_excel("data.xlsx", sheet = "Sheet3")


##### PCA datasets #####

data_environ <- data %>% 
  dplyr::select( c(q1_05,
            q1_08,
            q1_09,
            q1_10,
            q4_01) )

data_infra <- data %>% 
  dplyr::select( c(q1_01,
            q1_02,
            q1_03,
            q1_04,
            q1_06,
            q1_07,
            q6_03,
            q6_04,
            q6_05) )

data_mun <- data %>% 
  dplyr::select( c(q13_1,
            q13_2,
            q13_3,
            q13_4,
            q13_5) )

data_safe <- data %>% 
  dplyr::select( c(q2_03,
            q2_04,
            q6_02,
            q08,
            q09) )

data_trust <- data %>% 
  dplyr::select( c(q2_06,
            q2_07,
            q07,
            q11,
            q12) )

newheaders = c("Aalborg", "Amsterdam", "Ankara", "Antalya", "Antwerpen", "Athina", "Barcelona", "Belfast", "Beograd", "Berlin", "Biaystok", "Bologna", "Bordeaux", "Braga", "Bratislava", "Bruxelles", "Bucharest", "Budapest", "Burgas", "Cardiff", "Cluj-Napoca", "Diyarbakir", "Dortmund", "Dublin", "Essen", "Gdansk", "Geneve", "Glasgow", "Graz", "Groningen", "Hamburg", "Helsinki", "Irakleio", "Istanbul", "Kobenhavn", "Kosice", "Krakow", "Lefkosia", "Leipzig", "Liege", "Lille", "Lisboa", "Ljubljana", "London", "Luxembourg", "Madrid", "Malaga", "Malmö", "Malchester", "Marseille", "Miskolc", "Munich", "Naples", "Oslo", "Ostrava", "Oulu", "Oviedo", "Palermo", "Paris", "Piatra Neamt", "Podgorica", "Praha", "Rennes", "Reykjavik", "Riga", "Rome", "Rostock", "Rotterdam", "Skopje", "Sofia", "Stockholm", "Strasbourg", "Tallinn", "Tirana", "Turin", "Tyneside", "Valletta", "Verona", "Vilnius", "Warszawa", "Wien", "Zagreb", "Zurich")

##### PCA ENVIRONMENT #####
rownames(data_environ) = newheaders

kmo(data_environ, squared = FALSE)

communalities_optimal_solution(
  data_environ,
  1,
  type = "principal",
  rotate = "varimax",
  fm = "minres",
  squared = FALSE
)

pca_environ = prcomp(data_environ, center = TRUE, scale = TRUE)
dim(pca_environ$rotation)[2]

biplot(pca_environ, scale = 0)

pca_environ.var = pca_environ$sdev^2
pve=pca_environ.var/sum(pca_environ.var)
pve[1:5]

barplot(pve[1:5], xlab=" Principal Component ", ylab=" Proportion of Variance Explained ", ylim=c(0,1))
barplot(cumsum(pve[1:5]), xlab=" Principal Component ", ylab ="Cumulative Proportion of Variance Explained ", ylim=c(0,1))

loadings = pca_environ$rotation
print(loadings[order(abs(loadings[,1]), decreasing=TRUE)[1:5],1])

factor_environ <- as.data.frame(print(pca_environ$x[,1]))
names(factor_environ) <- c("environ_factor")
factor1 <-  100*(1-(factor_environ - min(factor_environ$environ_factor))/(max(factor_environ$environ_factor) - min(factor_environ$environ_factor)))

#factor1 <- tibble::rownames_to_column(factor1, "city")

##### PCA INFRASTRUCTURE #####

rownames(data_infra) = newheaders

kmo(data_infra, squared = FALSE)

communalities_optimal_solution(
  data_infra,
  1,
  type = "principal",
  rotate = "varimax",
  fm = "minres",
  squared = FALSE
)

pca_infra = prcomp(data_infra, center = TRUE, scale = TRUE)
dim(pca_infra$rotation)[2]

biplot(pca_infra, scale = 0)

pca_infra.var = pca_infra$sdev^2
pve=pca_infra.var/sum(pca_infra.var)
pve[1:9]

barplot(pve[1:9], xlab=" Principal Component ", ylab=" Proportion of Variance Explained ", ylim=c(0,1))
barplot(cumsum(pve[1:9]), xlab=" Principal Component ", ylab ="Cumulative Proportion of Variance Explained ", ylim=c(0,1))

loadings = pca_infra$rotation
print(loadings[order(abs(loadings[,1]), decreasing=TRUE)[1:9],1])

factor_infra <- as.data.frame(print(pca_infra$x[,1]))
names(factor_infra) <- c("infra_factor")
factor2 <-  100*(1-(factor_infra - min(factor_infra$infra_factor))/(max(factor_infra$infra_factor) - min(factor_infra$infra_factor)))

##### PCA MUNICIPALITY #####

rownames(data_mun) = newheaders

kmo(data_mun, squared = FALSE)

communalities_optimal_solution(
  data_mun,
  1,
  type = "principal",
  rotate = "varimax",
  fm = "minres",
  squared = FALSE
)

pca_mun = prcomp(data_mun, center = TRUE, scale = TRUE)
dim(pca_mun$rotation)[2]

biplot(pca_mun, scale = 0)

pca_mun.var = pca_mun$sdev^2
pve=pca_mun.var/sum(pca_mun.var)
pve[1:5]

barplot(pve[1:5], xlab=" Principal Component ", ylab=" Proportion of Variance Explained ", ylim=c(0,1))
barplot(cumsum(pve[1:5]), xlab=" Principal Component ", ylab ="Cumulative Proportion of Variance Explained ", ylim=c(0,1))

loadings = pca_mun$rotation
print(loadings[order(abs(loadings[,1]), decreasing=TRUE)[1:5],1])

factor_mun <- as.data.frame(print(pca_mun$x[,1]))
names(factor_mun) <- c("mun_factor")
factor3 <-  100*(1-(factor_mun - min(factor_mun$mun_factor))/(max(factor_mun$mun_factor) - min(factor_mun$mun_factor)))


##### PCA SAFETY #####

rownames(data_safe) = newheaders

kmo(data_safe, squared = FALSE)

communalities_optimal_solution(
  data_safe,
  1,
  type = "principal",
  rotate = "varimax",
  fm = "minres",
  squared = FALSE
)

pca_safe = prcomp(data_safe, center = TRUE, scale = TRUE)
dim(pca_safe$rotation)[2]

biplot(pca_safe, scale = 0)

pca_safe.var = pca_safe$sdev^2
pve=pca_safe.var/sum(pca_safe.var)
pve[1:5]

barplot(pve[1:5], xlab=" Principal Component ", ylab=" Proportion of Variance Explained ", ylim=c(0,1))
barplot(cumsum(pve[1:5]), xlab=" Principal Component ", ylab ="Cumulative Proportion of Variance Explained ", ylim=c(0,1))

loadings = pca_safe$rotation
print(loadings[order(abs(loadings[,1]), decreasing=TRUE)[1:5],1])

factor_safe <- as.data.frame(print(pca_safe$x[,1]))
names(factor_safe) <- c("safe_factor")
factor4 <-  100*(1-(factor_safe - min(factor_safe$safe_factor))/(max(factor_safe$safe_factor) - min(factor_safe$safe_factor)))


##### PCA TRUST #####

rownames(data_trust) = newheaders

kmo(data_trust, squared = FALSE)

communalities_optimal_solution(
  data_trust,
  1,
  type = "principal",
  rotate = "varimax",
  fm = "minres",
  squared = FALSE
)

pca_trust = prcomp(data_trust, center = TRUE, scale = TRUE)
dim(pca_trust$rotation)[2]

biplot(pca_trust, scale = 0)

pca_trust.var = pca_trust$sdev^2
pve=pca_trust.var/sum(pca_trust.var)
pve[1:5]

barplot(pve[1:5], xlab=" Principal Component ", ylab=" Proportion of Variance Explained ", ylim=c(0,1))
barplot(cumsum(pve[1:5]), xlab=" Principal Component ", ylab ="Cumulative Proportion of Variance Explained ", ylim=c(0,1))

loadings = pca_trust$rotation
print(loadings[order(abs(loadings[,1]), decreasing=TRUE)[1:5],1])

factor_trust <- as.data.frame(print(pca_trust$x[,1]))
names(factor_trust) <- c("trust_factor")
factor5 <-  100*(1-(factor_trust - min(factor_trust$trust_factor))/(max(factor_trust$trust_factor) - min(factor_trust$trust_factor)))


##### Binding the factors with the dataset#####

data_new <- cbind(factor1, factor2, factor3, factor4, factor5)
data_new <- tibble::rownames_to_column(data_new, "city")
data_reg <- left_join(data2, data_new, by = "city")

data_reg <- data_reg %>%
  dplyr::select(city, GDP_pc, GDP_growth, pop, pop_growth, density, environ_factor, mun_factor, safe_factor, trust_factor, infra_factor)
data_reg$GDP_growth <- as.numeric(unlist(data_reg$GDP_growth))

# Runnning a test regression

output_C <- lm(data = data_reg, GDP_growth ~ GDP_pc + pop + pop_growth + density + environ_factor + mun_factor + safe_factor + trust_factor + infra_factor ) 
vif(output_C)
summary(output_C)
bptest(output_C)
durbinWatsonTest(output_C)
output_C_rob  <- coeftest(output_C, vcov = vcovHC(output_C, type = "HC1")) 
output_C_rob
shapiro.test(output_C$residuals)

stargazer(output_C, output_C_rob,
          digits = 4,
          header = FALSE,
          type = "text",
          title = "Regression summary",
          model.numbers = FALSE,
          column.labels = c("ols1", "olsrob"))


##### Sensitivity issue #####

output_D1 <- lm(data = data_reg, GDP_growth ~ GDP_pc + pop_growth + density  + mun_factor + safe_factor + infra_factor ) 
summary(output_D1)

output_D2 <- lm(data = data_reg, GDP_growth ~ GDP_pc + density  + mun_factor + environ_factor + infra_factor ) 
summary(output_D2)

output_D3 <- lm(data = data_reg, GDP_growth ~ GDP_pc + density  + environ_factor + mun_factor + safe_factor + trust_factor + infra_factor ) 
summary(output_D3)

output_D4 <- lm(data = data_reg, GDP_growth ~ GDP_pc + pop  + environ_factor + mun_factor + safe_factor + infra_factor ) 
summary(output_D4)

output_D5 <- lm(data = data_reg, GDP_growth ~ GDP_pc + pop_growth  + environ_factor + mun_factor + trust_factor + infra_factor ) 
summary(output_D5)


stargazer(output_D1, output_D2, output_D3, output_D4, output_D5,
          digits = 4,
          header = FALSE,
          type = "text",
          title = "Regression summary",
          model.numbers = FALSE,
          column.labels = c("ols1", "ols2", "ols3", "ols4", "ols5"))

##### BMS #####

cities <- unlist(data_reg[1])
data_reg <- data_reg[-c(1)]
row.names(data_reg) = cities
data_reg <- data_reg[c(2,1,3:length(data_reg))]

reg_bms <- bms(data_reg, user.int = FALSE)
coef(reg_bms)

coef(reg_bms, std.coefs = TRUE, order.by.pip = TRUE, include.constant = TRUE)
summary(reg_bms)

topmodels.bma(reg_bms)[,1:4]
image(reg_bms)
image(reg_bms, yprop2pip = TRUE)

plotModelsize(reg_bms)

reg_bms1 = bms(data_reg, mprior = "uniform", user.int = FALSE)
plotModelsize(reg_bms1)

print(reg_bms1)

image(reg_bms)
image(reg_bms1)

# reg_bms_fixed = bms(data_reg, mprior = "fixed", mprior.size = 2, user.int = FALSE)
# plotModelsize(reg_bms_fixed)
# summary(reg_bms_fixed)
# coef(reg_bms_fixed)

# reg_bms_pip = bms(data_reg, mprior = "pip", mprior.size = c())
# coef(reg_bms_pip)
# summary(reg_bms_pip)

plotComp(reg_bms, reg_bms1)

##### Conclusions #####

# 1) The one can see that the optimal model contains no more than 4 factors (3-4 factors)
# 2) The one can also see that the two fundamental factors in all checked models are the initial GDP per capita and population (both have a negative impact on growth),
# both factors are included in all the models in both bms approaches
# 3) Next, 3 factors that might be most relevant in different factor combinations are infrastructure factor (+), municipality factor (+) and population growth factor (-)