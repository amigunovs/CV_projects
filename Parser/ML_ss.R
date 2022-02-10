setwd("C:/Users/Àíäðåé/Desktop/R projects/Parser + machine learning")
source("Parser_ss.R")

if(!"dplyr" %in% installed.packages()) install.packages("dplyr")
if(!"corrplot" %in% installed.packages()) install.packages("corrplot")
if(!"ggplot2" %in% installed.packages()) install.packages("ggplot2")
if(!"lattice" %in% installed.packages()) install.packages("lattice")

if(!"randomForest" %in% installed.packages()) install.packages("randomForest")
if(!"fastDummies" %in% installed.packages()) install.packages("fastDummies")
if(!"caret" %in% installed.packages()) install.packages("caret")


if(!"dplyr" %in% search()) library("dplyr") # Data
if(!"corrplot" %in% search()) library("corrplot") # Plots
if(!"ggplot2" %in% search()) library("ggplot2") # Plots
if(!"lattice" %in% search()) library("lattice") # Plots

if(!"randomForest" %in% search()) library("randomForest") # To run the model
if(!"fastDummies" %in% search()) library("fastDummies") # To create dummies
if(!"caret" %in% search()) library("caret") # To train the dataset

# Summarizing the data and viewing how it looks like
summary(qi2)

ggplot(  ) + 
  geom_point( aes(x = qi2$m_sq, y = qi2$Price, color = 'red', alpha = 0.1) )

# There clearly are some outliers. To make sure the model works properly, it is better to exclude the outliers
data <- qi2%>%
  filter(Price < 375000)%>%
  filter(m_sq < 200)

table <- data

# Now the data definitely looks better
ggplot(  ) + 
  geom_point( aes(x = data$m_sq, y = data$Price, color = 'red', alpha = 0.1) )

# Which variables can be used to predict the flat/house prices? Of course, there are several possible models (and many more if we think about variables outside the data table), but I drop the following:
# I delete the description and all information about streets (as the sample size is quite small to use streets as an instrument of prediction - there are too many unique streets)
data <- data[-c(1, 9, 10)]

# Creating dummies for several variables

data <-  fastDummies::dummy_cols(data, select_columns = "Series")
data <-  fastDummies::dummy_cols(data, select_columns = "Hood")
data <- data[-c(3,7)]

# Creating a data frame with the dependent variable
y <- data$Price
y <- as.data.frame(y)
y <- y %>%
  mutate(id = row_number())

# Removing the dependent variable from the data frame with independent variables
data <- data[-c(3)]

# Checking if the row numbers are the same
nrow(data)
nrow(y)

# Splitting the sample size into training and testing parts
index <- sample(nrow(data), 2/3 * nrow(data)) 

X_train <- data[ index, ]
X_test <- data[-index, ]
y_train <- y[index,]
y_test<-y[-index,]
y_train <-y_train$y
y_test <-y_test$y

# Checking the row number
nrow(data)
nrow(y)
nrow(X_train)
nrow(X_test)
nrow(y_train)
nrow(y_test)

# Building a sample model
regr <- randomForest(x = X_train, y = y_train, maxnodes = 7, ntree = 7)

# It can predict the following values
predictions <- predict(regr, X_test)

result <- y_test
result <- as.data.frame(result)
result['prediction']<-  predictions

# Visualizing. It can be actually seen that the predictions are not that accurate
ggplot(  ) + 
  geom_point( aes(x = X_test$m_sq, y = y_test, color = 'magenta', alpha = 0.1) ) + 
  geom_point( aes(x = X_test$m_sq, y = predictions, color = 'teal',  alpha = 0.99)) + 
  labs(x = "m2", y = "Price", color = "", alpha = 'Transperency') +
  scale_color_manual(labels = c( "Predicted", "Real"), values = c("blue", "red")) 

# Goodness of fit
print(paste0('MSE: ' ,caret::postResample(predictions , y_test)['RMSE']^2 ))
print(paste0('R2: ' ,caret::postResample(predictions , y_test)['Rsquared'] ))

# Improving the model

seed <-7
metric<-'RMSE'
customRF <- list(type = "Regression", library = "randomForest", loop = NULL)
customRF$parameters <- data.frame(parameter = c("maxnodes", "ntree"), class = rep("numeric", 2), label = c("maxnodes", "ntree"))
customRF$grid <- function(x, y, len = NULL, search = "grid") {}

customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, maxnodes = param$maxnodes, ntree=param$ntree, ...)
}

customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes

# Set grid search parameters
control <- trainControl(method="repeatedcv", number=10, repeats=3, search='grid')

# Outline the grid of parameters
tunegrid <- expand.grid(.maxnodes=c(10,50,100), .ntree=c(50, 100, 300))
set.seed(seed)

# Train the model
rf_gridsearch <- train(x=X_train, y=y_train, method=customRF, metric=metric, tuneGrid=tunegrid, trControl=control)

rf_gridsearch$bestTune

print(rf_gridsearch)
plot(rf_gridsearch)

#Selecting best trained model
regr2 <- randomForest(x=X_train, y=y_train, maxnodes = 300, ntree = 700)
prediction <-predict(regr2,X_test)

result <- X_test
result['price'] <- y_test
result['prediction']<-  prediction
#rf_gridsearch$results$ntree
print(paste0('R2 tuned model: ',max(rf_gridsearch$results$Rsquared)))

# R^2 is relatively high now, and the prices are predicted quite well. However, sometimes there are other (human) factors which could explain why the price is different from the real (predicted) value.
# One of the possible reasons is the liquidity issue - a person would like to sell as fast as possible due to personal circumstances. Although this is one of the possible explanations, the model itself might be improved with new factors as well if the one finds them.
# Plotting the results. The improvement can be easily seen
varImpPlot(rf_gridsearch$finalModel, main ='Feature importance')

ggplot(  ) + 
  geom_point( aes(x = X_test$m_sq, y = y_test, color = 'red', alpha = 0.1) ) + 
  geom_point( aes(x = X_test$m_sq , y = prediction, color = 'blue',  alpha = 0.99)) + 
  labs(x = "m2", y = "Price", color = "", alpha = 'Transperency') +
  scale_color_manual(labels = c( "Predicted", "Real"), values = c("red", "blue"))

# Compare the results
result$residual <- (result$price - result$prediction)

# 
table1 <- table[index,]
table2 <- table[-index,]

table2$residual <- result$residual
table2$prediction <- result$prediction
names(table2)[5] <- "price"

# The data frame below contains the observations where the predicted (real) price is higher than the actual price.
# So, this might be a good arbitrage opportunity of buying the real estate and selling it at a real price
buy_this <- table2 %>%
  arrange(residual)%>%
  filter(residual>(-20000))
View(buy_this)

# Since I am interested in a cheaper flat, I can also select the observations within my budget

for_poor_students <- buy_this %>%
  filter(price <81000) %>%
  filter(Rooms > 1) %>%
  filter(Floor_nr > 1) %>%
  filter(Max_floors >3 )
View(for_poor_students)

# The table above contains only observations from the testing data set. However, the one can also look at the training data set, as there also might be attractive offers
# Re-creating the table with all the results


sample <- X_train

prediction2 <-predict(regr2,sample)
result2 <- sample
result2["price"] <- y_train
result2["prediction"] <- prediction2
result2["residual"] <- (result2["price"] - result2["prediction"])

table1$residual <- result2$residual
table1$prediction <- result2$prediction
names(table1)[5] <- "price"
sample <- rbind(table1, table2)

# Now the same steps can be done to view the possible mispricing in the whole sample

buy_this <- sample %>%
  arrange(residual)%>%
  filter(residual>(-20000))
View(buy_this)

# Since I am interested in a cheaper flat, I can also select the observations within my budget

for_poor_students <- buy_this %>%
  filter(price <81000) %>%
  filter(Rooms > 1) %>%
  filter(Floor_nr > 1) %>%
  filter(Max_floors >3 )
View(for_poor_students)
