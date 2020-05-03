# Machine Learning Regression Algorithms
#
# Patrick Bäurer
# 2020-04-30
#

# TASK 2
# Predict one stock price movement out of the others on the day before

library(tidyverse)
library(magrittr)
library(Metrics)
library(caret)

# load raw data ----
DaxPrices <- read_rds(paste0(getwd(), "/Datasets/DaxPrices/DaxPrices.Rds"))


# prepare data ----
DaxPrices_prep <- DaxPrices %>%
  mutate(delta_rel = if_else(symbol == lag(symbol), (adjusted - lag(adjusted)) / adjusted, NA_real_)) %>%
  select(-c(open, high, low, close)) %>%
  drop_na() %>%
  pivot_wider(names_from = symbol, values_from = c(volume, adjusted, delta_rel)) 

DaxPrices_prep %>%
  select(date, delta_rel_DAI.DE) %>%
  ggplot(aes(x = date, y = delta_rel_DAI.DE)) +
  geom_point()

# aim: predict delta_rel_DAI.DE on the next day
DaxPrices_prep %<>%
  mutate(aim = lead(delta_rel_DAI.DE)) %>%
  drop_na() %>%
  select(-date) %>%
  scale() %>%
  as_tibble()


# METHOD 1
# simple linear regression ----
data <- DaxPrices_prep

train_sample <- sample(1:nrow(data), size = round(0.8*nrow(data)))
#train_sample <- 1:round(0.8*nrow(data))
data_train <- data[train_sample,]
data_test <- data[-train_sample,]
nrow(data)
nrow(data_train)
nrow(data_test)


# model with one predictor
model_lm1 <- lm(aim~delta_rel_DAI.DE, data = data_train)
data_test$pred <- predict(model_lm1, newdata = data_test)

ggplot(data_test, aes(x = aim, y = pred)) +
  geom_point()
rmse(data_test$aim, data_test$pred)
summary(model_lm1)$r.squared


# model with all predictors
model_lm2 <- lm(aim~., data = data_train)
data_test$pred <- predict(model_lm2, newdata = data_test)
ggplot(data_test, aes(x = aim, y = pred)) +
  geom_point()
rmse(data_test$aim, data_test$pred)
summary(model_lm2)$r.squared



# METHOD 2
# glmnet ----
model_glmnet <- train(aim~.,
                      method = "glmnet",
                      data = data_train,
                      trControl = trainControl(
                        method = "cv",
                        number = 5,
                        verboseIter = TRUE
                      ))
data_test$pred <- predict(model_glmnet, newdata = data_test)
ggplot(data_test, aes(x = aim, y = pred)) +
  geom_point()
rmse(data_test$aim, data_test$pred)


# METHOD 3
# ranger ----
model_ranger <- train(aim~.,
                      method = "ranger",
                      data = data_train,
                      tuneLength = 10,
                      trControl = trainControl(
                        method = "cv",
                        number = 5,
                        verboseIter = TRUE
                      ))
data_test$pred <- predict(model_ranger, newdata = data_test)
ggplot(data_test, aes(x = aim, y = pred)) +
  geom_point()
rmse(data_test$aim, data_test$pred)


# METHOD 4
# xgboost ----
model_xgboost <- train(aim~.,
                       method = "xgbTree",
                       data = data_train,
                       tuneLength = 10,
                       trControl = trainControl(
                         method = "cv",
                         number = 5,
                         verboseIter = TRUE
                       ))
data_test$pred <- predict(model_xgboost, newdata = data_test)
ggplot(data_test, aes(x = aim, y = pred)) +
  geom_point()
rmse(data_test$aim, data_test$pred)




# NO REAL IMPROVEMENT
#
# TASK IS PERHAPS IMPOSSIBLE

