# Machine Learning Regression Algorithms
#
# Patrick Bäurer
# 2020-04-30
#

# TASK 1
# Predict one stock price out of the others on the same day

library(tidyverse)
library(magrittr)
library(Metrics)

# load raw data ----
DaxPrices <- read_rds(paste0(getwd(), "/Datasets/DaxPrices/DaxPrices.Rds"))


# prepare data ----
# choose three features for each stock: adjusted, range, volume
DaxPrices_prep <- DaxPrices %>%
  mutate(range = high-low) %>%
  select(-c(open, high, low, close)) %>%
  pivot_wider(names_from = symbol, values_from = c(volume, adjusted, range))

# aim: predict adjusted_DAI.DE
DaxPrices_prep %<>%
  select(-volume_DAI.DE, -range_DAI.DE) %>%
  rename(aim = adjusted_DAI.DE)


# Train/test set split ----
data <- DaxPrices_prep

train_sample <- 1:round(0.8*nrow(data))
# DON'T USE THIS:
# train_sample <- sample(1:nrow(data), size = round(0.8*nrow(data)))
# Never learn from future data in time series machine learning problems

data_train <- data[train_sample,]
data_test <- data[-train_sample,]
nrow(data)
nrow(data_train)
nrow(data_test)



# METHOD 1
# simple linear regression ----

# model with one bad predictor
model1 <- lm(aim~adjusted_SAP.DE, data = data_train)
data_train

data_test$pred <- predict(model1, newdata = data_test)

ggplot(data_test, aes(x = date, y = aim)) +
  geom_point() +
  geom_line(data = data_test, aes(x = date, y = pred), col = "red")
ggplot(data_test, aes(x = aim, y = pred)) +
  geom_point()
rmse(data_test$aim, data_test$pred)
summary(model1)$r.squared


# model with one good predictor
model2 <- lm(aim~adjusted_BMW.DE, data = data_train)
data_test$pred <- predict(model2, newdata = data_test)

ggplot(data_test, aes(x = date, y = aim)) +
  geom_point() +
  geom_line(data = data_test, aes(x = date, y = pred), col = "red")
ggplot(data_test, aes(x = aim, y = pred)) +
  geom_point()
rmse(data_test$aim, data_test$pred)
summary(model2)$r.squared


# model with all predictors
model3 <- lm(aim~.-date, data = data_train)
data_test$pred <- predict(model3, newdata = data_test)
ggplot(data_test, aes(x = date, y = aim)) +
  geom_point() +
  geom_line(data = data_test, aes(x = date, y = pred), col = "red")
ggplot(data_test, aes(x = aim, y = pred)) +
  geom_point()
rmse(data_test$aim, data_test$pred)
summary(model3)$r.squared



# CONCLUSION
#
# Simple LM is good enough for this task

