# Machine Learning Regression Algorithms
#
# Patrick Bäurer
# 2020-04-30
#

# TASK 2
# Predict one stock price movement out of the others on the same day

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

# aim: predict delta_rel_DAI.DE on the same day
DaxPrices_prep %<>%
  rename(aim = delta_rel_DAI.DE) %>%
  select(-volume_DAI.DE, -adjusted_DAI.DE) %>%
  select(-date) %>%
  as_tibble()


# Train/Test set split ----
data <- DaxPrices_prep

#DON'T USE THIS:
#train_sample <- sample(1:nrow(data), size = round(0.8*nrow(data)))
train_sample <- 1:round(0.8*nrow(data))
data_train <- data[train_sample,]
data_test <- data[-train_sample,]
nrow(data)
nrow(data_train)
nrow(data_test)


# METHOD 0
# naive ----
data_test$pred <- data_train$aim %>% mean()
rmse(data_test$aim, data_test$pred)
mae(data_test$aim, data_test$pred)

# Conclusion
#   - MAE_test = 1,9% --> far too big for a good forecasting of stock movements


# METHOD 1
# simple linear regression ----

model_lm1 <- train(
  aim~.,
  data = data_train,
  method = "lm",
  preProcess = c("center", "scale")
)
model_lm1$results$MAE
model_lm1$results$RMSE

# MAE_train = 0,5% means biased model

data_test$pred <- predict(model_lm1, newdata = data_test)
ggplot(data_test, aes(x = aim, y = pred)) +
  geom_point()
mae(data_test$aim, data_test$pred)
rmse(data_test$aim, data_test$pred)
summary(model_lm1)$r.squared

# Conclustion:
#   - overfitted
#   - MAE_test = 1,0% --> still far too big


# METHOD 2
# glmnet ----
# reduces variance of lm by lasso and/or ridge
model_glmnet <- train(
  aim~.,
  method = "glmnet",
  data = data_train,
  trControl = trainControl(
    method = "cv",
    number = 5,
    verboseIter = TRUE
  ),
  preProcess = c("center", "scale")
)
# best model with alpha = 1 --> pure lasso
model_glmnet$results$MAE %>% mean()
model_glmnet$results$RMSE %>% mean()

# MAE_train = 0,5% means still biased


data_test$pred <- predict(model_glmnet, newdata = data_test)
ggplot(data_test, aes(x = aim, y = pred)) +
  geom_point()
mae(data_test$aim, data_test$pred)
rmse(data_test$aim, data_test$pred)

# Conclusion:
#   - still overfitting
#   - MAE_test = 0,9% --> better, but still too big


# METHOD 3
# ranger ----
model_ranger <- train(
  aim~.,
  method = "ranger",
  data = data_train,
  tuneLength = 5,
  trControl = trainControl(
    method = "cv",
    number = 5,
    verboseIter = TRUE
  ),
  preProcess = c("center", "scale")
)
plot(model_ranger)
model_ranger$results$MAE %>% mean()
model_ranger$results$RMSE %>% mean()

# MAE_train = 0,5% means still biased


data_test$pred <- predict(model_ranger, newdata = data_test)
ggplot(data_test, aes(x = aim, y = pred)) +
  geom_point()
mae(data_test$aim, data_test$pred)
rmse(data_test$aim, data_test$pred)

# Conclusion:
#   - still overfitting
#   - MAE_test = 1,0% --> worse than glmnet






# METHOD 4
# xgboost ----
model_xgboost <- train(
  aim~.,
  method = "xgbTree",
  data = data_train,
  tuneLength = 5,
  trControl = trainControl(
    method = "cv",
    number = 5,
    verboseIter = TRUE
  ),
  preProcess = c("center", "scale")
)
plot(model_xgboost)
model_xgboost$results$MAE %>% mean()
model_xgboost$results$RMSE %>% mean()

# MAE_train = 0,5% means still biased


data_test$pred <- predict(model_xgboost, newdata = data_test)
ggplot(data_test, aes(x = aim, y = pred)) +
  geom_point()
mae(data_test$aim, data_test$pred)
rmse(data_test$aim, data_test$pred)

# Conclusion:
#   - still overfitting
#   - MAE_test = 1,0% --> worse than glmnet




# NO REAL IMPROVEMENT


