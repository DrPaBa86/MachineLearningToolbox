library(keras)
library(tidyverse)


# download data ----
mnist <- dataset_mnist()
str(mnist)
train_images <- mnist$train$x
train_images <- array_reshape(train_images, c(60000, 28*28))
train_images <- train_images / 255
train_labels <- mnist$train$y %>% to_categorical()
test_images <- mnist$test$x
test_images <- array_reshape(test_images, c(10000, 28*28))
test_images <- test_images / 255
test_labels <- mnist$test$y %>% to_categorical()


# Visualize data in a heatmap ----
i <- 55
X <- test_images[i,]
plotdata <- expand.grid(y = 28:1, x = 1:28) %>%
  mutate(z = matrix(X, nrow = 28*28))
ggplot(plotdata, aes(x = x, y = y, fill= z)) + 
  geom_tile()
test_labels[i,]


# build the neural net ----
network <- keras_model_sequential() %>%
  layer_dense(units = 512, activation = "relu",
              input_shape = c(28*28)) %>%
  layer_dense(units = 10, activation = "softmax")

network %>% compile(
  optimizer = "rmsprop",
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)


# train the neural net ----
network %>% fit(train_images, train_labels, epochs = 5, batch_size = 128)


# evaluate the results ----
metrics <- network %>% evaluate(test_images, test_labels)
metrics


network %>% predict_classes(test_images[1:10,])

