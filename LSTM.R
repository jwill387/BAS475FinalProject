library(keras)
library(tensorflow)
use_condaenv("apple_tensorflow", required = T)

data = read.csv("credit.csv")

data = as.data.frame(t(rev(as.data.frame(t(data)))))

model <- keras_model_sequential()

model %>%
  layer_lstm(units = 256, input_shape = c(491, 1), return_sequences = TRUE) %>% 
  layer_dropout(0.25) %>%
  layer_lstm(units = 512,return_sequences = TRUE) %>%
  layer_dropout(0.25) %>%
  layer_dense(units = 256, activation = "relu") %>%
  layer_dropout(0.25) %>%
  layer_dense(units = 1, activation = "relu")

model %>% compile(optimizer = optimizer_adam(learning_rate = 3e-4), loss = "mse")


train_x = array_reshape(data$credit_in_millions[1:nrow(data)-1], c(1, nrow(data)-1, 1))
train_y = array_reshape(data$credit_in_millions[2:nrow(data)], c(1, nrow(data)-1, 1))

model %>% fit(as.array(train_x), as.array(train_y),
              epochs = 100)

