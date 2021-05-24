install.packages('devtools')
library('devtools')
devtools::install_github("rstudio/tensorflow")
devtools::install_github("rstudio/keras")

install.packages('tensorflow')
install.packages('keras')

library('keras')
library('tensorflow')
install_keras()

mnist <- dataset_mnist()

train_images <- mnist$train$x
train_labels <- mnist$train$y
test_images <- mnist$test$x
test_labels <- mnist$test$y

network <- keras_model_sequential() %>%
  layer_dense(units = 512, activation = 'relu', input_shape = c(28*28)) %>%
  layer_dense(units = 10, activation = 'softmax')

network %>% compile(
  optimizer = 'rmsprop',
  loss = 'categorical_crossentropy',
  metrics = c('accuracy')
)

# Inicialmente, las matrices tienen dimensiones de 60000, 28, 28, los valores en sí varían de 0 a 255
# para entrenar la red neuronal, debe transformar la forma 60000, 28 * 28 y convertir los valores a una dimensión de 0 a 1
train_images <- array_reshape(train_images, c(60000, 28*28)) # ???????????? ?????????????????????? ?? ??????????????
train_images <- train_images/255 # ???????????? ?????????????? ????????????????
str(train_images)
test_images <- array_reshape(test_images, c(10000, 28*28))
test_images <- test_images/255

# crear categorías para atajos
train_labels  <- to_categorical ( train_labels )
test_labels  <- to_categorical ( test_labels )

# campo de preparación de datos entrenar la red neuronal

network %>% fit(train_images, train_labels, epochs = 10, batch_size = 128)


# la precisión del modelo fue del 98,9%

metric <- network %>% evaluate(test_images, test_labels)
metric

network %>% predict_classes(test_images[1:15,])
test_labels1 <- mnist$test$y
test_labels1[1:15]

history <- network %>% fit(train_images, train_labels,
                           epochs = 5, batch_size = 128,
                           validation_split = 0.2)

a <- mnist$test$x[7, 1:28, 1:28]
a
image(as.matrix(a))


test_a <- array_reshape(a, c(1, 28*28))
test_a
dim(test_a)
test_a <- test_a/255
network %>% predict_classes(test_a)

