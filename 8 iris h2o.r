#Data IRIS which one is already into R
data("iris")
head(iris)
table(iris[,5])

iris_s <- iris[1:100, c(1,3,5)]
names(iris_s) <- c('sepal', 'petal', 'species')
head (iris_s)
table (iris_s[,3])

#Library GGplot2 for visualization
install.packages('ggplot2')
library(ggplot2)
ggplot(iris_s, aes(x=sepal, y= species), size =3) + 
  xlab('sepal lenght') +
  ylab ('petal lenght') + 
  ggtitle ('Species vs sepal and petal lenghts')

euclidean.norm <- function (x) {sqrt(sum(x*x))}
#dist(iris_s[,1], method = 'euclidean')
euclidean.norm(iris_s[,1])

distance_from_plane <-function(z, w, b) {
  sum(z*w) + b
}

#WHATS MEAN EACH VAR. 
#Create: classify
classify_linear <- function(x, w, b) {
  distances <- apply (x, 1, distance_from_plane, w, b)
  return(ifelse(distances < 0, -1, +1))
}

perceptron <- function(x, y, learning_rate = 1) {
  w = vector(length = ncol(x))
  b = 0
  k = 0
  R = max(apply(x, 1 ,euclidean.norm))
  mark.complete = TRUE
  while(mark.complete){
    mark.complete=FALSE
    yc = classify_linear(x,w,b)
    for (i in 1:nrow(x)) {
      if (y[i]!= yc[i]) {
        w = w + learning_rate * y[i] * x[i,]
        b = b + learning_rate * y[i] * R^2
        k = k + 1 
        mark.complete = TRUE
      }
    }
  }                
  
  s = euclidean.norm(w)
  return(list(w= w/s, b = b/s, updates = k))
  
  x <- cbind (iris_s$sepal, iris_s$petal)
  y <- ifelse (iris_s$species == 'setosa', +1, -1)
  table(y)
  p <- perceptron(x,y)
  
  
  ####
  
  plot(x, cex = 0.2)
  points(subset(x, y == 1), col = 'black', pch = '+', cex = 1)
  points(subset(x, y == -1), col = 'red', pch = '-', cex = 1)
  
  intercept = -p$b / p$w [[2]]
  slope = - p$w [[1]] / p$w [[2]]
  abline(intercept, slope, col = ' green')
  
  
  ### New library
  install.packages('RSNNS')
  install.packages('Rcpp')
  library('RSNNS')
  
  
  iris <- iris[sample(1:nrow(iris), length(1:nrow(iris))), 1:ncol(iris)]
  
  iris_values <- iris[, 1:4]
  
  iris_target <- decodeClassLabels(iris[,5])
  
  iris <- splitForTrainingAndTest(iris_values, iris_target, ratio = 0.15)
  
  iris <- normTrainingAndTestSet(iris)
  
  hist(iris$inputsTrain)
  
  model <- mlp (iris$inptsTrain,
                iris$targetsTrain,
                size=5,
                learnFncParams = C (0.1),
                maxit = 50,
                inputsTest = iris$inputsTest, 
                targetsTest = iris$targetsTest)
  summary(model)
  model
  weightMatrix(model) 
  
  
  par(mfrow = c(2,2))
  plotIterativeError(model)
  
  predictions <- predict(model, iris$inputsTest)                
  
  plotRegressionError(predictions[,2], iris$targetsTest[,2])
  
  confusionMatrix(iris$targetsTrain, fitted.values(model))
  confusionMatrix(iris$targetsTest, predictions)
  
  
  
  #ROC  ESCOGER 0 
  plotROC(fitted.values(model)[,2], iris$targetsTrain[,2])
  
  plotROC(predictions[,2], iris$targetsTest[,2])
  
  confusionMatrix(iris$targetsTrain, encodeClassLabels(fitted.values(model),
                                                       method = "402040", l=0.4, h=0.6))
  
  ##THIS UP ON ME - CHECK IT AGAIN
  library(NeuralNetTools)
  par(mfrow = c(1,1))
  plotnet(model)
  

  
  
##NEW LIBRARY - UPDATE R 4.0.3
install.packages('RCurl')
install.packages('bitops')
install.packages('rjson')
install.packages('jsonlite')
install.packages('statmod')
install.packages('tools')
library('RCurl')
library('bitops')
library('rjson')
library('jsonlite')
library('statmod')
library('tools')

install.packages('h2o')
library(h2o)

c1 <- h2o.init(max_mem_size = '2G', nthreads=2, ip ='localhost', port=54321)
c1 <- h2o.init(jvm_custom_args=c("sys.ai.h2o.debug.allowJavaVersions", "16"))
c1 <- h2o.init(jvm_custom_args=c("sys.ai.h2o.debug.allowJavaVersions", "16.0.1"))

#ADD DATAFRAME - WE GONNA USE THE SAME LIKE THE LAST LESSON IRIS - fuck error java

iris_d1<- h2o.deeplearning(1:4,5,
                           as.h2o(iris),
                           hidden = c(5,5),
                           export_weights_and_biases = T)
iris_d1
plot(iris_d1)

h2o.weights(iris_d1, matriz_id = 1)
h2o.weights(iris_d1, matriz_id = 2)
h2o.weights(iris_d1, matriz_id = 3)
h2o.biases(iris_d1, vector_id = 1)
h2o.biases(iris_d1, vector_id = 2)
h2o.biases(iris_d1, vector_id = 3)


plot(as.data.frame(h2o.weights(iris_d1, matrix_id = 1))[,1])

h2o.confusionMatrix(iris.d1)


pairs(iris[1:4], main = "Scatterplot matrices of Iris Data", pch=21, bg=c("red", "green"))

h2o.hit_ratio_table(iris.d1) #check it, kinda r2

h2o.r2(iris.d1)
gls=iris.lm <- h2.glm(x=2:5, y =1, training_frame = as.h2o(iris))
h2o.r2 



