data('mtcars')
head(mtcars)
force('mtcars')
View(mtcars)
str(mtcars)


#mtcars1 <- mtcars[, c(1,4,6,9)]
#names(mtcars1) <- c('miles per US gallon', 'gross horsepower', 'weight' ,'transmission')
#head(mtcars1)

library(ggplot2)
ggplot(mtcars, aes(x= hp, y= am), size =3) + 
  xlab('Gross horsepower') +
  ylab ('Transmission') + 
  ggtitle ('Gross horsepower vs Transmission')

euclidean <- function (x) {sqrt(sum(x*x))}
distance_from_plane <-function(z, w, b) {
  sum(z*w) + b
}


#El modelo de perceptr�n se define en los bucles for incrustados a continuaci�n con el bucle externo para la �poca y 
#el bucle interno para la actualizaci�n del peso de cada punto de datos. La precisi�n se calcula despu�s de cada �poca 
#para decidir si la pr�xima �poca es necesaria o no. Para cada �poca, el orden de los puntos de datos para actualizar el peso se baraja aleatoriamente 

perceptron <- function(x, y, learning_rate = 1){
  w = vector(length = ncol(x))
  b = 0
  k = 0
  R = max(apply(x, 1 ,euclidean))
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
  
s = euclidean(w)
return(list(w= w/s, b = b/s, updates = k))

#unir las columnas que nos interesan  
#ifelse  es la declaraci�n if... else 
x <- cbind (mtcars$hp, mtcars$am)
y <- ifelse (mtcars$mpg == 'Miles /(US) gallon', +1, -1)
table(y)
p <- perceptron(x,y)

force(p)

#pch y cex: formas de la observaci�n y su tama�o, respectivamente   
plot(x, cex = 0.2)
points(subset(x, y == 1), col = 'black', pch = '+', cex = 1)
points(subset(x, y == -1), col = 'red', pch = '-', cex = 1)
  
intercept = -p$b / p$w [[2]]
slope = - p$w [[1]] / p$w [[2]]
abline(intercept, slope, col = ' green')   #revisar porque no entiendo el resultado
  
#Biblioteca para simulaci�n de redes neuronales 
install.packages('RSNNS')
library('RSNNS')
  
  mtcars <- mtcars[sample(1:nrow(mtcars), length(1:nrow(mtcars))), 1:ncol(mtcars)]
  
  mtcars_values <- mtcars[, 1:4]
  
  mtcars_target <- decodeClassLabels(mtcars[,5])
  
  