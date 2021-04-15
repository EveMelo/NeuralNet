# Classification task using the SOM Kohonen neural network
# ???????????????????????????????????? ?????????? ???????????????? Self-organizing Kohonen map (If you try to translate into Russian)
# Using this network, clustering tasks are solved

install.packages("kohonen")
library('kohonen')
data('wines')
str(wines)
head(wines)
View(wines)

vintages
table(vintages)

set.seed(1) 

#create matrix hexagonal SOM 
som.wines <- som(scale(wines), grid = somgrid(5, 5, 'hexagonal')) #scale: measure k-means
som.wines
getCodes(som.wines)
dim(getCodes(som.wines)) #dimension

plot(som.wines, main = 'Wine data Kohonen SOM')
# Let's take a look at the nice display of Kohonen maps
# Which turned out during the som command for the object som.wines

## Let's get rid of our beautiful graphics so as not to torture memory
graphics.off()
par(mfrow = c(1,1)) # helps many graphics into big one 
plot(som.wines, type = 'changes', main = 'Wine data SOM')

# El gráfico muestra la distancia media hasta
# unidad más cercana versus número de iteraciones

# A continuación, establezcamos un conjunto de entrenamiento de 150 líneas
# y los 27 restantes se utilizarán como muestra de prueba
# ?? ?????????????????? 27 ?????????? ???????????????????????????? ?? ???????????????? ???????????????? ??????????????
#150/177

# to choose with "sample"

train <- sample(nrow(wines), 150)
X_train <- scale(wines[train,]) #seleccionar la tabla train, respeta filas y columnas 
X_train <- scale(wines[1:150,])
X_train <- scale(wines[1:150,1])
X_train

#not have value/importance/meaning
X_test <- scale(wines[-train,],
                center = attr(X_train, "scaled:center"),
                scale = attr(X_train, "scaled:center"))
train_data <- list(measurements = X_train,
                   vintages = vintages[train]) #vintages is a vector
test_data <- list(measurements = X_test,
                  vintages = vintages[-train])
#comand of library (kohanen)
mygrid <- somgrid(5, 5, 'hexagonal')

som.wines <- supersom(train_data, grid = mygrid)   #Can include exceptions             
plot(som.wines, type = 'changes', main = 'Wine data SOM')
plot(som.wines, main = 'Wine data Kohonen SOM')


som.predict <- predict(som.wines, newdata = test_data)
table(vintages[-train], som.predict$predictions[['vintages']]) #confusion matrix
map(som.wines)
som.predict$predictions[['vintages']]

#som.predict$vintages
plot(som.wines, main = 'Wine data Kohonen SOM')
plot(som.wines, type = 'changes', main = 'Wine data SOM')

???????????? ?????? ?????????????????? ?????????????????? ????????
plot(som.wines, main = 'Wine data Kohonen SOM')
