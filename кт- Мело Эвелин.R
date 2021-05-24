
#      KT Neural Net in R by Evelyn Melo Yanchapaxi, E2041
#DATA PANEL: CIGAR  (1963-1992)
#Variables: 
  #state: state abbreviation
  #year: the year
  #price: price per pack of cigarettes
  #pop: population
  #pop16: population above the age of 16
  #cpi: consumer price index (1983=100)
  #ndi: per capita disposable income
  #sales: cigarette sales in packs per capita
  #pimin: minimum price in adjoining states per pack of cigarettes

# Train a neural network based on a data set Cigar
# Get from https://vincentarelbundock.github.io/Rdatasets/datasets.html 

#Installation of package and library that contains the DB
install.packages('Ecdat')
library('Ecdat')
View(Cigar)

#Set an initial seed 20 - random generator 
set.seed(20)


# Neural networks are trained using the Neuralnet, RSNNS, and Kohonen libraries

##Neuralnet
install.packages('neuralnet')
library('neuralnet')

df <- Cigar

#Scaled normalization
M_df <- apply(df, 2, max)
m_df <- apply(df, 2, min)

#Max-min onrmalization
df_scaled <- scale(df, center = m_df, scale = M_df - m_df)

#Base in my training sample data on 80% of the observations. The test data is based on the remainng  20% obs.
index <- sample(1:nrow(df), round(0.80*nrow(df)))
train_df <- as.data.frame(df_scaled[index,])
test_df <- as.data.frame(df_scaled[-index,])

n <- colnames(df)
f <- as.formula(paste('sales~', paste(n[!n %in% 'sales'], collapse = '+')))
#f <- as.formula(paste('ndi~', paste(n[!n %in% 'ndi'], collapse = '+')))

#mistake 0.8059 Steps:991 with combination 9,5,3
neural_net <- neuralnet(f, data = train_df, hidden = c(5, 4, 2), linear.output = F)
plot(neural_net)

predicted <- compute(neural_net, test_df[2:9])
print(predicted$net.result)

#Confusion matrix
prueba <- table(test_df$sales, predicted$net.result)
prueba

sum(prueba[1,])
sum(prueba[2,])

Accuracy <- (prueba[1,1] + prueba[1, 1])/sum(prueba)
Accuracy

##KOHONEN
install.packages('kohonen')
library('kohonen')
set.seed(20)

df_koh <- df[2:9]
df_koh_1 <- df[1:1]
table(df_koh)

train <- sample(nrow(df_koh), 151)
X_train <- scale(df_koh[train,])
X_test <- scale(df_koh[-train,],
                center = attr(X_train, "scaled:center"),
                scale = attr(X_train, "scaled:center"))
train_df_koh <- list(measurements = X_train,
                   df_koh_1 = df_koh_1[train,])
test_df_koh <- list(measurements = X_test,
                  df_koh_1 = df_koh_1[-train,])

mygrid <- somgrid(5, 5, 'hexagonal')
som.df_koh <- supersom(train_df_koh, grid = mygrid)             
som.predict <- predict(som.df_koh, newdata = test_df_koh)
plot(som.df_koh, type = 'changes', main ='Cigar data SOM')
plot (som.df_koh, main = 'Cigar data Kohonen SOM')

prueba_df_koh <- table(df_koh_1[-train,], som.predict$predictions$df_koh_1)

sum(prueba_df_koh[1,])
sum(prueba_df_koh[2,])


Accuracy_df_koh <- (prueba_df_koh[1,1] + prueba_df_koh[1, 1])/sum(prueba_df_koh)
Accuracy_df_koh

## RSNNS

install.packages('RSNNS')
library('RSNNS')
set.seed(20)

df_rsnns <- df[sample(1:nrow(df), length(1:nrow(df))), 
                 1:ncol(df)]

df_rsnns_values <- df_rsnns [, 2:9]
df_rsnns_target <- df_rsnns [, 1]

df_rsnns <- splitForTrainingAndTest(df_rsnns_values, df_rsnns_target, ratio = 0.2)
df_rsnns <- normTrainingAndTestSet(df_rsnns)

model <- mlp(df_rsnns$inputsTrain,
             df_rsnns$targetsTrain,
             size = 5,
             maxit = 50,
             inputsTest = df_rsnns$inputsTest,
             targetsTest = df_rsnns$targetsTest)

prueba_df_rsnns <- confusionMatrix(df_rsnns$targetsTrain, encodeClassLabels(fitted.values(model), 
                                                               method = "402040", l = 0.5, h = 0.51))
prueba_df_rsnns


sum(prueba_df_rsnns[1,])
sum(prueba_df_rsnns[2,])

Accuracy_df_rsnns <- (prueba_df_rsnns[1,1] + prueba_df_rsnns[1, 1])/sum(prueba_df_rsnns)
Accuracy_df_rsnns

#Summary classification accuracy
Accuracy
Accuracy_df_koh
Accuracy_df_rsnns

# Create a table that presents the results of each neural network (accuracy estimates)
results <- cbind(Accuracy, Accuracy_df_koh, Accuracy_df_rsnns)
rownames(results) <- c("Cigarette Consumption")
colnames(results) <- c("NeuralNet", "Kohonen", "RSNNS")
View(results)

#The accuracy of the model is not the worst. 
#Tested with test adjustments but does not improve accuracy
