library('neuralnet')
install.packages('ISLR')

library(ISLR)

data<-College
View(College)

max_data <-apply(data[,2:18], 2, max)
min_data <-apply(data[,2:18], 2, min)
head(data)

#Normalization data
data_scaled <- scale(data[,2:18], center = min_data, scale = max_data - min_data)

Private <- as.numeric(College$Private)-1
table (Private)
data_scaled <- cbind(Private, data_scaled)
View(data_scaled)

#Make up data before analysis
#200 observaciones la muestra puede ser 80% / 70% 30 +- size sample is good

index <- sample(1:nrow(data), round(0.70*nrow(data)))
index

train_data <- as.data.frame(data_scaled[index,])
test_data <- as.data.frame(data_scaled[-index,])

n <- names(data)
f <- as.formula(paste('Private~', paste (n[!n %in% 'Private'], collapse='+')))
#Private ~ a+b

#Check again which funtion worked it
d_net<-neuralnet(f,data=train_data,hidden = c(5,3), linear.output = F)

plot(d_net)

#predic regression
predicted <- compute(d_net, test_data[2:18])
print(head(predicted$net.result)) 



predicted$net.result <- sapply(predicted$net.result,round, digits = 0)

test1<- table(test_data$Private, predicted$net.result)
test1


table(test_data$Private)

sum(test1[1,])
sum(test1[2,])


Accuracy <- (test1[1,1] + test1[2,2])/(sum(test1))
Accuracy 
