cereal <- read.csv(file.choose())
head(cereal)
nrow(cereal)
#constructing a neural network for cereal rating

#creating index
samplesize = 0.60 * nrow(cereal)#splitting the cereal data set to extract 60% of the data

set.seed(80)

index = sample( seq_len ( nrow ( cereal ) ), size = samplesize )

index

#creating training and test set with the index

train = cereal[index,]
test = cereal[-index,]

#scale the data for analysis

max = apply(cereal , 2 , max)
min = apply(cereal, 2 , min)
max
min

scaled = as.data.frame(scale(cereal, center = min, scale = max - min))

library(neuralnet)

trainNN = scaled[index , ]
testNN = scaled[-index , ]

set.seed(20)
NN = neuralnet(rating ~ calories + protein + fat + sodium + fiber, trainNN, hidden = 3 , linear.output = T )

plot(NN)

predict_testNN = compute(NN, testNN[,c(1:5)])
predict_testNN = predict_testNN$net.result * (max(cereal$rating) - min(cereal$rating)) + min(cereal$rating)
plot(predict_testNN)

plot(test$rating, predict_testNN, col='blue', pch=16, ylab = "predicted rating NN", xlab = "real rating")
