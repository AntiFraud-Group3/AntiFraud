library(neuralnet)
library(caret) 

data("iris")
str(iris) 

set.seed(123)

indexes=createDataPartition(iris$Species, p=.85, list = F)
train = iris[indexes, ]
test = iris[-indexes, ] 

xtest = test[, -5]
ytest = test[, 5] 

nnet=neuralnet(Species~., train, hidden = c(4,3), linear.output = FALSE)

plot(nnet) 

ypred = neuralnet::compute(nnet, xtest) 
yhat = ypred$net.result
print(yhat)

yhat=data.frame("yhat"=ifelse(max.col(yhat[ ,1:3])==1, "setosa",
                              ifelse(max.col(yhat[ ,1:3])==2, "versicolor", "virginica")))

cm=confusionMatrix(as.factor(ytest), yhat$yhat)
print(cm) 
