# Dataset iris
data("iris")
str(iris)
library(ggplot2)
qplot(Petal.Length,Petal.Width,data = iris,color=Species)
#Suport Vector Machine

library(e1071)
mymodel=svm(Species~.,data = iris,kernel='linear')
summary(mymodel)
plot(mymodel,data = iris,
     Petal.Width~Petal.Length,
     slice = list(Sepal.Width = 3, Sepal.Length=4))

#tune model
set.seed(123)
tmodel=tune(svm,Species~.,data = iris,
     ranges = list(epsilon=seq(0,1,0.1),cost=2^(2:9)))
tmodel
plot(tmodel)
summary(tmodel)
mymodel=tmodel$best.model
summary(mymodel)
plot(mymodel,data = iris, Petal.Width~Petal.Length,
     slice = list(Sepal.Width = 3, Sepal.Length=4))
# confusiion metrix and mis_classification error
pred=predict(mymodel,iris)
pred
tab= table(Predicted=pred,Actual=iris$Species)
tab
1-sum(diag(tab))/sum(tab)
