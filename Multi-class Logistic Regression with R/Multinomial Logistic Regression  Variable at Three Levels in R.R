
mydata = read.csv('C:\\Users\\hamza jamil\\Downloads\\Cardiotocographic.csv')
str(mydata)
mydata$NSPF = factor(mydata$NSP)
mydata$out = relevel(mydata$NSPF,ref = '1')

# Multinomial Logistic Regression with R: Categorical Response Variable at Three Levels
#model multinomial logistic regressoin
library(nnet)
mymodel = multinom(out~LB+AC+FM,data = mydata)
summary(mymodel)

#Predict
predict(mymodel,mydata,type = 'prob')#remove prob you get 0 1 2
predict(mymodel,mydata[c(3,100,400),],type = 'prob')

#Misclassification ERRO
cm = table(predict(mymodel),mydata$NSPF)
print(cm)

# Z- tailed Z test
z= summary(mymodel)$coefficients/summary(mymodel)$standard.errors
z
p = (1 - pnorm(abs(z),0,1)) * 2
p
