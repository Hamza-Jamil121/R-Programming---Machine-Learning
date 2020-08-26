
data= read.csv("C:\\Users\\hamza jamil\\Downloads\\binary.csv")
head(data)
str(data)
# here we convert over two column in factor
data$admit=as.factor(data$admit)
data$rank=as.factor(data$rank)
str(data)

#Two way table of factor variable
xtabs(~admit+rank,data = data)
#partition of ddata
set.seed(1234)
ind= sample(2,nrow(data),replace = TRUE,prob = c(0.8,0.2))
train = data[ind ==1,]
test = data[ind == 2,]

#logistic regression Model
mod = glm(admit~gpa+rank,data=train,family = 'binomial')
mod #here we drop gre

summary(mod)

# here we predict over mmodel
p1=predict(mod,train,type = 'response')
head(p1)
head(train)
y=-2.8979  +(0.9119*3.67)+(1*-1.5808)
y
exp(y)/(1+exp(y))
y=-2.8979  +(0.9119*4)
y
exp(y)/(1+exp(y))

#Misclassification erro -- train data
pred1 = ifelse(p1>0.5,1,0)
tc1=table(Predicted=pred1,Actual=train$admit)
tc1
1-sum(diag(tc1))/sum(tc1)
#Misclassification erro -- test data

p2=predict(mod,test,type = 'response')
pred2 = ifelse(p2>0.5,1,0)
tc2=table(Predicted=pred2,Actual=test$admit)
tc2
1-sum(diag(tc2))/sum(tc2)

# Goodness-of-fit test
with(mod, pchisq(null.deviance - deviance, df.null-df.residual, lower.tail = F))
#this is p value 1.450537e-06