data = read.csv('C:\\Users\\hamza jamil\\Downloads\\Cardiotocographic.csv')
data$NSPF= factor(data$NSP)
str(data)

# partition data into two part
set.seed(1234)
pd= sample(2,nrow(data),replace = TRUE,prob = c(0.8,0.2))
train = data[pd==1,]
test= data[pd==2,]

#here we create decesion tree model
library(party) # decesion tree library
tree = ctree(NSPF~LB+AC+FM,data = train,controls = ctree_control(mincriterion = 0.99,minsplit = 500) )
tree
plot(tree)

#predict
predict(tree,test,type='prob')
predict(tree,test)

# Decesion tree wiht rpart
library(rpart)
tree1 = rpart(NSPF~LB+AC+FM,data = train)
tree1
library(rpart.plot)
rpart.plot(tree1,extra = 1)
# prediction rpart
predict(tree1,test)

# misclafication error for train data
tab=table(predict(tree),train$NSPF)
print(tab)
1-sum(diag(tab))/sum(tab)
# misclafication error for test data
testpred = predict(tree,newdata=test)
tab= table(testpred,test$NSPF)
tab
1-sum(diag(tab))/sum(tab)
