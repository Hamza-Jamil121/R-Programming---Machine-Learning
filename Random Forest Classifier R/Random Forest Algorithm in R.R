data = read.csv('C:\\Users\\hamza jamil\\Downloads\\CTG.csv')
str(data)
data$NSP = as.factor(data$NSP)
table(data$NSP)

# Data Partition
set.seed(123)
ind = sample(2,nrow(data),replace = TRUE,prob = c(0.7,0.3))
train = data[ind == 1,]
test = data[ind == 2,]

# Random forest model
library(randomForest)
set.seed(222)
rf = randomForest(NSP ~.,data = train,
                  ntree = 300,
                  mtry = 8,
                  importance= TRUE,
                  proximity=TRUE)
print(rf)
attributes(rf)
rf$confusion

# predictoiin & confusion metrix train data
library(caret)
p1 = predict(rf,train)
head(p1)
head(train$NSP)
confusionMatrix(p1,train$NSP)

# predictoiin & confusion metrix test data
p2 = predict(rf,test)
confusionMatrix(p2,test$NSP)

# Error rate Random forest
plot(rf)

# tune ranndom forest
t = tuneRF(train[,-22],train[,22],
           stepFactor = 0.5,
           plot = TRUE,
           ntreeTry = 300,
           trace = TRUE,
           improve = 0.05)
# no of node in tree
hist(treesize(rf),
     main = 'no of node in tree',
     col='green')

# Variable importance
varImpPlot(rf,sort= T,n.var = 10,main = 'top 10 variable')
importance(rf)
varUsed(rf)

# Partial Dependance plot
partialPlot(rf,train,ASTV,'2')

# extract single tree
getTree(rf,1,labelVar = TRUE)
# Multi dimentional scalling plot
MDSplot(rf,train$NSP)
