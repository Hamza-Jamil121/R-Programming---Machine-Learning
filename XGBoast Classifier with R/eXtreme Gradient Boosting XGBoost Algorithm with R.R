# library
library(dplyr)
library(plyr)
library(Matrix)
library(xgboost)
library(magrittr)
data = read.csv("C:\\Users\\hamza jamil\\Downloads\\binary.csv")
data$rank =as.factor(data$rank)
str(data)

# Data Partition
ind = sample(2,nrow(data),replace = TRUE,prob = c(0.8,0.2))
train = data[ind == 1,]
test = data[ind == 2,]

# create metrix  one hot encoding for factor variable train data
trainm= sparse.model.matrix(admit~.-1,data = train)
head(trainm)
train_label = train[,'admit']
train_label
train_metrix = xgb.DMatrix(data=as.matrix(trainm),label = train_label)
train_metrix

# create metrix  one hot encoding for factor variable test data
testm= sparse.model.matrix(admit~.-1,data = test)
head(testm)
test_label = test[,'admit']
train_label
test_metrix = xgb.DMatrix(data=as.matrix(testm),label = test_label)
test_metrix


nc = length(unique(train_label))
nc

xgb_params = list("objective" = "multi:softprob",
                  "eval_metric" = "mlogloss",
                  "num_class" = nc)
watchlist = list(train = train_metrix , test = test_metrix)

# Xxtreme Grading Boosting Model

bst_mode = xgb.train(params = xgb_params,
                     data = train_metrix,
                     nrounds = 100,
                     watchlist = watchlist,
                     eta=0.1  
                     )          
            


#max.depth=6, these two parameter we also pass in over model
#gamma=0)  # subsample=1 

bst_mode

# training & test errror plot
e = data.frame(bst_mode$evaluation_log)
plot(e$iter,e$train_mlogloss,col='blue')
lines(e$iter,e$test_mlogloss,col='red')
min(e$test_mlogloss)
e[e$test_mlogloss == 0.650158,]

# Featreu importNCE
imp = xgb.importance(colnames(train_metrix),model = bst_mode)
imp
xgb.plot.importance(imp)

# confusion metrix & predicton of test data
p=predict(bst_mode,newdata = test_metrix)
head(p)
pred = matrix(p,nrow = nc,ncol = length(p)/nc) %>%
        t() %>%
        data.frame() %>%
        mutate(label = test_label,max_prob = max.col(.,'last')-1)
head(pred)
table(Prediction= pred$max_prob,Actual = pred$label)
