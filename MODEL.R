
library(dplyr)
library(ggplot2)
library(stringr)
library(DT)
library(tidyr)
library(knitr)      # web widget
library(tidyverse)  # data manipulation
library(data.table) # fast file reading
library(caret)      # rocr analysis
library(ROCR)       # rocr analysis
library(kableExtra) # nice table html formating 
library(gridExtra)  # arranging ggplot in grid
library(arules)

orders <- read.csv('C:/Users/valen/OneDrive/Desktop/CAPSTONE PROJECT 2020/Insta_Dataset/orders.csv')
products <- read.csv('C:/Users/valen/OneDrive/Desktop/CAPSTONE PROJECT 2020/Insta_Dataset/products.csv')
order_products_train <- read.csv('C:/Users/valen/OneDrive/Desktop/CAPSTONE PROJECT 2020/Insta_Dataset/ordertrain.csv')
order_products_prior <- read.csv('C:/Users/valen/OneDrive/Desktop/CAPSTONE PROJECT 2020/Insta_Dataset/orderprior.csv')
aisles <- read.csv('C:/Users/valen/OneDrive/Desktop/CAPSTONE PROJECT 2020/Insta_Dataset/aisles.csv')
departments <- read.csv('C:/Users/valen/OneDrive/Desktop/CAPSTONE PROJECT 2020/Insta_Dataset/departments.csv')


users_orders_products_ <- orders %>% inner_join(order_products_prior) %>% # inner_join with prior table will filter out train orders 
  left_join(products) %>% 
  left_join(aisles) %>% 
  left_join(departments) %>% 
  arrange(user_id, order_number) %>% 
  select(user_id, order_id, order_number, order_dow, order_hour_of_day, 
         days_since_prior_order, product_id, product_name, reordered, 
         add_to_cart_order, department_id, aisle_id, department, aisle)



# update this variable for changing split ratio
train_proportion = 0.7
# build list of all users ID
tmp = orders %>% filter(eval_set=='train') %>% distinct(user_id)
# 70/30 split
set.seed(12345)
train.rows = sample( 1:nrow(tmp), train_proportion * nrow(tmp) )
train.users = tmp[train.rows,]  # select training rows, list of train users
test.users  = tmp[-train.rows,] # select testing rows, list of test users
cat("Total Rows in Training Users: ", length(train.users),
    "\nTotal Rows in Testing Users: ", length(test.users),
    "\nTrain/Test Split % : ", 100*length(train.users)/(length(test.users)+length(train.users)),
    " / ", 100*length(test.users)/(length(test.users)+length(train.users)))

##TRAINING DATA CONSTRUCT
# list of products in the final order, this make up the label
construct1 = orders %>%    
  filter(user_id %in% train.users, eval_set=='train') %>% 
  left_join(order_products_train) %>%
  distinct(user_id, product_id) %>%
  mutate(actual=1)  #training label
# list of products each users had bought before in prior orders
construct2 = orders %>%   
  filter(user_id %in% train.users, eval_set=='prior') %>% 
  left_join(order_products_prior) %>%
  distinct(user_id,product_id)
# Training Construct
train.construct = left_join(construct2,construct1) %>%
  mutate(key=paste(user_id,product_id,sep="-")) %>%  # key
  select(key, user_id, product_id, actual) %>%
  arrange(user_id, product_id) %>%
  replace_na(list(actual = 0)) # proudcts not in last order, but exist in prior order
#  drop_na # remove proudcts not in historical but appear in last order
rm(list=c('construct1','construct2'))
head(train.construct,50)


##TESTING DATA CONSTRUCT
# list of products in the final order, this make up the label
construct1 = orders %>%    
  filter(user_id %in% test.users, eval_set=='train') %>% 
  left_join(order_products_train) %>%
  distinct(user_id, product_id) %>%
  mutate(actual=1)  #training label

# list of products each users had bought before in prior orders
construct2 = orders %>%   
  filter(user_id %in% test.users, eval_set=='prior') %>% 
  left_join(order_products_prior) %>%
  distinct(user_id,product_id)

# Training Construct
test.construct = left_join(construct2,construct1) %>%
  mutate(key=paste(user_id,product_id,sep="-")) %>%  # key
  select(key, user_id, product_id, actual) %>%
  arrange(user_id, product_id) %>%
  replace_na(list(actual = 0)) # proudcts not in last order, but exist in prior order
#  drop_na # remove proudcts not in historical but appear in last order

rm(list=c('construct1','construct2'))
head(test.construct,50)



#MODEL EVALUATION AND OPTIMIZATION
## Custom Function For Binary Class Performance Evaluation
binclass_eval = function (actual, predict) {
  cm = table(as.integer(actual), as.integer(predict), dnn=c('Actual','Predicted'))
  ac = (cm['1','1']+cm['0','0'])/(cm['0','1'] + cm['1','0'] + cm['1','1'] + cm['0','0'])
  pr = cm['1','1']/(cm['0','1'] + cm['1','1'])
  rc = cm['1','1']/(cm['1','0'] + cm['1','1'])
  fs = 2* pr*rc/(pr+rc)
  list(cm=cm, recall=rc, precision=pr, fscore=fs, accuracy=ac)
}


### Cutoff Threshold Optimization
optimize_cutoff = function (actual, probability) {
  rocr.pred = prediction(predictions = probability, labels = actual)
  rocr.metrics = data.frame(
    cutoff   = rocr.pred@cutoffs[[1]],
    accuracy = (rocr.pred@tp[[1]] + rocr.pred@tn[[1]]) / 
      (rocr.pred@tp[[1]] + rocr.pred@tn[[1]] + rocr.pred@fp[[1]] + rocr.pred@fn[[1]]),
    tpr = rocr.pred@tp[[1]] / (rocr.pred@tp[[1]] + rocr.pred@fn[[1]]),
    fpr = rocr.pred@fp[[1]] / (rocr.pred@fp[[1]] + rocr.pred@tn[[1]]),
    ppv = rocr.pred@tp[[1]] / (rocr.pred@tp[[1]] + rocr.pred@fp[[1]])
  )
  rocr.metrics$fscore = 2 * (rocr.metrics$tpr * rocr.metrics$ppv) / (rocr.metrics$tpr + rocr.metrics$ppv)
  rocr.metrics$tpr_fpr = rocr.metrics$tpr / rocr.metrics$fpr
  
  ## Discovery the optimal threshold for various metrics
  rocr.best = rbind(
    best.accuracy = c(max = max(rocr.metrics$accuracy, na.rm = TRUE), cutoff=rocr.metrics$cutoff[which.max(rocr.metrics$accuracy)]),
    best.ppv = c(max = max(rocr.metrics$ppv, na.rm = TRUE), cutoff = rocr.metrics$cutoff[which.max(rocr.metrics$ppv)]),
    best.recall = c(max = max(rocr.metrics$tpr, na.rm = TRUE), cutoff = rocr.metrics$cutoff[which.max(rocr.metrics$tpr)]),
    best.fscore = c(max = max(rocr.metrics$fscore, na.rm = TRUE), cutoff = rocr.metrics$cutoff[which.max(rocr.metrics$fscore)]),
    best.tpr_fpr = c(max = max(rocr.metrics$tpr_fpr, na.rm = TRUE), cutoff = rocr.metrics$cutoff[which.max(rocr.metrics$tpr_fpr)])
  )
  
  list(metrics = rocr.metrics, best = rocr.best)
}

##Model 1 : Naive Prediction
m1.train.data = users_orders_products_ %>%
  filter(user_id %in% train.users) %>%
  group_by(user_id) %>%
  top_n(n=1, wt=order_number)  %>% #last order has the higher order_number
  select(user_id, product_id) %>% 
  mutate (predicted=1)  %>%        #predict based on last ordered, therefore 1
  full_join(train.construct) %>%   #join with train construct for items not 
                                   #predicted but in final order
  select(user_id, product_id, actual, predicted) %>%
  replace_na(list(predicted = 0))
head(m1.train.data,25)

m1.eval = binclass_eval(m1.train.data$actual, m1.train.data$predicted)
m1.eval$cm

cat("Accuracy:  ", m1.eval$accuracy,
    "\nPrecision: ", m1.eval$precision,
    "\nRecall:    ", m1.eval$recall,
    "\nFScore:    ", m1.eval$fscore)



## Build Model
m2.train.data = users_orders_products_ %>%
  filter(user_id %in% train.users) %>%
  group_by(user_id) %>%
  mutate(total_orders = max(order_number)) %>%  # total number of orders made previously
  ungroup %>% 
  select(user_id, order_id, product_id, total_orders) %>%
  group_by(user_id, product_id) %>%
  summarize(predicted=n()/max(total_orders)) %>%
  select(user_id, product_id, predicted) %>%
  full_join(train.construct) %>%  # join with train construct for items not predicted but in final order
  select(user_id, product_id, actual, predicted) %>%
  replace_na(list(predicted = 0))
head(m2.train.data,20)


### Threshold Optimization
m2.rocr = optimize_cutoff(actual = m2.train.data$actual, probability = m2.train.data$predicted)
kable(m2.rocr$best) %>% kable_styling(bootstrap_options = c("striped"))


m2.eval = binclass_eval(m2.train.data$actual, m2.train.data$predicted>0.3367347)
m2.eval$cm


cat("Accuracy:  ", m2.eval$accuracy,
    "\nPrecision: ", m2.eval$precision,
    "\nRecall:    ", m2.eval$recall,
    "\nFScore:    ", m2.eval$fscore)

### Threshold Optimization
m2.rocr = optimize_cutoff(actual = m2.train.data$actual, probability = m2.train.data$predicted)
kable(m2.rocr$best) %>% kable_styling(bootstrap_options = c("striped"))
m2.eval = binclass_eval(m2.train.data$actual, m2.train.data$predicted>0.3367347)
m2.eval$cm


cat("Accuracy:  ", m2.eval$accuracy,
    "\nPrecision: ", m2.eval$precision,
    "\nRecall:    ", m2.eval$recall,
    "\nFScore:    ", m2.eval$fscore)

##MACHINE LEARNING
#Logistic Regression
m3.train.data = users_orders_products_ %>%
  filter(user_id %in% train.users) %>%
  left_join(user_products_) %>% 
  left_join(products_) %>%
  #left_join(users_)  #user_products_ already contain user specific features
  full_join(train.construct, by=c('user_id','product_id')) %>%
  arrange(user_id, product_id) %>%
  select(-c('key','user_id','order_id', 'product_id', 'product_name', 
            'department_id', 'aisle_id', 'department','aisle', 
            'days_since_prior_order')) 
glimpse(m3.train.data)

m3.test.data = users_orders_products_ %>%
  filter(user_id %in% test.users) %>%
  left_join(user_products_) %>% 
  left_join(products_) %>%
  #left_join(users_)  #user_products_ already contain user specific features
  full_join(test.construct, by=c('user_id','product_id')) %>%
  arrange(user_id, product_id) %>%
  select(-c('key','user_id','order_id', 'product_id', 'product_name', 
            'department_id', 'aisle_id', 'department','aisle', 
            'days_since_prior_order')) 
glimpse(m3.test.data)

#Model Training
m3.fit = glm(actual ~ ., family = binomial, data = m3.train.data)

#Prediction
m3.predict = predict(m3.fit, type = 'response', newdata = m3.train.data)

#Confusion Matrix
m3.eval = binclass_eval(m3.train.data$actual, m3.predict>0.2233115)
m3.eval$cm


#ROC
rocr.auc

plot(rocr.perf,
     lwd = 3, colorize = TRUE,
     print.cutoffs.at = seq(0, 1, by = 0.1),
     text.adj = c(-0.2, 1.7),
     main = 'ROC Curve')
mtext(paste('auc : ', round(rocr.auc, 5)))

abline(0, 1, col = "red", lty = 2)










