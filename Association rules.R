
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



tmp <-order_products_train %>% 
  group_by(product_id) %>% 
  left_join(products,by="product_id")

write.csv(tmp, file = "transactions.csv")
transactions<-read.transactions("transactions.csv", format = "single", sep = ",",cols = c(2,6))

summary(transactions)


inspect(transactions[1:3])


groceryrules <- apriori(transactions, parameter = list(support = 0.001, confidence = 0.25))
summary(groceryrules)

inspect(groceryrules[1:10])

