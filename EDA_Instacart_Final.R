library(data.table)
library(dplyr)
library(ggplot2)
library(knitr)
library(stringr)
library(DT)

orders <- read.csv('C:/Users/valen/OneDrive/Desktop/CAPSTONE PROJECT 2020/Insta_Dataset/orders.csv')
products <- read.csv('C:/Users/valen/OneDrive/Desktop/CAPSTONE PROJECT 2020/Insta_Dataset/products.csv')
order_products_train <- read.csv('C:/Users/valen/OneDrive/Desktop/CAPSTONE PROJECT 2020/Insta_Dataset/ordertrain.csv')
order_products_prior <- read.csv('C:/Users/valen/OneDrive/Desktop/CAPSTONE PROJECT 2020/Insta_Dataset/orderprior.csv')
aisles <- read.csv('C:/Users/valen/OneDrive/Desktop/CAPSTONE PROJECT 2020/Insta_Dataset/aisles.csv')
departments <- read.csv('C:/Users/valen/OneDrive/Desktop/CAPSTONE PROJECT 2020/Insta_Dataset/departments.csv')

kable(head(orders,5))
glimpse(orders)

kable(head(products,5))
glimpse(products)

kable(head(order_products_train,5))
glimpse(order_products_train)

kable(head(order_products_prior,5))
glimpse(order_products_prior)

kable(head(aisles,5))
glimpse(aisles)

kable(head(departments,5))
glimpse(departments)


orders <- orders %>% mutate(order_hour_of_day = as.numeric(order_hour_of_day), eval_set = as.factor(eval_set))
products <- products %>% mutate(product_name = as.factor(product_name))
aisles <- aisles %>% mutate(aisle = as.factor(aisle))
departments <- departments %>% mutate(department = as.factor(department))

orders %>% 
  ggplot(aes(x=order_hour_of_day)) + 
  geom_histogram(stat="count",fill="pink")


orders %>% 
  ggplot(aes(x=order_dow)) + 
  geom_histogram(stat="count",fill="pink")

orders %>% 
  ggplot(aes(x=days_since_prior_order)) + 
  geom_histogram(stat="count",fill="pink")

orders %>% filter(eval_set=="prior") %>% count(order_number) %>% 
  ggplot(aes(order_number,n)) + 
  geom_line(color="blue", size=1)+
  geom_point(size=2, color="blue")

order_products_train %>% 
  group_by(order_id) %>% 
  summarize(n_items = last(add_to_cart_order)) %>%
  ggplot(aes(x=n_items))+
  geom_histogram(stat="count",fill="pink") + 
  geom_rug()+
  coord_cartesian(xlim=c(0,80))

tmp <- order_products_train %>% 
  group_by(product_id) %>% 
  summarize(count = n()) %>% 
  top_n(10, wt = count) %>%
  left_join(select(products,product_id,product_name),by="product_id") %>%
  arrange(desc(count)) 
kable(tmp)

tmp %>% 
  ggplot(aes(x=reorder(product_name,-count), y=count))+
  geom_bar(stat="identity",fill="pink")+
  theme(axis.text.x=element_text(angle=90, hjust=1),axis.title.x = element_blank())

tmp <- order_products_train %>% 
  group_by(reordered) %>% 
  summarize(count = n()) %>% 
  mutate(reordered = as.factor(reordered)) %>%
  mutate(proportion = count/sum(count))
kable(tmp)

tmp %>% 
  ggplot(aes(x=reordered,y=count,fill=reordered))+
  geom_bar(stat="identity") +scale_fill_manual(values=c("#696969","#006400"))

tmp <-order_products_train %>% 
  group_by(product_id) %>% 
  summarize(proportion_reordered = mean(reordered), n=n()) %>% 
  filter(n>40) %>% 
  top_n(10,wt=proportion_reordered) %>% 
  arrange(desc(proportion_reordered)) %>% 
  left_join(products,by="product_id")
kable(tmp)


tmp %>% 
  ggplot(aes(x=reorder(product_name,-proportion_reordered), y=proportion_reordered))+
  geom_bar(stat="identity",fill="pink")+
  theme(axis.text.x=element_text(angle=90, hjust=1),axis.title.x = element_blank())+coord_cartesian(ylim=c(0.85,0.95))

tmp <- order_products_train %>% 
  group_by(product_id, add_to_cart_order) %>% 
  summarize(count = n()) %>% mutate(pct=count/sum(count)) %>% 
  filter(add_to_cart_order == 1, count>10) %>% 
  arrange(desc(pct)) %>% 
  left_join(products,by="product_id") %>% 
  select(product_name, pct, count) %>% 
  ungroup() %>% 
  top_n(10, wt=pct)
kable(tmp)

tmp %>% 
  ggplot(aes(x=reorder(product_name,-pct), y=pct))+
  geom_bar(stat="identity",fill="pink")+
  theme(axis.text.x=element_text(angle=90, hjust=1),axis.title.x = element_blank())+coord_cartesian(ylim=c(0.4,0.7))


order_products_train %>% 
  left_join(orders,by="order_id") %>% 
  group_by(days_since_prior_order) %>%
  summarize(mean_reorder = mean(reordered)) %>%
  ggplot(aes(x=days_since_prior_order,y=mean_reorder))+
  geom_bar(stat="identity",fill="pink")

order_products_train %>% 
  group_by(product_id) %>% 
  summarize(proportion_reordered = mean(reordered), n=n()) %>%
  ggplot(aes(x=n,y=proportion_reordered))+
  geom_point()+
  geom_smooth(color="red")+
  coord_cartesian(xlim=c(0,2000))

products <- products %>% 
  mutate(organic=ifelse(str_detect(str_to_lower(products$product_name),'organic'),"organic","not organic"), organic= as.factor(organic))

tmp <- order_products_train %>% 
  left_join(products, by="product_id") %>% 
  group_by(organic) %>% 
  summarize(count = n()) %>% 
  mutate(proportion = count/sum(count))
kable(tmp)

tmp %>% 
  ggplot(aes(x=organic,y=count, fill=organic))+
  geom_bar(stat="identity") +scale_fill_manual(values=c("#696969","#006400"))

tmp <- order_products_train %>% left_join(products,by="product_id") %>% 
  group_by(organic) %>% summarize(mean_reordered = mean(reordered))
kable(tmp)

tmp %>% 
  ggplot(aes(x=organic,fill=organic,y=mean_reordered))+geom_bar(stat="identity")+scale_fill_manual(values=c("#696969","#006400"))

library(treemap)

tmp <- products %>% group_by(department_id, aisle_id) %>% summarize(n=n())
tmp <- tmp %>% left_join(departments,by="department_id")
tmp <- tmp %>% left_join(aisles,by="aisle_id")

tmp2<-order_products_train %>% 
  group_by(product_id) %>% 
  summarize(count=n()) %>% 
  left_join(products,by="product_id") %>% 
  ungroup() %>% 
  group_by(department_id,aisle_id) %>% 
  summarize(sumcount = sum(count)) %>% 
  left_join(tmp, by = c("department_id", "aisle_id")) %>% 
  mutate(onesize = 1)

treemap(tmp2,index=c("department","aisle"),
        vSize="onesize",vColor="department",palette="Set3",
        title="",sortID="-sumcount", border.col="#FFFFFF",type="categorical",
        fontsize.legend = 0,bg.labels = "#FFFFFF")

treemap(tmp,index=c("department","aisle"),vSize="n",title="",palette="Set3",border.col="#FFFFFF")

treemap(tmp2,index=c("department","aisle"),vSize="sumcount",title="",palette="Set3",border.col="#FFFFFF")

tmp <- order_products_prior %>% 
  group_by(order_id) %>% 
  summarize(m = mean(reordered),n=n()) %>% 
  right_join(filter(orders,order_number>2), by="order_id")

tmp2 <- tmp %>% 
  filter(eval_set =="prior") %>% 
  group_by(user_id) %>% 
  summarize(n_equal = sum(m==1,na.rm=T), percent_equal = n_equal/n()) %>% 
  filter(percent_equal == 1) %>% 
  arrange(desc(n_equal))

datatable(tmp2, class="table-condensed", style="bootstrap", options = list(dom = 'tp'))


uniqueorders <- filter(tmp, user_id == 99753)$order_id
tmp <- order_products_prior %>% 
  filter(order_id %in% uniqueorders) %>% 
  left_join(products, by="product_id")

datatable(select(tmp,-aisle_id,-department_id,-organic), style="bootstrap", class="table-condensed", options = list(dom = 'tp'))

tmp <- orders %>% filter(user_id==99753, eval_set == "train")
tmp2 <- order_products_train %>%  
  filter(order_id == tmp$order_id) %>% 
  left_join(products, by="product_id")

datatable(select(tmp2,-aisle_id,-department_id,-organic), style="bootstrap", class="table-condensed", options = list(dom = 't'))

