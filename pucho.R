cat<-read.csv("aisles.csv")
orders<-read.csv("orders.csv")
prod<-read.csv("products.csv")
dep<-read.csv("departments.csv")
orders_prior<-read.csv("order_products__prior.csv")


library(dplyr)
library(ggplot2)
#TOP 10 Products sold
q <- orders_prior %>% 
  group_by(product_id) %>% 
  summarize(total_buys = n()) %>% 
  left_join(select(prod,product_id,product_name),by="product_id") %>%
  arrange(desc(total_buys))

q_temp<-q[1:10,]
ggplot(data=q_temp, aes(x=product_name, y=total_buys, width=.7)) +
  geom_bar(stat="identity", position="identity",fill="steelblue")+scale_x_discrete(limits=q_temp$product_name)+
  theme(axis.text.x=element_text(angle=90))+ ggtitle("Top 10 Products sold")

#Top 10 departments

q<- q %>%
  left_join(select(prod,product_id,department_id),by="product_id")

q<- q %>%
  left_join(select(dep,department,department_id),by="department_id")

q1 <- q %>% 
  group_by(department_id,department) %>% 
  summarize(total_buys = n()) %>% 
  arrange(desc(total_buys))

q1_temp<-q1[1:10,]
ggplot(data=q1_temp, aes(x=department, y=total_buys, width=.9)) +
  geom_bar(stat="identity", position="identity",fill="steelblue")+scale_x_discrete(limits= q1_temp$department)+
  theme(axis.text.x=element_text(angle=90))+ggtitle("Top 10 departments")

#Which day of week is the sale high??

dow <-  orders %>% 
  group_by(order_dow) %>% 
  summarize(total_buys = n()) %>% 
  arrange(desc(total_buys))


ggplot(data=dow, aes(x=order_dow, y=total_buys, width=.9)) +
  geom_bar(stat="identity", position="identity",fill="steelblue")+scale_x_discrete()+
  ggtitle("which Day of the week is the sale high?") 

#Which time is the sale high/at the peak?

t <-  orders %>% 
  group_by(order_hour_of_day) %>% 
  summarize(total_buys = n()) %>% 
  arrange(desc(total_buys))

ggplot(data=t, aes(x=order_hour_of_day, y=total_buys, width=.9)) +
  geom_bar(stat="identity", position="identity",fill="steelblue")+scale_x_discrete()+
  ggtitle("Which time is the sale high or is at the peak?")

#How many times people reorder?

reorder_count <-  orders_prior %>% 
  group_by(reordered) %>% 
  summarize(total_buys = n()) %>% 
  arrange(desc(total_buys))
reorder_count$status<-c("reordered","not reordered")

ggplot(data=reorder_count, aes(x=status, y=total_buys, width=.9)) +
  geom_bar(stat="identity", position="identity",fill="steelblue")+scale_x_discrete()+
  ggtitle("How many times people reorder?")

#Which products people re-order?
data <-  subset(orders_prior,reordered==1)

reorder_prod<- data %>% group_by(product_id) %>% 
  summarize(total_buys = n()) %>% 
    left_join(select(prod,product_name,product_id),by="product_id") %>%
  arrange(desc(total_buys))
reorder_prod_temp<-reorder_prod[1:10,]
ggplot(data=reorder_prod_temp, aes(x=product_name, y=total_buys, width=.7)) +
  geom_bar(stat="identity", position="identity",fill="steelblue")+scale_x_discrete(limits=reorder_prod_temp$product_name)+
  theme(axis.text.x=element_text(angle=90))+ ggtitle("Top 10 Products reordered")

