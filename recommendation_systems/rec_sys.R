library(randomForest)
library(missForest)
library(recosystem)
library(data.table)
library(lubridate)
library(devtools)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(caret)
library(zoo)
library(readr)
library(rfm)
library(RColorBrewer)
gc()
gc(reset = TRUE)
setwd("~/Desktop/")
df <- data.table::fread("sales_history.csv",na = c(""," ","NA",NA))
length(unique(df$customer_id))
length(unique(df$item_id))

df$idx <- NULL
sum(df$birthdate=='NULL')
df$birthdate <- NULL
table(df$gender)
df$gender <- ifelse(df$gender=="m",'m',
                    ifelse(df$gender=="f","f",NA))
df$gender <- ifelse(is.na(df$gender),"NA",df$gender)



df$level_one_id <- ifelse(is.na(df$level_one_id),0,df$level_one_id)
df$level_two_id <- ifelse(is.na(df$level_two_id),0,df$level_two_id)
df$level_three_id <- ifelse(is.na(df$level_three_id),0,df$level_three_id)
sapply(df,function(x){sum(is.na(x))})

df <- df[complete.cases(df),]
# Delete customers with one transaction

df$date <- as.Date(df$occurence)

# удалили возраст, ID, привели переменные в нужный вид



items_bought_time <- df %>% group_by(item_id) %>% summarise(item_bought = n())
summary(items_bought_time$item_bought)

boxplot(items_bought_time$item_bought)
# посмотрели просто распределение

quantile(items_bought_time$item_bought,c(0.05,0.95))

# решаем проблему холодного старта
items_bought_time <- items_bought_time[items_bought_time$item_bought>5,]
items_bought_time <- items_bought_time[items_bought_time$item_bought<142,]

df <- inner_join(df,items_bought_time)

df$item_bought <- NULL


#first_level <- df %>% group_by(level_one_id,date) %>% summarise(y=sum(cost))

#f <- as.data.frame(table(first_level$level_one_id))[as.data.frame(table(first_level$level_one_id))$Freq>30,]
#f$Freq <- NULL;colnames(f) <- c("level_one_id");f$level_one_id <- as.numeric(as.character(f$level_one_id))

#first_level <- inner_join(first_level,f)


to_delete <- df %>% group_by(customer_id) %>% summarise(transaction_count = n())
quantile(to_delete$transaction_count,c(0.05,0.95))

to_delete <- to_delete[to_delete$transaction_count>5,]
to_delete <- to_delete[to_delete$transaction_count<41,]
df <- inner_join(df,to_delete)

# Delete transactions with cost less than 0 tenge

df <- df[df$cost>1500,]

to_delete_item <- df %>% group_by(customer_id) %>% summarise(items_count = length(unique(item_id)))

to_delete_item <- to_delete_item[to_delete_item$items_count>4,]

df <- inner_join(df,to_delete_item)


# реализация RFM

# Mini data engineering
x <- df %>%
  group_by(customer_id) %>%
  summarise(revenue = sum(cost,na.rm = T),
            most_recent_visit = as.Date(max(occurence,na.rm = T)),
            number_of_orders = n())


x$recency_days = as.Date(Sys.Date())-as.Date(x$most_recent_visit)

head(x)

OutVals = boxplot(x$number_of_orders)$out
boxplot(which(x$number_of_orders %in% OutVals))
x <- (x[-(which(x$number_of_orders %in% OutVals)),])

OutVals = boxplot(x$revenue)$out
boxplot(which(x$revenue %in% OutVals))
x <- (x[-(which(x$revenue %in% OutVals)),])
boxplot(x$revenue)
boxplot(x$number_of_orders)

boxplot(as.integer(x$recency_days))

x_customers <- unique(select(x,customer_id))
length(unique(df$customer_id))
df <- inner_join(df,x_customers)


# paste code recs.R here please
#
#
#
#
gc()
gc(reset=TRUE)

most_popular_1_months = df %>% filter(occurence>=as.Date(max(df$occurence))-30) %>% 
  group_by(item_id) %>% summarise(customers_counts=n(),joinkey=1) %>% 
  arrange(-customers_counts)

most_popular_1_months <- most_popular_1_months[1:1000,]



gc()
gc(reset = T)
#full_panel = inner_join(all_customers, most_popular_1_months,by="joinkey")
#full_panel$joinkey=NULL

##recs = anti_join(full_panel, most_recent_per_customer, c("customer_id","item_id")) #%>%arrange(ncodpers,-count)
#recs = arrange(recs, customer_id, -customers_counts)



#
#
#
#
# user id, n(), sum, recency
analysis_date <- Sys.Date()
str(x)
x <- as.data.frame(x)
x$analysis_date <- analysis_date
rfm_result <- rfm_table_customer(x,
                                 customer_id,
                                 number_of_orders,
                                 recency_days,
                                 revenue,
                                 analysis_date)


#gf <- rfm_result$rfm
#gf <- gf[1:100,]
#write.csv(gf,"rfm_example.csv",row.names = F)
#ggplotly(rfm_heatmap(rfm_result))
#rfm_bar_chart(rfm_result)
#rfm_histograms(rfm_result)
#rfm_order_dist(rfm_result)
#rfm_rm_plot(rfm_result)
#rfm_fm_plot(rfm_result)
#rfm_rf_plot(rfm_result)

rrr <- rfm_result$rfm
rrr$Segment <- ifelse(between(rrr$recency_score,4,5) & between(rrr$frequency_score,4,5) & between(rrr$monetary_score,4,5),"Крутые клиенты",
                      ifelse(between(rrr$recency_score,2,5) & between(rrr$frequency_score,3,5) & between(rrr$monetary_score,3,5),"Лояльные клиенты",
                             ifelse(between(rrr$recency_score,3,5) & between(rrr$frequency_score,1,3) & between(rrr$monetary_score,1,3),"Потенциальные лояльные клиенты",
                                    ifelse(between(rrr$recency_score,4,5) & between(rrr$frequency_score,0,1) & between(rrr$monetary_score,0,1),"Новые клиенты",
                                           ifelse(between(rrr$recency_score,3,4) & between(rrr$frequency_score,0,1) & between(rrr$monetary_score,0,1),"Часто приходят но мало тратят",
                                                  ifelse(between(rrr$recency_score,2,3) & between(rrr$frequency_score,2,3) & between(rrr$monetary_score,2,3),"Нужно уделить внимание",
                                                         ifelse(between(rrr$recency_score,2,3) & between(rrr$frequency_score,0,2) & between(rrr$monetary_score,0,2),"Мало приходят, берут, тратят",
                                                                ifelse(between(rrr$recency_score,2,0) & between(rrr$frequency_score,2,5) & between(rrr$monetary_score,2,5),"Приходят за дорогими покупками но редко",
                                                                       ifelse(between(rrr$recency_score,1,0) & between(rrr$frequency_score,4,5) & between(rrr$monetary_score,4,5),"Делали когда то большие покупки и часто но не сейчас",
                                                                              ifelse(between(rrr$recency_score,1,2) & between(rrr$frequency_score,1,2) & between(rrr$monetary_score,1,2),"Очень мало тратят приходят","Потеряли"))))))))))
rrr %>% 
  count(Segment) %>% 
  arrange(desc(n)) %>% 
  rename(Segment = Segment, Count = n)

head(rrr)
segments_of_customers <- select(rrr,c(customer_id,Segment))
head(segments_of_customers)
write.csv(segments_of_customers,"customer_segments.csv",row.names = F)
data <- rrr %>%
  group_by(Segment) %>%
  dplyr::select(Segment, recency_days) %>%
  summarize(median(recency_days)) %>%
  rename(Segment = Segment, avg_recency = `median(recency_days)`) %>%
  arrange(avg_recency) 

n_fill <- nrow(data)
ggplot(data, aes(Segment, avg_recency)) +
  geom_bar(stat = "identity", fill = brewer.pal(n = n_fill, name = "Set1")) +
  xlab("Segment") + ylab("Median Recency") +
  ggtitle("Median Recency by Segment") +
  coord_flip() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )
data <- 
  rrr %>%
  group_by(Segment) %>%
  dplyr::select(Segment, transaction_count) %>%
  summarize(median(transaction_count)) %>%
  rename(Segment = Segment, avg_frequency = `median(transaction_count)`) %>%
  arrange(avg_frequency) 


ggplot(data, aes(Segment, avg_frequency)) +
  geom_bar(stat = "identity", fill = brewer.pal(n = n_fill, name = "Set1")) +
  xlab("Segment") + ylab("Median Frequency") +
  ggtitle("Median Frequency by Segment") +
  coord_flip() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

data <- 
  rrr %>%
  group_by(Segment) %>%
  dplyr::select(Segment, amount) %>%
  summarize(median(amount)) %>%
  rename(Segment = Segment, avg_monetary = `median(amount)`) %>%
  arrange(avg_monetary) 
ggplot(data, aes(Segment, avg_monetary)) +
  geom_bar(stat = "identity", fill = brewer.pal(n = n_fill, name = "Set1")) +
  xlab("Segment") + ylab("Median Monetary Value") +
  ggtitle("Median Monetary Value by Segment") +
  coord_flip() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )


write.csv(rrr,"rfm-by-customers.csv",row.names = FALSE)
head(rrr)
cust_seg <- dplyr::select(rrr,c(customer_id,Segment))
cust_seg <- unique(cust_seg)
df <- inner_join(df,cust_seg)
table(df$Segment)
df <- df[df$Segment=="Крутые клиенты",]
x <- df %>%
  group_by(customer_id,item_id) %>%
  summarise(revenue = sum(cost,na.rm = T),
            most_recent_visit = as.Date(max(occurence,na.rm = T)),
            number_of_orders = n())
head(x)
x$recency_days = as.Date(Sys.Date())-as.Date(x$most_recent_visit)
gc()
gc(reset = TRUE)

str(x)
x <- as.data.frame(x)
#x_1 <- sample_frac(x,0.01)


f <- function(x){x <- rfm_table_customer(x,
                                         customer_id,
                                         number_of_orders,
                                         recency_days,
                                         revenue,
                                         analysis_date)
x$rfm
}

library(plyr)
#head(x_1)
g <- dlply(x,"item_id",f)
t <- ldply(g,data.frame)

head(t)
rrr <- t
head(rrr)
rrr$Segment <- ifelse(between(rrr$recency_score,4,5) & between(rrr$frequency_score,4,5) & between(rrr$monetary_score,4,5),"Крутые клиенты",
                      ifelse(between(rrr$recency_score,2,5) & between(rrr$frequency_score,3,5) & between(rrr$monetary_score,3,5),"Лояльные клиенты",
                             ifelse(between(rrr$recency_score,3,5) & between(rrr$frequency_score,1,3) & between(rrr$monetary_score,1,3),"Потенциальные лояльные клиенты",
                                    ifelse(between(rrr$recency_score,4,5) & between(rrr$frequency_score,0,1) & between(rrr$monetary_score,0,1),"Новые клиенты",
                                           ifelse(between(rrr$recency_score,3,4) & between(rrr$frequency_score,0,1) & between(rrr$monetary_score,0,1),"Часто приходят но мало тратят",
                                                  ifelse(between(rrr$recency_score,2,3) & between(rrr$frequency_score,2,3) & between(rrr$monetary_score,2,3),"Нужно уделить внимание",
                                                         ifelse(between(rrr$recency_score,2,3) & between(rrr$frequency_score,0,2) & between(rrr$monetary_score,0,2),"Мало приходят, берут, тратят",
                                                                ifelse(between(rrr$recency_score,2,0) & between(rrr$frequency_score,2,5) & between(rrr$monetary_score,2,5),"Приходят за дорогими покупками но редко",
                                                                       ifelse(between(rrr$recency_score,1,0) & between(rrr$frequency_score,4,5) & between(rrr$monetary_score,4,5),"Делали когда то большие покупки и часто но не сейчас",
                                                                              ifelse(between(rrr$recency_score,1,2) & between(rrr$frequency_score,1,2) & between(rrr$monetary_score,1,2),"Очень мало тратят приходят","Потеряли"))))))))))

str(rrr)
rrr %>% 
  dplyr::count(Segment) %>% 
  arrange(desc(n)) 
table(rrr$Segment)
r <- rrr
head(r)
rm(t)
r_1 <- r

r$rating <- r$recency_score + r$frequency_score + r$monetary_score
gc(reset = TRUE)
#r <- r_1
head(r)
r$rating <- r$rating/2.25
r$rating <- round(r$rating)
r$rating <- ifelse(r$rating>=5,5,r$rating)
qplot(r$rating)

#cust_item <- dplyr::select(x,c(customer_id,item_id,id,Segment))
r <- dplyr::select(r,c(customer_id,item_id,rating))

head(r)
gc(reset = TRUE)
total <- r
str(total)
total <- data.table(total)
total <- as.data.frame(total)
total <- total[complete.cases(total),]