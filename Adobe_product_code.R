library("readxl")
library("dplyr")
install.packages("lubridate")
library("lubridate")
install.packages("tidyverse")
library(tidyverse)
install.packages("combinat")
library(combinat)


my_data <- read_excel("data_2.xlsx", sheet = "data_2")
my_data <- na.omit(my_data)
#calculating reveue per transaction
my_data <-  mutate(my_data, revenue = Quantity*UnitPrice)
my_data <-rename(my_data,"prodcat" = "product categories")
# removing invoice date
my_data$InvoiceDate= NULL

#monthly revenue 
my_data <- my_data %>%
mutate(month=month(as.Date(my_data$InvoiceTS,format="%Y-%m-%d")))

Monthrev <- my_data %>% group_by(`month`) %>% summarise(Monthlyrev = sum(revenue))
summary(Monthrev)
my_data %>% group_by(`Country`) %>% summarise(country_rev = sum(revenue)) %>% arrange(desc(country_rev))      

#desc reveue 
item_rev <- my_data %>% group_by(`Description`,prodcat,UnitPrice) %>% summarise(item_rev = sum(revenue), quant= sum(Quantity)) %>% arrange(desc(item_rev))

#desc_unit price analysis
unit_price_variance <- my_data %>% group_by(prodcat,Description,UnitPrice) %>% summarise(item_rev = sum(revenue), quant= sum(Quantity))
library(descr)
unit_price_variance_c <- as.data.frame(table(unit_price_variance$Description))


# histograms of distributions
rev_tran <- my_data %>% select(CustomerID,revenue) %>%group_by(CustomerID) %>% summarise(revpercust = sum(revenue)) %>% arrange(revpercust)
hist(rev_tran$revpercust,5000,xlim=c(0,10000),main="revenue per cust")
boxplot(rev_tran$revpercust)


tran_num <- my_data %>% select(InvoiceTS,CustomerID) %>%group_by(CustomerID) %>% summarise(numOtran = n()) %>% arrange(numOtran)
hist(tran_num$numOtran,5000,xlim=c(0,1000),main="tran per cust")
boxplot(tran_num$numOtran)


# recommendation system 
my_data <- arrange(my_data,InvoiceNo)
       
Reccodata <- my_data %>% group_by(InvoiceNo) %>% mutate(prod_bought_together = paste0(Description, collapse = ",")) 
Reccodata <- Reccodata %>% select(InvoiceNo,prod_bought_together)

write.csv(Reccodata,file = "reccodata.csv")
#the products bought in a particular purhcase 
Reccodata <- distinct(Reccodata,InvoiceNo,prod_bought_together)

#unique products list
unique_prod <- distinct(my_data,Description)
#generating product pairs
prod_pair <- combn(unique_prod$Description,2)
prod_pair_tran <-t(prod_pair)
#write.csv(prod_pair_tran,file = "prod_pair.csv")

#frequency of prod_pair


