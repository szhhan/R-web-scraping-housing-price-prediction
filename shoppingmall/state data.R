
library("readxl")
state_data <- read_excel(path = "/Users/freyafu/Desktop/st_data.xlsx", col_names = TRUE,na = "")
price_data <- read_excel(path = "/Users/freyafu/Desktop/price_2017.xlsx", col_names = TRUE,na = "")
library(tidyr)
library(purrr)
library(dplyr)
A<-state_data %>% 
  group_by(location,State) %>% 
  count(unique(location,State)) %>% 
  select(location, State,n)%>% 
  arrange(-n)

colnames(A)<-c("County","State","Number of Malls")
write.xlsx(A, 'shoppingmall_data.xlsx')
B<-as.data.frame(price_data)


C<-merge(A,B,by=c("County","State"))
