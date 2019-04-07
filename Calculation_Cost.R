library(dplyr)
library(magrittr)

source("udf.R")

cc <- fn_gendata(venue_choice = "CC")
bbq <- fn_gendata(venue_choice = "BBQ")



# summary venue, age group, time of day and menu
invt_lst%>% arrange(Time_of_Day.x) %>% group_by(Age_Group.x,Time_of_Day.x,Menu.x) %>% 
  summarise(Sum_Price = sum(Total_Price)) 

# summary by time of day
invt_lst%>% arrange(Time_of_Day.x) %>% group_by(Time_of_Day.x) %>% 
  summarise(Sum_Price = sum(Total_Price)) 

# summary by inviter name
invt_lst%>% arrange(Time_of_Day.x) %>% group_by(Name, Time_of_Day.x) %>% 
  summarise(Sum_Price = sum(Total_Price))

invt_lst %>% filter(Time_of_Day.x == "Dinner") %>% group_by(Name) %>% summarise(Count = sum(Number)) %>% 
  arrange(Name)
invt_lst %>% filter(Time_of_Day.x == "Dinner") %>% group_by(Name, Age_Group.x) %>% summarise(Count = sum(Number)) %>% 
  arrange(Name)

################## Chutney Chang ###########################################

# loading invitee list
invt_lst <- read.csv(file = "D:\\Aurin\\Invitee_list.csv")
price_lst_bbq <- read.csv(file = "D:\\Aurin\\price_list.csv")

# merging both files based on unique id - BBQ
invt_lst %<>% mutate(Unq_Key = paste0(Age_Group,Time_of_Day,Menu)) %>% 
  select(Name,Type,Invitees,Age_Group,Time_of_Day,Menu,Unq_Key,Number)

price_lst_bbq %<>% mutate(Unq_Key = paste0(Age_Group,Time_of_Day,Menu)) %>% 
  select(Venue,Age_Group,Time_of_Day,Menu,Unq_Key,INR) %>% filter(Venue == "CC") %>% select(-Venue)

invt_lst <- left_join(invt_lst, price_lst_bbq, by = "Unq_Key")
invt_lst %<>% select(Name,Type,Invitees,Age_Group.x,Time_of_Day.x,Menu.x,Unq_Key,Number,INR) %>% mutate(Total_Price = Number*INR)

# summary venue, age group, time of day and menu
invt_lst%>% arrange(Time_of_Day.x) %>% group_by(Age_Group.x,Time_of_Day.x,Menu.x) %>% 
  summarise(Sum_Price = sum(Total_Price)) 

# summary by time of day
invt_lst%>% arrange(Time_of_Day.x) %>% group_by(Time_of_Day.x) %>% 
  summarise(Sum_Price = sum(Total_Price)) 

# summary by inviter name
invt_lst%>% arrange(Time_of_Day.x) %>% group_by(Name, Time_of_Day.x) %>% 
  summarise(Sum_Price = sum(Total_Price))

write.csv(file = "D:\\Aurin\\CC_Cost.csv", invt_lst, row.names = F)

invt_lst %>% filter(Time_of_Day.x == "Dinner") %>% group_by(Name) %>% summarise(Count = sum(Number)) %>% 
  arrange(Name)
invt_lst %>% filter(Time_of_Day.x == "Dinner") %>% group_by(Name, Age_Group.x) %>% summarise(Count = sum(Number)) %>% 
  arrange(Name)

################# function

fn_gendata <- function(venue_choice = NULL){
  # loading invitee list
  invt_lst <- read.csv(file = "D:\\Aurin\\Invitee_list.csv")
  price_lst_bbq <- read.csv(file = "D:\\Aurin\\price_list.csv")
  
  # merging both files based on unique id - dynamic
  invt_lst %<>% mutate(Unq_Key = paste0(Age_Group,Time_of_Day,Menu)) %>% 
    select(Name,Type,Invitees,Age_Group,Time_of_Day,Menu,Unq_Key,Number)
  
  price_lst_bbq %<>% mutate(Unq_Key = paste0(Age_Group,Time_of_Day,Menu)) %>% 
    select(Venue,Age_Group,Time_of_Day,Menu,Unq_Key,INR) %>% filter(Venue == venue_choice) %>% select(-Venue)
  
  invt_lst <- left_join(invt_lst, price_lst_bbq, by = "Unq_Key")
  invt_lst %<>% select(Name,Type,Invitees,Age_Group.x,Time_of_Day.x,Menu.x,Unq_Key,Number,INR) %>% mutate(Total_Price = Number*INR)
  
  # summary venue, age group, time of day and menu
  invt_lst%>% arrange(Time_of_Day.x) %>% group_by(Age_Group.x,Time_of_Day.x,Menu.x) %>% 
    summarise(Sum_Price = sum(Total_Price)) 
  
  # summary by time of day
  invt_lst%>% arrange(Time_of_Day.x) %>% group_by(Time_of_Day.x) %>% 
    summarise(Sum_Price = sum(Total_Price)) 
  
  # summary by inviter name
  invt_lst%>% arrange(Time_of_Day.x) %>% group_by(Name, Time_of_Day.x) %>% 
    summarise(Sum_Price = sum(Total_Price))
  
  return(invt_lst)
}


                                             