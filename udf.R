# function to create data set for venue based cost calculation
# both BBQ and CC are used as venues

fn_gendata <- function(venue_choice = NULL){
  # loading invitee list
  invt_lst <- read.csv(file = "D:\\Aurin\\Aurin-BDay\\data\\Invitee_list.csv")
  price_lst_bbq <- read.csv(file = "D:\\Aurin\\Aurin-BDay\\data\\price_list.csv")
  
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