library(dplyr)
library(magrittr)

source("udf.R")

# create BBQ data
bbq <- fn_gendata(venue_choice = "BBQ")

# BBQ summaries
# renaming variable names
bbq <- rename(bbq, Age_Group = "Age_Group.x")
bbq <- rename(bbq, Time_of_Day = "Time_of_Day.x")
bbq <- rename(bbq, Menu = "Menu.x")

# total cost
bbq_tot <- bbq %>% group_by(Time_of_Day) %>% summarise(Sum_Price = sum(Total_Price)) %>% arrange(Time_of_Day, desc(Sum_Price))

# summary by Time_of_Day
bbq_tod <- bbq%>% arrange(Time_of_Day) %>% group_by(Name, Time_of_Day) %>% 
  summarise(Sum_Price = sum(Total_Price)) %>% arrange(Time_of_Day, desc(Sum_Price))

# summary venue, age group, time of day and menu
bbq_tod_menu <- bbq%>% arrange(Time_of_Day) %>% group_by(Name, Age_Group,Time_of_Day,Menu) %>% 
  summarise(Sum_Price = sum(Total_Price)) %>% arrange(Time_of_Day, desc(Sum_Price), Menu)

################## Chutney Chang ###########################################

cc <- fn_gendata(venue_choice = "CC")

# CC summaries
# renaming variable names
cc <- rename(cc, Age_Group = "Age_Group.x")
cc <- rename(cc, Time_of_Day = "Time_of_Day.x")
cc <- rename(cc, Menu = "Menu.x")

# summary by Time_of_Day
cc_tod <- cc%>% arrange(Time_of_Day) %>% group_by(Name, Time_of_Day) %>% 
  summarise(Sum_Price = sum(Total_Price)) %>% arrange(Time_of_Day, desc(Sum_Price))

# summary venue, age group, time of day and menu
cc_tod_menu <- cc%>% arrange(Time_of_Day) %>% group_by(Name, Age_Group,Time_of_Day,Menu) %>% 
  summarise(Sum_Price = sum(Total_Price)) %>% arrange(Time_of_Day, desc(Sum_Price), Menu)



                                             