



setwd("/Users/tsebaotshewarega")
CITIES_4 <- read.csv("CITIES_4.csv")



library(gridExtra)    
library(anyflights)
library(tidyr)
library(lubridate)
library(tidyverse)

options(timeout = 1000)
  

     
#making a function using the lubridate package
make_datetime_100 <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)




make_datetime_100 <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
}
#A time series to visualize the overall effect of COVID on the volume of flights across the four carriers  

covid<- CITIES_4%>%
  filter(!is.na(dep_time), 
         !is.na(arr_time)) %>%
  mutate(departure= make_datetime_100(year, month, day, dep_time))%>%
  group_by(year, carrier, month(departure))%>%
  summarise(number=n())%>%
  ggplot(aes(month(`month(departure)`, label = T), 
             number, group=year, color=factor(year)))+
  geom_line()+
  facet_wrap(~carrier)+
  labs(title="Volume of Flights across the carriers", x= "Month", y= "Volume of Flights")

covid
  




# To show the same distribution across the years (color coded by carrier this time)

covid2<- CITIES_4%>%
  filter(!is.na(dep_time), 
         !is.na(arr_time)) %>%
  mutate(departure= make_datetime_100(year, month, day, dep_time))%>%
  group_by(year, carrier, month(departure))%>%
  summarise(number=n())%>%
  ggplot(aes(month(`month(departure)`, label = T), 
             number, group=carrier, color=carrier))+
  geom_line()+
  facet_wrap(~year)+
  labs(title="Volume of Flights across the years", x= "Month", y= "Volume of Flights")

covid2

  

#Focusing on the holiday seasons of Thanksgiving and Christmas
#Thanksgiving falls on the 4th week of November, and people usually start traveling the week before 
#Christmas always falls on Dec 25th, but people start travelling as early as 3 or 2 weeks early. Winter break starts week early. 
#So they would sum up to the last 2 weeks of November and the 4 weeks of December (starting from the 47th week of the year)
#A time series on the volume of flights by carrier during the holiday weeks color coded by carrier

  


HOLIDAY2<- CITIES_4%>%
  filter(!is.na(dep_time), 
         !is.na(arr_time)) %>%
  mutate(departure= make_datetime_100(year, month, day, dep_time))%>%
  filter(week(departure)>=47)%>%
  group_by(year, carrier, month(departure))%>%
  summarise(number=n())%>%
  ggplot(aes(month(`month(departure)`, label = T), 
             number, group=carrier, color=carrier))+
  geom_line()+
  facet_wrap(~year)+
  labs(x= "Months of Holiday Season", y= "Volume of Flights")

HOLIDAY2


  

# A tabular representaion of the number of volumes for the 3 years

table <- CITIES_4%>%
  filter(!is.na(dep_time), 
         !is.na(arr_time)) %>%
  mutate(departure= make_datetime_100(year, month, day, dep_time))%>%
  filter(week(departure)>=47) %>%
  mutate(week= week(departure))%>%
  group_by(year, carrier)%>%
  summarise(volume = n())%>%
  group_by(year, carrier)%>%
  filter(volume==max(volume))

table
  

# Now taking a deeper by expanding the months into weeks 


WEEKS<- CITIES_4%>%
  filter(!is.na(dep_time), 
         !is.na(arr_time)) %>%
  mutate(departure= make_datetime_100(year, month, day, dep_time))%>%
  filter(week(departure)>=47) %>%
  mutate(week= week(departure))%>%
  group_by(year, carrier, week)%>%
  summarise(number=n())%>%
  ggplot(aes(x=week ,y= number, group=carrier, color=carrier)) + 
  geom_line()+  
  facet_wrap(~year)+
  labs(x= "Weeks during Holiday Season", y= "Volume of Flights") 


WEEKS

  

# We also wanted to check if this change was constant across the different distance flights





# looking at the summary of the distance variable 
summary(CITIES_4$distance)

#re-coding distance into a new variable(miles) and separating it into 3 layers: short, medium and long

dist<- CITIES_4%>%
  mutate(miles = case_when(distance<=1000 ~ "SHORT", 
                           distance>1000 & distance<=2000 ~ "MEDIUM", 
                           distance>2000  ~ "LONG"))
 

# A Tabular representation of the change in volume among flights of different length for the different carriers

#Not all of them fly to Alaska and Hawaii and since distance is being directly compared, filtered them out not to bias the numbers


MILEAGE<- dist%>%
  filter(dest!= "ANC"| dest!= "HNL")%>%
  filter(!is.na(dep_time), 
         !is.na(arr_time)) %>%
  mutate(departure= make_datetime_100(year, month, day, dep_time))%>%
  filter(week(departure)>=47) %>%
  group_by(year, carrier, miles)%>%
  summarise(volume=n())%>%
  group_by(year, carrier, miles)%>%
  filter(volume==max(volume))
MILEAGE

#To visualize the distribution of volume change across flights of different length 

Y_2019<- dist%>%
  filter(dest!= "ANC"| dest!= "HNL", year==2019)%>%
  filter(!is.na(dep_time), !is.na(arr_time)) %>%
  mutate(departure= make_datetime_100(year, month, day, dep_time))%>%
  filter(week(departure)>=47) %>%
  mutate(week= week(departure))%>%
  group_by(carrier, week, miles)%>%
  summarise(number=n())%>%
  ggplot(aes(x= week, y=number))+
  geom_col(aes(fill=miles), position = "dodge")+
  facet_wrap(~carrier)+
  labs(title="Holiday Weeks of 2019")
Y_2019


Y_2020<- dist%>%
  filter(dest!= "ANC"| dest!= "HNL", year==2020)%>%
  filter(!is.na(dep_time), !is.na(arr_time)) %>%
  mutate(departure= make_datetime_100(year, month, day, dep_time))%>%
  filter(week(departure)>=47) %>%
  mutate(week= week(departure))%>%
  group_by(carrier, week, miles)%>%
  summarise(number=n())%>%
  ggplot(aes(x= week, y=number))+
  geom_col(aes(fill=miles), position = "dodge")+
  facet_wrap(~carrier)+
  labs(title="Holiday Weeks of 2020")
Y_2020


Y_2021<- dist%>%
  filter(dest!= "ANC"| dest!= "HNL", year==2021)%>%
  filter(!is.na(dep_time), !is.na(arr_time)) %>%
  mutate(departure= make_datetime_100(year, month, day, dep_time))%>%
  filter(week(departure)>=47) %>%
  mutate(week= week(departure))%>%
  group_by(carrier, week, miles)%>%
  summarise(number=n())%>%
  ggplot(aes(x= week, y=number))+
  geom_col(aes(fill=miles), position = "dodge")+
  facet_wrap(~carrier)+
  labs(title="Holiday Weeks of 2021")
Y_2021



grid.arrange(Y_2019,Y_2020, Y_2021)


