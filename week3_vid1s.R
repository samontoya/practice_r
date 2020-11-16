library(readr)
library(dplyr)

beijing_pm_raw <- read.csv(file="data/Beijing_2017_HourlyPM25.csv", 
                           skip=3) # we are skipping the headers of the file

beijing_pm <- beijing_pm_raw %>%
  rename(sample_time = Date..LST.,
         value = Value,
         qc=QC.Name) %>%
  select(sample_time, value, qc) %>%
  mutate(aqi=cut(value,
                 breaks = c(0, 50, 100, 150,200, 300, 500, Inf), #we assigned breaks based on aqi index: INf=infinitely high in R 
                 labels = c("Good", "Moderate",
                            "Unhealthy for some groups",
                            "Unhealthy", "Very Unhealthy",
                            "Hazardous","Beyond Index")))

mean(beijing_pm$value) # or for pipeline version look below

beijing_pm %>%
  pull(value) %>%
  mean()

beijing_pm %>%
  summarize(min_pm =min(value),
            mean_pm = mean(value),
            max_pm=max(value)) %>%
  mutate(pm_range = max_pm - min_pm) # this takes the max-min and adds a column

#this is the Og version with no piping
summarize(.data = beijing_pm,
          min_pm =min(value),
          mean_pm = mean(value),
          max_pm=max(value))

levels(beijing_pm$aqi)

#can pipe and pull for levels - same as above output
beijing_pm %>%
  pull(aqi) %>%
  levels()

beijing_pm %>%
  group_by(aqi) %>%
  count()    #this factor level order is based on what we typed when we input above, NA may have some -999 
              # but we gave it a bottom cap at 0 so will show as NA
beijing_pm %>%
  group_by(aqi) %>%
  summarize(mean_pm= mean(value), #allows us to find min and means after sorted from vector of group_by
            min_pm=min(value),
            max_pm = max(value),
            n =n())

# video4 tangent to look at r datasets:
library("faraway")
data("worldcup")
head(worldcup)
?worldcup

data() #shows all available data in your R 
data(package="faraway") #shows data just for this package

# back to Beijing data
typeof(beijing_pm$sample_time)
# our date is returned as character not date we need to change

beijing_pm %>% pull(sample_time) # use this to see the whole vector and which one is first day or month

library(lubridate)
beijing_pm <- beijing_pm %>%
  mutate(sample_time = mdy_hm(sample_time))

beijing_pm %>%
  pull(sample_time) %>%
  class()    #this outputs psoxct for class type -  special type of date, no longer a character
 
beijing_pm %>% 
  pull(sample_time) %>% 
  wday(labe=TRUE)  # this turns the dates into days of the week

beijing_pm <- beijing_pm%>% 
  mutate(sample_weekday = wday(sample_time, label = TRUE )) # this adds the weekdays as a column

#now we can look at the diff weekdays to see if anything interesting 

beijing_pm %>% 
  group_by(sample_weekday) %>% 
  summarize(mean_pm = mean(value)) # could have diff pollution for commuting time in weekday vs weekend

head(beijing_pm$value > 500)
length(beijing_pm$value)

beijing_pm<- beijing_pm %>% 
  mutate(beyond_index=value >500) %>% 
  filter(value > 0)

beijing_pm <- beijing_pm %>% 
  mutate(heating= sample_time<ymd("2017-03-15"))

beijing_pm %>% 
  filter(!heating) #the !heating flips it to look at not any heating going

beijing_pm %>% 
  pull(beyond_index) %>% 
  sum()


beijing_pm %>% 
  pull(beyond_index) %>% 
  mean()

# Video8 ggplot
library(ggplot2)
beijing_pm


ggplot(data = beijing_pm)+
  geom_point(mapping= aes(x=sample_time, y=value))+
  geom_line(mapping= aes(x=sample_time, y=value))+
  geom_rug(mapping= aes(x=sample_time, y=value)) #shows on the axis how the data is distributed, shows
                                #majority of data bellow 400 with some under 600 on y axis

#cleaner code bellow
beijing_pm %>% 
  ggplot( mapping= aes(x=sample_time, y=value))+  #this mapping is default can ovveride i
    geom_point()+     #in the geoms right here if add mapping vars tp these
    geom_line()+
    geom_rug() 
 
beijing_pm %>% 
  ggplot(mapping= aes(x=sample_time, y=value))+  
  geom_point(mapping= aes(x=sample_time, y=value), color="blue", shape=9) #how to set default color as blue


beijing_pm %>% 
  ggplot(mapping= aes(x=sample_time, y=value, color=aqi))+  
  geom_point(mapping= aes(x=sample_time, y=value))+
  labs(x="Date in 2017", y="PM", color="Air Quality Index")+
  ggtitle("AIR Quality Measurements, Beijing China 2017", subtitle = "US Embassy Air Pollution Monitor")

ggplot(data=beijing_pm)+
  geom_histogram(aes(x=value),
                 color="gray", fill="blue", bins = 100)+
  labs(x="PM 2.5 Concentration", y="# of Observations")+
  ggtitle("PM COncentration in Beijing China, 2017")

beijing_pm %>% 
  ggplot()+
  geom_bar(aes(x=aqi, fill=heating),
           position="dodge") # the dodge changes color to be dodged and not stacked

beijing_pm %>% 
  ggplot()+
  geom_boxplot(mapping=aes(x=1, y=value))

beijing_pm %>%               #this example same as above one except the boxplots are split by their aqi
  ggplot(mapping=aes(x=aqi, y=value, group=aqi))+
  geom_boxplot()
  