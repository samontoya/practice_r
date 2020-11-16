library("faraway")
library("ggplot2")
library("ggthemes")
library("dplyr")
library("gridExtra")

data("worldcup")
head(worldcup)

library("dlnm")
data("chicagoNMMAPS")
head(chicagoNMMAPS)

chic <- chicagoNMMAPS

chic_july <-chic %>% 
  filter(year ==1995 & month ==7) 

ggplot(data=chic_july, aes(x= date, y=death))+
  geom_point() +
  theme_bw()


ggplot(data=chic_july, aes(x= date, y=death))+
  geom_point() +
  theme_few()

ggplot(data=chic_july, aes(x= date, y=death))+
  geom_point() +
  theme_tufte()  

ggplot(data=chic_july, aes(x= date, y=death))+
  geom_point() +
  theme_excel()

#video3
worldcup %>% 
  ggplot(aes(x=Position))+
  geom_bar()+
  labs(y="Number of Players")

worldcup %>% 
  ggplot(aes(x=Position))+
  geom_bar()+
  labs(x="", 
       y="Number of Players") # this takes out x label

#flip axis to show better
worldcup %>% 
  ggplot(aes(x=Position))+
  geom_bar()+
  labs(x="",y="Number of Players")+
  coord_flip()

worldcup %>% 
  ggplot(aes(x=Passes, y=Shots))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_few()

worldcup %>% 
  ggplot(aes(x=Passes, y=Shots))+
  geom_point()+
  geom_smooth(span=0.1)+
  theme_few()

worldcup %>% 
  ggplot(aes(x=Passes, y=Shots))+
  geom_point()+
  geom_smooth(span=0.1, se=FALSE)+
  theme_few()

#vid4
ggplot(dat=chic_july, aes(x=date, y=death))+
  geom_point()

#create separate dataframe to show hottest day
hottest_day<- chic_july %>% 
  filter(temp == max(temp))

hottest_day # add this as a label to ggplot
#can add data from diff dataframes:
ggplot(dat=chic_july, aes(x=date, y=death))+
  geom_point(color="red")+
  geom_text(data= hottest_day, label="Max")
#where above writes max on max value

ggplot(dat=chic_july, aes(x=date, y=death))+
  geom_point(color="red")+
  geom_text(data= hottest_day, aes(label=temp))
# where the above shows the temp max value

ggplot(dat=chic_july, aes(x=date, y=death))+
  geom_point(color="red")+
  geom_text(data= hottest_day, label="Max",
            hjust =0.2, vjust=0) # these adjust labels a bit

ggplot(dat=chic_july, aes(x=date, y=death))+
  geom_point(color="red")+
  geom_label(data= hottest_day, label="Max")

ggplot(dat=chic_july, aes(x=date, y=death))+
  geom_point(color="red")+
  geom_label(data= hottest_day, aes(label=temp))
#comparing while label puts box around label
#geom text does not

library("lubridate")
 hw<- tibble(date= c(ymd("1995-07-12"), ymd("1995-07-16")),
                 death= c(425, 425))

ggplot(chic_july, aes(x=date, y=death))+
  geom_point(color="red")+
  geom_line(data=hw, size=3)


