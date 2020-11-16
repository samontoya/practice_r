#Video Five start
library("dplyr")

hair_color <- c("brown", "blond", "red", "brown", "blond")
hair_color

#to get the levels as a factor
hair_color <- as.factor(hair_color)
hair_color
hair_color %>% as.numeric()

hair_color %>% class()

library(forcats)
hair_color <- hair_color %>% fct_relevel("red", "blond", "brown")
hair_color

hair_color %>% as.numeric() # the levels and order changed

#video6
library("faraway")
library("ggplot2")
library("ggthemes")
library("dplyr")
library("gridExtra")
library(forcats)

library("dlnm")
data("chicagoNMMAPS")
head(chicagoNMMAPS)

chic <- chicagoNMMAPS

chic_july <-chic %>% 
  filter(year ==1995 & month ==7) 

chic %>% 
  mutate(dow= fct_relevel(dow, "Saturday")) %>% 
  ggplot(aes(x=o3))+
  geom_histogram(fill="skyblue", color="black") +
  facet_wrap(~ dow, ncol=2)
  
#now look at facte grid
chic %>% 
  mutate(dow= fct_relevel(dow, "Saturday")) %>% 
  ggplot(aes(x=o3))+
  geom_histogram(fill="skyblue", color="black") +
  facet_grid (~ dow, ncol=2)
#now look at changing way grid looks

chic %>% 
  mutate(dow= fct_relevel(dow, "Saturday")) %>% 
  ggplot(aes(x=o3))+
  geom_histogram(fill="skyblue", color="black") +
  facet_grid(dow ~ .)

#Video7
data("worldcup")

worldcup %>% 
  ggplot(aes(x=Time, y=Passes, color=Position, size=Shots)) +
  geom_point(alpha=0.5)+
  scale_x_continuous(name="Time played (min)", 
                     breaks=90*c(2,4,6),
                     minor_breaks = 90*c(1,3,5))
  

chic_july %>% 
  ggplot(aes(x=date, y=death))+
  geom_line()+
  scale_x_continuous(name="Date in July 1995")

#changes date label from Jul to July
chic_july %>% 
  ggplot(aes(x=date, y=death))+
  geom_line()+
  scale_x_date(name="Date in July 1995",
               date_labels="%B %d")
#changes date label to 07 for July
chic_july %>% 
  ggplot(aes(x=date, y=death))+
  geom_line()+
  scale_x_date(name="Date in July 1995",
               date_labels="%m %d")
