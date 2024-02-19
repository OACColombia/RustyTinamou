#_Turdus fuscater_

library(cowplot)# ggdraw
library(tidyverse) # maneja datos
library(data.table) #manejar tables

data <- read.csv("Acoustic_records.csv")
head(data)

data<-subset(data,Month!="NA")

annual <- data %>%
  group_by(Source,Month) %>%
  summarise(freq = n())

head(annual)

Aabsences <- data.frame("Source" = rep("ARBIMON",11),
                      "Month" = c(1:7,9:12),
                      "freq" = rep(0,11))

MLabsences <- data.frame("Source" = rep("Macaulay Library",8),
                        "Month" = c(2,4:7,9,11:12),
                        "freq" = rep(0,8))

XCabsences <- data.frame("Source" = rep("xeno-canto",5),
                         "Month" = c(1:3,6,11),
                         "freq" = rep(0,5))

an <- rbind(Aabsences,MLabsences,XCabsences,annual)

an<-as.data.table(an)#to work with data.table

an[, fct_month := factor(Month, levels = 1:12, labels = month.abb)]

anf <- ggplot(an, aes(x = fct_month, y = freq)) + 
  geom_col(aes(fill = Source))+
  scale_fill_grey()+
  scale_y_continuous(expand = c(0, 0)) +
  coord_polar(start = -0.3) +
  theme_bw() + xlab(NULL) + ylab(NULL) +
  labs(title = "Annual pattern")+
  theme(legend.position = "none")

#Dial
diel <- data %>%
  group_by(Source,Hour) %>%
  summarise(freq = n())

head(diel)

dAabsences <- data.frame("Source" = rep("ARBIMON",20),
                        "Hour" = c(0:16,18,19,21),
                        "freq" = rep(0,20))

dMLabsences <- data.frame("Source" = rep("Macaulay Library",23),
                         "Hour" = c(0:15,17:23),
                         "freq" = rep(0,23))

dXCabsences <- data.frame("Source" = rep("xeno-canto",18),
                         "Hour" = c(1:4,7:16,19:22),
                         "freq" = rep(0,18))

di <- rbind(dAabsences,dMLabsences,dXCabsences,diel)

dif <- ggplot(di, aes(x = Hour, y = freq)) + 
  geom_col(aes(fill = Source))+
  scale_fill_grey()+
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(breaks = c(0,2,4,6,8,10,12,14,16,18,20,22))+
  coord_polar(start = -0.1) +
  theme_bw() + xlab(NULL) + ylab(NULL) +
  labs(title = "Diel pattern")+
  theme(legend.position = "none")

ggdraw()+
  draw_plot(anf, x = 0, y = 0, width = 0.5, height = 1)+
  draw_plot(dif, x = 0.5, y = 0, width = 0.5, height = 1)
