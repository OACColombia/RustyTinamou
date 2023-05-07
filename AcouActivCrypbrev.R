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

annual$freq <- annual[,3] %>% mutate_if(is.numeric, ~1 * (. != 0))

a <- as.data.table(annual)

Aabsences <- data.frame("Source" = rep("ARBIMON",11),
                      "Month" = c(1:7,9:12),
                      "freq" = rep(0,11))

MLabsences <- data.frame("Source" = rep("Macaulay Library",8),
                        "Month" = c(2,4:7,9,11:12),
                        "freq" = rep(0,8))

XCabsences <- data.frame("Source" = rep("xeno-canto",5),
                         "Month" = c(1:3,6,11),
                         "freq" = rep(0,5))

an <- rbind(Aabsences,MLabsences,XCabsences,a)

an<-as.data.table(an)#to work with data.table

an[, fct_month := factor(Month, levels = 1:12, labels = month.abb)]

anf <- ggplot(an, aes(x = fct_month, y = Source, 
                           group = Source, colour = Source)) + 
  geom_line(alpha = 0.5) +
  geom_point(size = 5, aes(alpha = as.factor(freq))) +
  scale_y_discrete(expand = c(0, 1), breaks = NULL) +
  coord_polar() +
  theme_bw() + xlab(NULL) + ylab(NULL) +
  labs(title = "Annual pattern",alpha = "Detection")+
  theme(legend.position = "right")

#Dial
diel <- data %>%
  group_by(Source,Hour) %>%
  summarise(freq = n())

head(diel)

diel$freq <- rep(1,nrow(diel))

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

dif <- ggplot(di, aes(x = Hour, y = Source, 
               group = Source, colour = Source)) + 
  geom_line() +
  geom_point(size = 5, aes(alpha = as.factor(freq))) +
  scale_y_discrete(expand = c(0, 1), breaks = NULL) +
  scale_x_continuous(breaks = c(0,4,8,12,16,20))+
  coord_polar(start = -0.1) +
  theme_bw() + xlab(NULL) + ylab(NULL) +
  labs(alpha = "Detection",title = "Diel pattern")+
  theme(legend.position = "left")

ggdraw()+
  draw_plot(anf, x = 0, y = 0, width = 0.6, height = 1)+
  draw_plot(dif, x = 0.375, y = 0, width = 0.6, height = 1)
