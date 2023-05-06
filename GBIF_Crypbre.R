#Orlando Acevedo-Charry
#Crypturellus brevirostris records

#Upload packages (remember to install those packages that you still don't have)
library(dismo) #this package is for extract data from GBIF
library(tidyverse) #data manipulation and figures
library(lubridate) #Manipulation of dates
library(ggmap) #generate elegant map with ggplot2

#1) Extract data from GBIF with dismo ####

data = gbif("Crypturellus","brevirostris")
colnames(data)

table(data$basisOfRecord)

#Map ####
am <- c(left = -95, bottom = -15.5, right = -45, top = 15)
AmericaMap <- get_stamenmap(am, zoom = 5, maptype = "toner-lite") %>% 
  ggmap()

cc <- scales::seq_gradient_pal("blue", "red", "Lab")(seq(0,1,length.out=10))

AmericaMap +
  geom_point(data = data,
             aes(x = lon, y = lat, fill = basisOfRecord), 
             shape = 21, 
             alpha = 0.5)+
  theme_classic() + 
  scale_fill_manual(values = cc) +
  labs(x = "Longitude", y = "Latitude", fill = "") +
  theme(legend.position = "bottom")

names(data)

data <- data %>%
  filter(basisOfRecord == "PRESERVED_SPECIMEN")

write_csv(data, "Crypbrev_specimensGBIF.csv")
