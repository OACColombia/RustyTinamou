#####   Confirmation distributional hypothesis of birds of Colombia   ####

####~Packages~####
library(auk) #eBird data filters
library(tidyverse) #manipulate data
library(ggmap)

####################
####~ eBird data ~####
####################

#Data downloaded from eBird, up to December 2022

#reference to an ebd in preparatio for filtering using awk
auk_ebd("ebd_rustin1_relMar-2023/ebd_rustin1_relMar-2023.txt")

##I create a path to a new polygon with to save my filter ()
f_ebd <- "ebd_Crybre_OAC.txt"#Data to be saved

#Filtering the presence data
ebd_filt<-auk_ebd("ebd_rustin1_relMar-2023/ebd_rustin1_relMar-2023.txt") %>%
  #spatially subsample data that are <5 km long
#  auk_distance(distance = c(0,5)) %>%            #Remove the numeral at the beginning if you want to filter lists >5km
  #only keep checklists that are less than 5 hours
#  auk_duration(duration = c(0,300))%>%           #Remove the numeral at the beginning if you want to filter lists >5hrs
  #Exclude Historical ISS
#  auk_protocol(c("Traveling", "Stationary")) %>% #Remove the numeral at the beginning if you want to filter lists with detailed effort protocol
  #To provide data with no detections
#  auk_complete() %>%.                            #Remove the numeral at the beginning if you want to filter complete list (detection/no detection)
  auk_filter(f_ebd,overwrite=T)

#and with read_ebd I apply another filter to do not repeate records from groups
ebd_only <- read_ebd(f_ebd) 

write.csv(ebd_only, "Crypbrev_NoEffort.csv") #also Crypbrev_effort5x5complete.csv

#Map records
am <- c(left = -80, bottom = -15, right = -40, top = 15)
AmericaMap <- get_stamenmap(am, zoom = 5, maptype = "toner-lite") %>% 
  ggmap()

AmericaMap +
  geom_point(data = ebd_only,
             aes(x = longitude, y = latitude), 
             shape = 21, 
             alpha = 0.25)+
  theme_classic() + 
  labs(x = "Longitude", y = "Latitude", fill = "") +
  theme(legend.position = "bottom")
