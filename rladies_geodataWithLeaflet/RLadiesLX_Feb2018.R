## Set your working directory
setwd("~/Desktop/")

Sys.setenv(TZ='GMT') 

## Load all necessary packages >>> The tidyverse in packages <<<
packages <- c('tibble', 'reshape2', 'plyr', 'dplyr', 'lubridate', 'ggplot2', 
              'skimr', 'lubridate')
lapply(packages, require, character.only = TRUE)

# Question: what is the difference between library(package) and require(package)?



## Download the data from https://s3.amazonaws.com/tripdata/index.html
data <- read.csv("JC-201801-citibike-tripdata.csv", header = T)



## Question: how do you take a peek at the data?
head(data, 10)

str(data) # get the variables type

cols = c("start.station.id", "end.station.id", "gender")

data[cols] <- lapply(data[cols], factor) # transform variables into factor variables


str(data)

##1 - TIBBLE
data_as_tibble <- as.tibble(data)  # Note: this is equivalent to as_data_frame (from dplyr)



## 2 - DPLYR
# how do the numerical columns behave?

data %>% select_if(is.numeric) %>% skimr::skim()







# lubridate is the friendly parser of datetime objects

data$starttime <- ymd_hms(data$starttime)
data$stoptime <- ymd_hms(data$stoptime)




## PLYR

data_by_station <- ddply(data, .(start.station.id, usertype), summarise, 
                         latitude = mean(start.station.latitude), 
                         longitude = mean(start.station.longitude),
                         avg_trip_duration = mean(tripduration))

## GGPLOT2
m <- ggplot(data=data_by_station, 
       aes(x=longitude, y=latitude)) + geom_point(aes(size=avg_trip_duration, 
                                                     color=usertype))

m




## adding the Empire State building location

Empire_State_building_latitude <- 40.748817

Empire_State_building_longitude <- -73.985428	



m + geom_hline(yintercept = Empire_State_building_latitude, 
               linetype="dashed", color="grey") +
    geom_vline(xintercept = Empire_State_building_longitude, 
               linetype="dashed", color="grey")




## tracking popularity of stations


data_trips <- ddply(data, .(start.station.id, end.station.id), 
                     summarise,
                     total_rides = length(start.station.id))



## RESHAPE2

## melt allows you to frame your dataset into long format
data2  <- melt(data_trips)
head(data2)

## popularity of starting stations

starting_stations <- ddply(data, .(start.station.id), 
                           summarise, num_trips = length(start.station.id))


colnames(starting_stations)[colnames(starting_stations) == 'start.station.id'] <- 'station_ID'

       


data3 <- data[ , c("start.station.id", 
                  "start.station.latitude", 
                  "start.station.longitude")]

data <- merge(x=starting_stations, y=data3, 
                           by.x = "station_ID", by.y="start.station.id", 
             all.x=TRUE)

data <- unique(data)


lat_centre <- mean(data$start.station.latitude)
long_centre <- mean(data$start.station.longitude)




library(leaflet)

pal <- colorNumeric(
    palette = "RdYlGn",
    domain = data$size
)

data$size <- ((data$num_trips) - min(data$num_trips)) /(max(data$num_trips) - min(data$num_trips))*10
head(data, 3)        

m = leaflet(data) %>%   
    setView(lng = long_centre, lat = lat_centre, zoom = 10) %>%
    addTiles() %>%  # Adds in the default OpenStreetMap map tiles - we will overwrite in this case
    addProviderTiles("CartoDB.Positron") %>% # Overwriting with another map tile sset
    #addProviderTiles("Stamen.Toner") %>%
    addCircleMarkers(lat = ~start.station.latitude, 
                     lng = ~start.station.longitude,
                     radius = ~size,  # here we add in circles which correspond with pedestrian traffic
                     color = ~pal(num_trips), 
                     fill = TRUE, 
                     opacity = .8,
                     fillOpacity = .8) %>%
    addLegend("bottomright", pal = pal, values = ~size, # Adding a legend for colour interpretation
              title = "Pedestrian Volume",
              opacity = 1
    )

m


## adding icons
bike <- makeIcon(
  iconUrl="bike.png", 
  iconWidth = 38, iconHeight = 50,
  iconAnchorX = 20, iconAnchorY = 94)


m %>%
    addMarkers(~NY_Stock_Exchange_longitude, ~NY_Stock_Exchange_latitude, icon = bike)
