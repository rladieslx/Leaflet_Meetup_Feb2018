## Set your working directory
# make sure all relevant files are available there
setwd("~/Desktop/")
Sys.setenv(TZ='GMT') 

## Load all necessary packages >>> The tidyverse in packages <<<
packages <- c('tibble', 'reshape2', 'plyr', 'dplyr', 'lubridate', 'ggplot2', 
              'skimr')
lapply(packages, require, character.only = TRUE)

# Question: what is the difference between library(package) and require(package)?



## Download the data from https://s3.amazonaws.com/tripdata/index.html
data <- read.csv("JC-201801-citibike-tripdata.csv", header = T)



## Question: how do you take a peek at the data?
head(data, 10)

str(data) # get the variables type

# As you can see, not all the variables'types are properly identified.
## Transform variables into factor variables

cols = c("start.station.id", "end.station.id", "gender")

data[cols] <- lapply(data[cols], factor) 

# Check the data again:
str(data)



#################################################
######## A very quick look at TIDYVERSE: ######## 
################################################# 


##1 - TIBBLE
data_as_tibble <- as.tibble(data)  
# Note: this is equivalent to as_data_frame (from dplyr)



## 2 - DPLYR
# how do the numerical columns behave?

data %>% select_if(is.numeric) %>% skimr::skim()




## 3 - LUBRIDATE
# lubridate is the friendly parser of datetime objects

data$starttime <- ymd_hms(data$starttime)
data$stoptime <- ymd_hms(data$stoptime)




## 4- PLYR

data_by_station <- ddply(data, .(start.station.id, usertype), summarise, 
                         latitude = mean(start.station.latitude), 
                         longitude = mean(start.station.longitude),
                         avg_trip_duration = mean(tripduration))


## 5- GGPLOT2
m <- ggplot(data=data_by_station, 
       aes(x=longitude, y=latitude)) + geom_point(aes(size=avg_trip_duration, 
                                                     color=usertype))

m


# think about this map as plotting a matrix: in the x axis you have the longitude (columns), 
# while in the y axis you have the latitude (rows).


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



## 6 - RESHAPE2

## melt allows you to frame your dataset into long format
data2  <- melt(data_trips)
head(data2)



#################################################
################# EXERCISE:  ####################
## map the popularity of each starting station ##
################################################# 


## first, define popularity
starting_stations <- ddply(data, .(start.station.id), 
                           summarise, num_trips = length(start.station.id))


# it's always good practice to name columns without full stops (think compatibility with python)
colnames(starting_stations)[colnames(starting_stations) == 'start.station.id'] <- 'station_ID'

       

# subset your initial dataset
data3 <- data[ , c("start.station.id", 
                  "start.station.latitude", 
                  "start.station.longitude")]


# merge it with latitude and longitude information
data <- merge(x=starting_stations, y=data3, 
                           by.x = "station_ID", by.y="start.station.id", 
             all.x=TRUE)

# remove all duplicates
data <- unique(data) 


# check the data
head(data)



# define where you want the "camera" to be centred on (to point at)
lat_centre <- mean(data$start.station.latitude)
long_centre <- mean(data$start.station.longitude)




# load the leaflet package
library(leaflet)


# define a coherent normalisation for your data points
data$size <- ((data$num_trips) - min(data$num_trips)) /(max(data$num_trips) - min(data$num_trips))*10
# Note: this normalisation ensures that size varies between 0 and 10.


# check the dataset
head(data, 3)        


# map this normalisation to a colour gradient, likeso:
pal <- colorNumeric(
    palette = "RdYlGn",
    domain = data$size
)


#Credit: https://rpubs.com/kieg/toronto-volume-leaflet



# always remember to think about the layering approach: 
# think simple first, and then add more content
m = leaflet(data) %>%   
    setView(lng = long_centre, lat = lat_centre, zoom = 10) %>%
    addTiles() %>%  # default
    #addProviderTiles("Stamen.Watercolor") %>% # another possible style
    addProviderTiles("CartoDB.Positron")%>% 
    addCircleMarkers(lat = ~start.station.latitude, 
                     lng = ~start.station.longitude,
                     radius = ~size,  
                     color = ~pal(size), 
                     fill = TRUE, 
                     opacity = .8,
                     fillOpacity = .8) %>%
    addLegend("bottomright", pal = pal, values = ~size, 
              title = "Station popularity",
              opacity = 1
    )

m


## adding icons
bike <- makeIcon(
  iconUrl="bike.png", 
  iconWidth = 38, iconHeight = 50,
  iconAnchorX = 20, iconAnchorY = 94)


m %>%
    addMarkers(~Empire_State_building_longitude, ~Empire_State_building_latitude, 
               icon = bike)



# Now it's showtime. Pick up an interesting dataset and tell the story that data 
# transpires in the form of a map or graph. If you want added inspiration, 
# feel free to use the resources mentioned in the slides.
