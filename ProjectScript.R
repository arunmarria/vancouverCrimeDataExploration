##loading libraries
library(rmarkdown)
library(dplyr)

library(rgdal)
library(proj4)

## importing data set
setwd("C:/Users/I853328/OneDrive - SAP SE/Data science/Project 1 Vancouver crime data/R workspace/")
crime_data <- read.csv("crime_csv_all_years.csv", header = TRUE)


##Exploring top/bottom rows of the data set
head(crime_data)
tail(crime_data)

str(crime_data)


## converting x y corrdinates to latitude and longitude respectively'
proj4string<-"+proj=utm +zone=10 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"



pj <- project(crime_data[, c('X', 'Y')],proj4string, inverse = TRUE)

crime_data<-cbind(crime_data,pj)

## changing colnames to latitude and longitude respectively for appeneded columns
colnames(crime_data)[11] <-"lon"
colnames(crime_data)[12] <-"lat"

## removing x and y coordinates

crime_data <- select(crime_data, -c('X','Y'))

## checking for nas

head((crime_data %>%  filter(NEIGHBOURHOOD=="")),5)

## setting missinf values for neighbourhoood as crime data as private
levels(crime_data$NEIGHBOURHOOD)[1] <- "Not revealed"


crime_data %>%  filter(NEIGHBOURHOOD=="Not revealed")

## removing hundred's block column
crime_data <- select(crime_data, -c("HUNDRED_BLOCK"))



## working with date and time columns

library(lubridate)

##exploring some date info to understand the package contents
crime_data<- crime_data %>%  mutate(TIMESTAMP =make_datetime(YEAR, MONTH, DAY, HOUR,MINUTE))


str(crime_data)


## reordering columns to put date forward and removing minutes column as we are not analyzing data at minute level


crime_data <- crime_data[ ,c(10,2,3,4,5, 8,9,7,1) ]


## Just gathering data upto 2017
crime_data<-  crime_data %>% filter(YEAR<2018)


##bar charts

## bar chart for crimes in differnt years. 
library(repr)
library(ggplot2)

  options(repr.plot.width = 6, repr.plot.height = 5)
 
   bar_chart_years<- 
    ggplot(crime_data,aes(x= YEAR))+
    geom_histogram(binwidth = (max(crime_data$YEAR)-min(crime_data$YEAR))/28, fill ="royalblue3" )+
    labs(y = "Number of crimes", x= "Year", title ="Crimes in Vancouver over last few years")+
    theme(axis.text.x = element_text(angle = 50, vjust = 0.5),
          panel.background = element_rect(fill =   "lavender"))+
    scale_x_continuous(breaks = unique(crime_data$YEAR))
   
   bar_chart_years
   
   
   bar_chart_types <- ggplot(crime_data, aes(TYPE))+
     geom_bar(fill = "royalblue3")+ 
     labs(x="Crime Type", y="Number of crimes", title = "Crime categories")+
     theme(axis.text.x = element_text(angle = 90,vjust = 0.5), panel.background = element_rect(fill = "lavender"))
     

   
   ## As number of fatalities with vehiclle collision are very less clubbing it togther with injuries
   ## Also clubbing together Theft of bicycle and theft of vehicles
   
   crime_data$TYPE<- as.character(crime_data$TYPE)
 
   crime_data$TYPE<-sapply(crime_data$TYPE, function(x){ifelse(x =="Vehicle Collision or Pedestrian Struck (with Fatality)"|x=="Vehicle Collision or Pedestrian Struck (with Injury)",
                                            "Veh. collision/Pedestrian struck(Injury & Fatality)",x )})
    
   
   crime_data$TYPE<-sapply(crime_data$TYPE, function(x){ifelse(x =="Theft of Vehicle"|x=="Theft of Bicycle",
                                                               "Theft of Vechicle/Bicycle",x )})
   crime_data$TYPE <-as.factor(crime_data$TYPE)
   
   
   
   ##plotting crime by categories again
   
   bar_chart_types <- ggplot(crime_data, aes(TYPE))+
     geom_bar(fill = "royalblue3")+ 
     labs(x="Crime Type", y="Number of crimes", title = "Vancouver Crime numbers by Crime Types")+
     theme(axis.text.x = element_text(angle = 90,vjust = 0.5), panel.background = element_rect(fill = "lavender"))
   
   bar_chart_types

   
   
   ## by location
   
   
   bar_chart_loc <- ggplot(crime_data, aes(NEIGHBOURHOOD))+
     geom_bar(fill = "royalblue3")+ 
     labs(x="Region", y="Number of crimes", title = "Vancouver Crime numbers by Region")+
     theme(axis.text.x = element_text(angle = 90,vjust = 0.5), panel.background = element_rect(fill = "lavender"))
   
   bar_chart_loc
   
   
   
   ##Analysing in detail yearly crime data from 2010-2017
  plot_all <- ggplot(crime_data %>% filter(YEAR>2009), aes(NEIGHBOURHOOD, fill = TYPE))+
     geom_bar()+ 
     labs(x="Region", y="Number of crimes", title = "Vancouver yearly crime numbers")+
     theme(axis.text.x = element_text(angle = 90,vjust = 0.5), 
           panel.background = element_rect(fill = "lavender"),
           legend.text = element_text(size = 7), legend.title = element_blank(), legend.position = "bottom")+
     facet_wrap(~YEAR, ncol =2)
  
  
  
  
  ## insgight number similar over years in terms of location and type
  ## not revealed for crimes against a person
  ##maximum centtral business district
  ## minimum stanley park
  
  
  plot_all2<-ggplot(crime_data, aes(NEIGHBOURHOOD, fill = TYPE))+
    geom_bar()+ 
    labs(x="Region", y="Number of crimes", title = "Vancouver yearly crime numbers")+
    theme(axis.text.x = element_text(angle = 90,vjust = 0.5), 
          panel.background = element_rect(fill = "lavender"),
          legend.text = element_text(size = 7), legend.title = element_blank(), legend.position = "bottom")
  
  
  ## getting more location based info and seeing over maps for last two years
  
  
  library(mapview)
  
  ## reading shape file
  
  yvr_shp1 <- readOGR(".", "crime_shp_2016")
  yvr_shp2 <- readOGR(".", "crime_shp_2017")
  
  
  ## getting information about theft from vehicle and theft of vehicle
  
  ## filtering data in shape file for these type of crimes
  library(mapedit)
  
  
yvr_shp<-  rbind(yvr_shp1,yvr_shp2)


library(sf)
library(dplyr)




yvr_shp_df <- as.data.frame(yvr_shp)


yvr_shp_df_2016 <-  yvr_shp_df %>% filter( (TYPE =="Theft from Vehicle"| TYPE == "Theft of Bicycle"| TYPE =="Theft of Vehicle"),YEAR ==2016)
   
yvr_shp_df_2017 <-  yvr_shp_df %>% filter( (TYPE =="Theft from Vehicle"| TYPE == "Theft of Bicycle"| TYPE =="Theft of Vehicle"), YEAR ==2017)


yvr_theft_vech_2016 <- SpatialPointsDataFrame(coords = yvr_shp_df_2016[,c(9,10)], data = yvr_shp_df_2016, proj4string = CRS("+proj=utm +zone=10 +datum=NAD83 +units=m +no_defs +ellps=GRS80
+towgs84=0,0,0"))


yvr_theft_vech_2017 <- SpatialPointsDataFrame(coords = yvr_shp_df_2017[,c(9,10)], data = yvr_shp_df_2017, proj4string = CRS("+proj=utm +zone=10 +datum=NAD83 +units=m +no_defs +ellps=GRS80
+towgs84=0,0,0"))



mapview(yvr_theft_vech_2016, zcol = c("TYPE","N_HOOD"), legend =TRUE)
mapview(yvr_theft_vech_2017, zcol = c("TYPE","N_HOOD"), legend =TRUE)

## add code for google map
library(ggmap)


##Analysing time within year/month and within day



##plotting monthly info all years



bar_chart_monthly_all_years <- ggplot(filter(crime_data, !is.na(TIMESTAMP) ), aes(month(TIMESTAMP, label=TRUE), fill = TYPE))+
  geom_bar()+ 
  labs(x="Month", y="Number of crimes", title = "Monthly Crime numbers for all years")+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5),
        panel.background = element_rect(fill = "lavender"),
        legend.position = "bottom")



## for individual years to see if trend is similar
bar_chart_monthly_last4_years <- ggplot(filter(crime_data, !is.na(TIMESTAMP), YEAR>2013 ), aes(month(TIMESTAMP, label=TRUE), fill = TYPE))+
  geom_bar()+ 
  labs(x="Month", y="Number of crimes", title = "Monthly Crime numbers last 4 years")+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5),
        panel.background = element_rect(fill = "lavender"),
        legend.position = "bottom")+
  facet_wrap(~YEAR,ncol=2)

##day wise crime data all years

bar_chart_daily_all_years <- ggplot(filter(crime_data, !is.na(TIMESTAMP) ), aes(wday(TIMESTAMP, label=TRUE), fill = TYPE))+
  geom_bar()+ 
  labs(x="Day", y="Number of crimes", title = "Daily Crime numbers for all years")+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5),
        panel.background = element_rect(fill = "lavender"),
        legend.position = "bottom")

bar_chart_daily_last4_years <- ggplot(filter(crime_data, !is.na(TIMESTAMP), YEAR>2013 ), aes(wday(TIMESTAMP, label=TRUE), fill = TYPE))+
  geom_bar()+ 
  labs(x="Day", y="Number of crimes", title = "Daily Crime numbers last 4 years")+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5),
        panel.background = element_rect(fill = "lavender"),
        legend.position = "bottom")+
  facet_wrap(~YEAR,ncol=2)



## hour of the day crimes
bar_chart_hourly_all_years <- ggplot(filter(crime_data, !is.na(TIMESTAMP) ), aes(hour(TIMESTAMP), fill = TYPE))+
  geom_bar()+ 
  labs(x="Hour of the day", y="Number of crimes", title = "Hourly Crime numbers for all years")+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5),
        panel.background = element_rect(fill = "lavender"),
        legend.position = "bottom")+
  scale_x_continuous(breaks = unique(hour(crime_data$TIMESTAMP)))



bar_chart_hourly_last4_years <- ggplot(filter(crime_data, !is.na(TIMESTAMP), YEAR>2013 ), aes(hour(TIMESTAMP), fill = TYPE))+
  geom_bar()+ 
  labs(x="Hour of the day", y="Number of crimes", title = "Daily Crime numbers last 4 years")+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5),
        panel.background = element_rect(fill = "lavender"),
        legend.position = "bottom")+
  facet_wrap(~YEAR,ncol=2)


