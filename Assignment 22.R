library(tidyverse)
library(ggplot2)
library(lubridate)
library(dplyr)

read.csv("INDIA-AQI-DATA-2015-2020.csv") ->df
df
1#Trend Analysis Plot


#Create a time series plot of PM10 levels for at least 4 cities using different colors. Customize the theme.
#Hint: Use facet_wrap().
df %>% 
  mutate(year = Date %>% year(),
         month = Date %>% month(),
         day= Date %>% day(),
         week= Date %>% week(),
         weekday = Date %>% wday(label=T))-> df1
df1
df1 %>% 
  pivot_longer(3:14, names_to = "parameter", values_to = "values")-> df2
df2 %>% 
  filter(parameter=="PM10", City %in% c("Bengaluru", "Delhi", "Hyderabad", "Visakhapatnam","Amaravati")) %>%
  group_by(year,parameter,City) %>% 
  summarise(mean_value= mean(values, na.rm=T))-> df3
  #facet_wrap(~city)
df3 %>% 
  ggplot(aes(x=year, y=mean_value, colour = City))+
  geom_line(size=1)+
  theme_classic()+
  facet_wrap(~parameter, scale = "free_x")+
  labs(title = "PM10 trend analysis",
       subtitle = "Cities data",
       x=NULL,
       y=NULL)


#2Category-wise AQI Histogram


#Plot a histogram of the AQI values, color-coded by AQI_Bucket. Ensure the x-axis is clearly labelled and ticked at intervals of 50, up to 500.
#Hint: Use binwidth and scale_x_continuous().


df2 %>% 
  ggplot(aes(x=AQI, fill = AQI_Bucket))+
  geom_histogram(binwidth = 50, color = "green")+
  
  scale_x_continuous(breaks = seq(0,500, by=50),limits = c(0,500))+
  labs(title = "AQI Data",
       x="AQI",
       y="AQI_Bucket",
       caption = "AIQ data")


                      
#3 Pollutant Relationship Scatter Plot, Plot a scatter graph of PM2.5 vs NO where:
# Point color = AQI_Bucket
# Point size = O3
# Point shape = City
# Apply a custom theme.
df1 %>% 
  ggplot(aes(x= PM2.5, y = NO, color= AQI_Bucket, size = O3, shape = City))+
  geom_point(alpha=0.1)+
  scale_x_continuous(breaks = seq(0,500, by=50),limits = c(0,500))+
  scale_y_continuous(breaks = seq(0,200, by=50),limits = c(0,200))+
  theme_classic()+
  theme(
    legend.position = "Bottom"
  )+
  
  labs(title = "Pollutant Relationship Scatter Plot",
       subtitle = "PM2.5 vs NO",
       x = "PM2.5 Concentration",
       y = "NO Concentration",
       color = "AQI Category",
       size = "O3 Level",
       shape = "City"
  )-> NOvsPM2.5
print(NOvsPM2.5)





#4Faceted Histogram
#Create histograms of CO for each AQI_Bucket, each in its own facet, using free y-axis scales. Use appropriate fill color and remove grid lines.
df1 %>% 
  ggplot(aes(x = CO, fill = AQI_Bucket)) +
  geom_histogram(binwidth = 0.5, color = "red", show.legend = T) +
  facet_wrap(~ AQI_Bucket, scales = "free_y") +
  labs(title = "CO Histogram by AQI Category",
       x = "CO Level",
       y = "Count") +
  theme_classic() +
  theme(panel.grid = element_blank())

#5  SO2 Line Type Comparison
#For 2 cities of your choice (e.g., Delhi and Chennai), compare SO2 levels over time. Use different line types for each city and ensure the lines are clearly distinguishable.

df2 %>%
  df2$ate %>% 
  filter(City %in% c("Delhi", "Chennai")) %>%
  ggplot(aes(x = Date, y = SO2, color = City, linetype = City)) +
  geom_line(size = 3) +
  labs(title = "SO₂ Levels Over Time: Delhi vs Chennai",
       x = "Date",
       y = "SO₂ Concentration") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "top"
  )





#6 Annotate a Special Observation
# Find the day with the maximum AQI and highlight it on a scatter plot of PM2.5 vs PM10.
# Annotate the point with a label and different color/size.


# Find the day with the maximum AQI
max_aqi_day <- df %>% filter(AQI == max(AQI, na.rm = TRUE))
ggplot(df, aes(x = PM10, y = PM2.5)) +
  geom_point(alpha = 0.9) +  # Plot all points
  geom_point(data = max_aqi_day, aes(x = PM10, y = PM2.5),
             color = "red", size = 1) +  # Highlight max AQI point
  geom_text(data = max_aqi_day, aes(x = PM10, y = PM2.5, label = Date),
            vjust = -1, color = "red", fontface = "bold") +  # Annotate it
  labs(title = "PM2.5 vs PM10 with Max AQI Highlighted",
       x = "PM10 Concentration",
       y = "PM2.5 Concentration") +
  theme_minimal()





