install.packages(c("ggplot2", "dplyr"))

library(ggplot2)
library(dplyr)

# read in data
# cloud is always lowercase
datCO2 <- read.csv("/cloud/project/activity03/annual-co-emissions-by-region.csv")

# check column names
colnames(datCO2)

# change the 4 column name
colnames(datCO2)[4] <- "CO2"
# check names again
colnames(datCO2)

# convert the entity names to factor and store a variable with levels for
# easy reference
datCO2$Entity <- as.factor(datCO2$Entity)
# make a vector of all levels
name.Ent <- levels(datCO2$Entity)

name.Ent

# new data frame for US
US <- datCO2[datCO2$Entity == "United States",]
# new data frame for Mexico
ME <- datCO2[datCO2$Entity == "Mexico",]


# make a plot of US CO2
plot(US$Year, # x data
     US$CO2, # y data
     type = "b", #b = points and lines
     pch = 19, # symbol shape
     ylab = "Annual fossil fuel emissions (billons of tons CO2)", #y axis label
     xlab = "Year", #x axis label
     yaxt = "n") # turn off y axis
# add y axis
# arguments are axis number (1 bottom, 2 left, 3 top, 4 right)
# las = 2 changes the labels to be read in horizontal direction
axis(2, seq(0,6000000000, by=2000000000), #location of ticks
     seq(0,6, by = 2), # label for ticks
     las=2 )
# add mexico to plot ----
# add points
points(ME$Year, # x data
       ME$CO2, # y data
       type = "b", #b = points and lines
       pch = 19, # symbol shape,
       col= "darkgoldenrod3")

#use ggplot to make nicer graphs. Start with dataframe
ggplot(US, aes(x=Year, y=CO2))+
  geom_point()+
  geom_line()+
  labs(x="Year", y="US fossil fuel CO2 emissions (tons CO2)")+
  theme_classic()

NorthA = datCO2 %>%
  filter(Entity == "United States" |
         Entity == "Mexico" |
         Entity == "Canada")

ggplot(NorthA,
      aes(x=Year, y=CO2, color=Entity))+
  geom_point()+
  geom_line()+
  scale_color_manual(values=c("red", "royalblue", "darkgoldenrod"))+
  theme_classic()

#In class prompts

install.packages(c("lubridate"))
library(lubridate)


tempAnom = read.csv("/cloud/project/activity03/climate-change.csv")
tempAnom$date <- ymd(tempAnom$Day)

#Make a plot of air temperature anomalies in the Northern and Southern Hemisphere in base R and in ggplot2.

# new data frame for US
northB <- tempAnom[tempAnom$Entity == "Northern Hemisphere",]
# new data frame for Mexico
southB <- tempAnom[tempAnom$Entity == "Southern Hemisphere",]


# make a plot of Northern Hemisphere CO2
plot(northB$date, # x data
     northB$temperature_anomaly, # y data
     type = "b", #b = points and lines
     pch = 19, # symbol shape
     ylab = "Temperature Anomaly", #y axis label
     xlab = "Date", #x axis label
     yaxt = "n",
     col = "salmon") # turn off y axis


# add y axis
# arguments are axis number (1 bottom, 2 left, 3 top, 4 right)
# las = 2 changes the labels to be read in horizontal direction
axis(2, seq(-2, 2, by=.5), #location of ticks
     las=2 )
# add Southern Hemisphere to plot ----
# add points
points(southB$date, # x data
       southB$temperature_anomaly, # y data
       type = "b", #b = points and lines
       pch = 19, # symbol shape,
       col= "darkgoldenrod3")

