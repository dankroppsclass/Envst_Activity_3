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



#HOMEWORK A3

#Question 1
#Make a graph that communicates about emissions from any countries of your choice. Explain how
#you considered principles of visualization in making your graph.

#Save entity for both Denmark and the United States

USUKCH = datCO2 %>%
  filter(Entity == "United States" |
           Entity == "China" |
           Entity == "United Kingdom")

#make plot of US vs Denmark

ggplot(USUKCH,
       aes(x=Year, ymin=0, ymax=CO2, fill=Entity))+ #fill works for polygons/shaded areas 
  geom_ribbon(alpha=0.5 )+ #fill in with 50% transparency
  labs(title = "CO2 Emissions US, UK, China", x="Year", y="Annual emissions (tons CO2)")+
  theme_classic()+
  scale_x_continuous(limits = c(1800, NA))

#Question 2
#You are tasked with communicating the change in world air temperatures and CO emissions to a
#broad audience in visually appealing graphs. Make two graphs to present in your word document side
#by side. Plot world CO emissions on one graph and world air temperature anomalies on the other
#graph.

#Use all co2 to graph total accumulation

totalglobal_co2 <- datCO2 %>%
  group_by(Year) %>%
  summarise(total_co2 = sum(CO2, na.rm = TRUE))

#plot using ggplot
ggplot(totalglobal_co2,
       aes(x = Year, ymin = 0, ymax = total_co2, fill = "Total CO2")) +
  geom_ribbon(alpha = 0.5) +
  labs(x = "Year", y = "Annual emissions (tons CO2)") +
  theme_classic()+
  scale_fill_manual(values = "blue")+
  scale_x_continuous(limits = c(1880, NA))

#save world inputs as dataframe
worldAll <- tempAnom[tempAnom$Entity == "World",]

#plot using a gradient to show temperature
ggplot(worldAll, aes(x = date, y = temperature_anomaly,
                     color = temperature_anomaly)) +
  geom_line(size = 1) +
  geom_point() +
  scale_color_gradient2(
    low = "darkblue",
    mid = "blue",
    high = "red",
    midpoint = 0
  ) +
  labs(
    x = "Year",
    y = "Temperature Anomaly",
    color = "Temp Anomaly"
  ) +
  theme_classic()

#Question 3
#Look up any type of environmental data of your interest in our world in data (link in tutorial).
#Download the csv and upload it to RStudio Cloud. Remake the graph. You may make the graph exactly
#as it is or alter it to present the data in a different format. Explain how you considered principles of
#visualization in making your graph. Explain the main conclusion of the graph.

biodiv <- read.csv("/cloud/project/activity03/global-living-planet-index.csv")

# Keep only World entities
world <- biodiv[biodiv$Entity == "World", ]

# Plot central estimate
plot(world$Year, world$Central.estimate,
     type = "l",
     col = "maroon",
     lwd = 2,
     xlab = "Year",
     ylab = "Percentage %",
     main = "World Wildlife Abundance Estimates Over Time")

# Add upper and lower lines
lines(world$Year, world$Upper.estimate, col = "darkgrey", lwd = 2)
lines(world$Year, world$Lower.estimate, col = "grey", lwd = 2)

# Add legend
legend("topright",
       legend = c("Central", "Upper", "Lower"),
       col = c("maroon", "darkgrey", "grey"),
       lwd = 2)


#Question 4
#Copy the URL to your R script here.


