data=Gold_prcie_USD
str(data)
head(data)

# extract the year from data column
data$Year =format(data$Date, "%Y")
data$Year


# check for any missing values or unexpected data types
summary(data)

# calculate the average price of gold for each year using the aggregate function
yearly_data = aggregate(USD~Year, data=data, FUN=mean)
yearly_data

#check the structure of yearly_data to confirm aggeration
str(yearly_data)

#Ensure year is treated as a factor for propper plotting
yearly_data$Year = as.factor(yearly_data$Year)


# plotting the year wise average price of gold and trend line.
library(ggplot2)
ggplot(yearly_data, aes(x=Year, y=USD,group=1)) + 
  geom_line(color="blue", size=1) + # line plot
  geom_point(color="red", size=2) + # adding points for better visualization
  geom_smooth(method="lm", se=F, color="black",linetype="dashed", size=1) + # adding trend line
  labs(title="Yearly Average Price of Gold",
       x="Year",
       y="Avg gold price per gram") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=90, hjust=1)) 

library(zoo)

# rolling average of gold for first 12 days
data$rolling_average= rollmean(data$USD, k=12, fill=NA)
data

# plotting the rolling average of gold price
ggplot(data,aes(x=Date, y=USD)) +
  geom_line(color="blue") +
  geom_line(aes(y=rolling_average),color="red") +
  labs(title="Rolling Average Price of Gold",
       x="Date",
       y="Price")

#rolling average of gold for first 50 days
data$rolling_average= rollmean(data$USD, k=50, fill=NA)
data  

# plotting the rolling average of gold price for first 50 days

ggplot(data,aes(x=Date, y=USD)) +
  geom_line(color="blue") +
  geom_line(aes(y=rolling_average),color="red") +
  labs(title="Rolling Average Price of Gold",
       x="Date",
       y="Price")

#rolling average of gold for first 100 days

data$rolling_average= rollmean(data$USD, k=100, fill=NA)
data

# plotting the rolling average of gold price for first 100 days

ggplot(data,aes(x=Date, y=USD)) +
  geom_line(color="blue") +
  geom_line(aes(y=rolling_average),color="red") +
  labs(title="Rolling Average Price of Gold",
       x="Date",
       y="Price")

