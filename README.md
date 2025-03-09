data=readxl::read_xlsx("C:/Users/alanj/Downloads/R/Gold prcie USD.xlsx")
str(data)
head(data,n=10)

# extract the year from data column
data$Year =format(data$Date, "%Y")
data


# check for any missing values or unexpected data types
summary(data)

# calculate the average price of gold for each year using the aggregate function
yearly_data = aggregate(USD~Year, data=data, FUN=mean)
yearly_data

#check thestructure of yearly_data to confirm aggeration
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

#extracting the month from the date column
data$Month =  format(data$Date,"%B")
data$Month
data


# calculate the average price of gold for each month using the aggregate function

monthly_data = aggregate(USD~Month, data=data, FUN=mean)
monthly_data

# ensure month is treated as a factor for proper plotting

monthly_datata$Month = factor(monthly_data$Month,levels = month.name)

# plotting the month wise average price of gold
ggplot(monthly_data, aes(x=Month, y=USD,group=1)) + 
  geom_line(color="blue", size=2) + # line plot
  geom_point(color="red", size=3) + # adding points for better visualization
  labs(title="Monthly Average Price of Gold",
       x="Month",
       y="Avg gold price per gram") +
  theme(axis.text.x = element_text(angle=90, hjust=1))

# forecast the range
library(forecast)
ts_data= ts(data$USD,start = c(1978,1),frequency = 12)
ts_data
arima_model= auto.arima(ts_data)
forecast_data= forecast(arima_model,h=60,level=c(90,95)) # forecast for 5 years

# plotting the forecasted data
autoplot(forecast_data) +
  labs(title="Gold Price Forecast",x="Year",y="Gold price")


# forecast the next 5 years
forecast_data =  forecast(arima_model,h=60)
forecast_data

#retrive the forecasted values (gold price for the next 5 years)
forecasted_values = forecast_data$mean
forecasted_values

#extract the forecasted values for the year 2029(month 49:60)
forecasted_values_2029 = forecasted_values[49:60]
forecasted_values_2029
forecasted_values_2029[12]

#forecast the gold price for the year 2027

arima_model= auto.arima(ts_data)
forecast_data= forecast(arima_model,h=36,level=c(90,95))

forecasted_values = forecast_data$mean
forecasted_values

forecasted_values_2027 = forecasted_values[25:36]
forecasted_values_2027
forecasted_values_2027[12]
