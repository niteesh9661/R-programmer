#TIME SERIES (It is the analysis of the time and a specific intervals between observations)
#There are 4 types of Time series
# Regular TS- Specific Interval between observations
# Irregular TS- No fixed interval Btwn observation changes
# Univariate TS- Time stamp and a single variable.names
# Multivriate TS- Time stamp and Multiple variables

#Seps in Time series
#STEP 1 Change Colum if required in excel

#STEP 2 Import data
store=read.csv(file.choose())

#STEP 3 Clean Data- Missing Values, Outliers, Merge, Split etc

#STEP 4 Convert the data to time series object to object

#STEP 5 EDA (STR SUMMARY)
str(store)
summary(store)
#Build ts object
#ts(data,start,frequency)
myts=ts(data = store$X2,start = 1,frequency = 12)


#STEP 6 Visualization- Mutiple Graphs for understanding patterns
# Graph without ggplot ( use any1 method in exam)
# Graph 1
plot(myts)

# Graph 2
monthplot(myts,labels=1:12,xlab="Bidaily units")

# Graph 3
seasonplot(myts,season.labels=F,xlab="")


# Graph using ggplot package 
# Add on packages for advanced plots
library(forecast)
library(ggplot2)

# Graph 1 
autoplot((myts))

# Graph 2
ggseasonplot(myts,season.labels=1:12)

# Graph3
# ggmonth plot will not allow to change the x axis graph values
ggmonthplot(myts)

#STEP 7 Forecasting using ARIMA MODEL
plot(forecast(auto.arima(myts)))

