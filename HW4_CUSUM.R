################################# Start of Code =====================================
rm(list = ls())
getwd()
setwd("G:/Georgia Tech/Analytical Models/Assignments")

install.packages("data.table")
install.packages("lubridate")
install.packages("dplyr")
install.packages("weatherData")

require(data.table)
require(lubridate)
require(dplyr)
require(ggplot2)
require(weatherData)
################################### Q3 ============================================

######################### Get Data and manipulate =================================


ATL_station_code = getStationCode("Atlanta",region="GA")

fetchJul2OctoberData <- function (year) {
  getSummarizedWeather (station_id = "KATL", 
                        start_date = paste(year,"-07-1", sep = ''), 
                        end_date = paste(year,"-10-31", sep = ''))
}

years = seq(1996,2015)
all_data <- Map(fetchJul2OctoberData, years)
final_data <- Reduce(rbind, all_data)


#CHecking for NAs.
sum(is.na(final_data$Mean_TemperatureF))
sum(is.na(final_data$Min_TemperatureF))
sum(is.na(final_data$Max_TemperatureF))

#Only one NA in mean temp. - entering value for that
final_data$Mean_TemperatureF[535] = (final_data$Max_TemperatureF[535] + final_data$Min_TemperatureF[535])/2


############################ Cusum Analysis ========================================

#We should do our analysis with Mean temp. as Min and Max temperatures can be inflated/
#deflated for a particular day but question says use the high temperature.

#Picking the critical value
#For the critical value of birthdays in class, we picked the middle value.

#Here, we are picking a critical value across all the years. It might have made sense
#to pick a separate critical value for each year due to "global warming" etc. but this
#will also work.

final_data$Month = month(final_data$Date)

#The average temperature for all the months can be taken as the critical value,
#going below which we can say that summer for Atlanta has ended. This is a good
#choice as so far we don't know when summer ends. Could be it ends in December.
#We cannot suppose anything.

###################Question says use Daily High temperature instead of the Mean.

critical = mean(final_data$Max_TemperatureF)

#Considering that the temperature can drop for a few days due to rain or other factors, 
#Setting a threshold of >10 seems right. We will try different values of threshold.

cusum_func <- function(data, critical_value, threshold, change = "down"){
  #data is a vector and the other two are numerical values.
  S = 0
  S_agg = 0
  index = 0
  for (i in data){
    index = 1 + index
    if(change == "down"){
    S = critical_value - i #IN our case we have to see the downward change.
    S_agg = max(0, S + S_agg)
    }
    else {
      S = i - critical_value
      S_agg = max(0, S + S_agg)
      }
  if(S_agg > threshold){
    return(index)
  }
    }
}


cusum_func_graph <- function(data, critical_value, threshold, change = "down"){
  #data is a vector and the other two are numerical values.
  S = 0
  S_agg = 0
  index = 0
  store_agg = vector()
  for (i in data){
    index = 1 + index
    if(change == "down"){
      S = critical_value - i #IN our case we have to see the downward change.
      S_agg = max(0, S + S_agg)
      store_agg = append(store_agg, S_agg)
    }
    else {
      S = i - critical_value
      S_agg = max(0, S + S_agg)
      store_agg = append(store_agg, S_agg)
    }
  }
  return(store_agg)
}

#Breaking up the data into years
final_data$Year = year(final_data$Date)

increment = 0
cnt = 0
date_vector = vector()

for( i in seq(1996, 2015, 1)){
  
  subset_data = final_data[final_data$Year == i, ]
  
  #A threshold of 35 makes sense as there should be a drop of 5 degrees for each day
  #of the week, on an average. Accounting for some hot days in between, a total
  #drop of 35 means that has changed.
  
  ind = cusum_func(subset_data$Max_TemperatureF, critical_value = critical, threshold = 35, "down")
  print(paste("Summer end date for the year", i, "is", subset_data$Date[ind]))
  
  cnt = cnt + 1
  #Making a vector of all the dates
  date_vector = append(date_vector, subset_data$Date[ind])
  
  #As we want to find the date when summer ends, we will have to take the average
  #of all the dates over the years. To take the average, bringing everything to
  #1 year:
  
  year(date_vector[cnt]) = 1970
  
}


#Taking the mean 
final_date = mean(date_vector)

print(final_date)

#The date we have is September 23.
#Thus we conclude that summer in Atlanta ends on 23rd September on an average.

#PLotting the change for 2015
S_t = data.frame(Cusum = cusum_func_graph(subset_data$Max_TemperatureF, critical_value = critical, threshold = 35, "down"), Index = subset_data$Date)

plt = ggplot(S_t, aes(x = Index, y = Cusum)) + geom_point() + geom_line()
plt + geom_hline(yintercept = 35)



#################################### Q4 ===========================================

monthly_data = final_data[,c("Year", "Max_TemperatureF", "Month")]

monthly_data1 = group_by(monthly_data, Year, Month)

#Summarizing on the average max temperature for a month for that year
agg_dataset = summarize(monthly_data1,
                        Max_TemperatureF = mean(Max_TemperatureF))

#First of all we will only consider the summer months in our analysis.
#Using the answer from the previous question, we will exclude October.

agg_dataset = agg_dataset[!agg_dataset$Month == 10, ]

#We can do two kinds of analysis - see the change over 20 years for each month separtely
#OR take an average temperature for the three summer months and see the change in the
#average.

#################################### Average Analysis =============================
Yearly_av = agg_dataset %>%
                        group_by(Year) %>%
                        summarize(Max_Temp_Mean = mean(Max_TemperatureF))

#Again, we take the critical as the average of all the years.
crit_value = mean(Yearly_av$Max_Temp_Mean)

#The threshold should not be very high this time as we are looking at Climate change
#over the years.

Year_of_change = Yearly_av[cusum_func(Yearly_av$Max_Temp_Mean, crit_value, 8, "up"),]

print(Year_of_change)

#We have chosen the threshold of 8 considering that a change of 2 degrees for 
#4 years consecutive would be an indication of climate change. Thus, equivalently,
#when the cumsum goes above 8, we call it a change.

#Thus, we conclude that the Atlanta summer has gotten warmer over the years and after
#aggregation over the years, this change can be definitively seen by 2012.

#Plot the same
S_t = data.frame(Cusum = cusum_func_graph(Yearly_av$Max_Temp_Mean, crit_value, 8, "up"), Index = Yearly_av$Year)

plt = ggplot(S_t, aes(x = Index, y = Cusum)) + geom_point() + geom_line()
plt + geom_hline(yintercept = 8)

############################# Per Month Analysis ===============================

#We can do a similar analysis per month - July, Aug, Sep - and then look at the
#median of the results.

agg_july = agg_dataset[agg_dataset$Month == 7,]
agg_aug = agg_dataset[agg_dataset$Month == 8,]
agg_sep = agg_dataset[agg_dataset$Month == 9,]


#July analysis
crit_july = mean(agg_july$Max_TemperatureF)

changeYear_July = agg_july[cusum_func(agg_july$Max_TemperatureF, crit_july, 8, "up"),]

print(changeYear_July)


#August analysis
crit_aug = mean(agg_aug$Max_TemperatureF)

changeYear_Aug = agg_aug[cusum_func(agg_aug$Max_TemperatureF, crit_aug, 8, "up"),]

print(changeYear_Aug)


#September analysis
crit_sep = mean(agg_sep$Max_TemperatureF)

changeYear_Sep = agg_sep[cusum_func(agg_sep$Max_TemperatureF, crit_sep, 8, "up"),]

print(changeYear_Sep)

Final_changeYear = median(c(changeYear_Sep$Year, changeYear_Aug$Year, changeYear_July$Year))

print(Final_changeYear)

#Thus both analysis give year of climate change as 2012.