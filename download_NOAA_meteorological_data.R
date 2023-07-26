# This program will download meteorological data from the NOAA tides and currents
# database using the noaaoceans package to access the NOAA API
# The following data are downloaded: air temp, water temp, wind, air pressure,
# conductivity, humidity, and salinity

library(noaaoceans)
library(lubridate)
library(dplyr)
library(tidyr)

units = 'metric'

#creates a function to call the NOAA API and query the data for an entire month
#in a given year

call_api = function(station_id, year, month, data_product){
  #the date you would like the query to start at
  #set to first day of month
  start = paste0(year, month, "01")
  
  date = paste0(year, "-", month, "-", "01")
  date = as.Date(date, "%Y-%m-%d")
  #gets the number of the last day of the month
  last_day = days_in_month(date)
  #the data you would like the query to end at
  #set to last day of month
  end   = paste0(year, month, last_day)
  
  call_api_temp_data = query_coops_data(station_id = station_id, #calls the NOAA API
                                        start_date = start,
                                        end_date   = end,
                                        data_product = data_product,
                                        units = units
  )
  
  return(call_api_temp_data)
}

#https://www.statology.org/r-add-column-if-does-not-exist/
#custom function to add one or more columns to a data frame
#if they do not already exist
add_cols <- function(df, cols) {
  add <- cols[!cols %in% names(df)]
  if(length(add) != 0) df[add] <- NA
  return(df)
}

#the names of all of the products you would like to download
product_list = c("air_temperature", "water_temperature", "wind", "air_pressure",
                 "conductivity", "humidity", "salinity")
product_list_length = length(product_list)

month_list = c("01", "02", "03", "04", "05", "06","07", "08", "09", "10", "11", "12")
start_year = 2009
end_year = 2023

get_all_data_for_station = function(station){
  all_data = data.frame(matrix(nrow = 0, ncol = product_list_length + 1))
  colnames(all_data) = c("date", product_list)
  
  for(year in start_year:end_year){
    
    year_data = data.frame(matrix(nrow = 0, ncol = product_list_length + 1))
    colnames(year_data) = c("date", product_list)
    
    for(month in month_list){
      
      #creates an empty data frame to store the data from that month
      month_data = data.frame(date = NULL, value = NULL, data_type = NULL)
      
      #for each product in the product_list, query the api for that product and add
      #the results to month_data
      #a tryCatch is necessary because sometimes the data is not available for a specific
      #month and we would like the loop to keep going even if that is the case
      for(product_num in 1:product_list_length){
        query_results = data.frame()
        tryCatch({query_results = call_api(station, year, month, product_list[product_num])
        query_results = query_results[, c(1,2)]
        colnames(query_results) = c("date", "value")
        query_results = query_results %>% mutate(data_type = product_list[product_num])
        }, error = function(e){})
        month_data = rbind(month_data, query_results)
      }
      
      tryCatch({month_data = month_data %>% pivot_wider(names_from = data_type, values_from = value)
      #if data for one of the products is not there, this adds a column of NAs for that
      #product, this is necessary to rbind with other months that do have data for
      #that product
      month_data = month_data %>% add_cols(product_list)}, 
      error = function(e){})
      
      
      year_data = rbind(year_data, month_data)
    }
    
    all_data = rbind(all_data, year_data)
  }
  return(all_data)
}

setwd("D:/Hotel Research/Data")
station_8771341_data = get_all_data_for_station(8771341)

test = station_8771341_data %>% mutate(date = as.POSIXct(date, format = "%Y-%m-%d %H:%M"))
#some of the dates have missing values, since the data table is in sequential order, we can just
#give any missing values the value of the previous data + 6 minutes
for(x in 1:length(test$date)){
  if(is.na(test$date[x])){
    test$date[x] = test$date[x - 1] + 6
  }
}
#checks to see if any dates are repeated
#is 0, there are not repeated dates
sum(duplicated(test$date))
#converts missing values to NAs
test = test %>% mutate(air_temperature = na_if(air_temperature, ""), water_temperature = na_if(water_temperature, ""),
                       wind = na_if(wind, ""), air_pressure = na_if(air_pressure, ""))
#the beginning of the data has no measurements, so we remove all rows until the first measurement
first_non_missing_value = which(!is.na(test$air_temperature))[1]
test = test[-c(1:first_non_missing_value),]

#fills in any missing dates in the data table
test = test %>% complete(date = seq.POSIXt(first(test$date), last(test$date), by = "6 min"))
#imputes missing values with the previous measurement
#since many measurements are the same as the previous measurement, this is a reasonable imputation
test = test %>% fill(air_temperature, water_temperature, wind, air_pressure, .direction = "down")

write.csv(test, "test.csv")

write.csv(station_8771341_data, "station_8771341_meteorological_data.csv")

station_8771450_data = get_all_data_for_station(8771450)
write.csv(station_8771450_data, "station_8771450_meteorological_data.csv")

station_8771486_data = get_all_data_for_station(8771486)
write.csv(station_8771486_data, "station_8771486_meteorological_data.csv")

station_8771972_data = get_all_data_for_station(8771972)
write.csv(station_8771972_data, "station_8771972_meteorological_data.csv")


station_9087044_data = get_all_data_for_station(9087044)
