# This program will download meteorological data from the NOAA tides and currents
# database using the noaaoceans package to access the NOAA API
# The following data are downloaded: air temp, water temp, wind, air pressure,
# conductivity, humidity, and salinity

library(noaaoceans)
library(lubridate)
library(dplyr)
library(tidyr)

units = 'metric'

call_api = function(station_id, year, month, data_product){
  start = paste0(year, month, "01")
  
  date = paste0(year, "-", month, "-", "01")
  date = as.Date(date, "%Y-%m-%d")
  #gets the number of the last day of the month
  last_day = days_in_month(date)
  end   = paste0(year, month, last_day)
  call_api_temp_data = query_coops_data(station_id = station_id, #calls the NOAA API
                                        start_date = start,
                                        end_date   = end,
                                        data_product = data_product,
                                        units = units,
                                        #interval = hourly
  )
  
  return(call_api_temp_data)
}

product_list = c("air_temperature", "water_temperature", "wind", "air_pressure",
                 "conductivity", "humidity", "salinity")
product_list_length = length(product_list)

station = "8771341"
year = "2020"
month = "01"

month_data = data.frame(date = NULL, value = NULL, data_type = NULL)

for(product_num in 1:product_list_length){
  query_results = data.frame()
  tryCatch({query_results = call_api(station, year, month, product_list[product_num])
  query_results = query_results[, c(1,2)]
  colnames(query_results) = c("date", "value")
  query_results = query_results %>% mutate(data_type = product_list[product_num])
  }, error = function(e){})
  month_data = rbind(month_data, query_results)
}

month_data = month_data %>% pivot_wider(names_from = data_type, values_from = value)
xyz