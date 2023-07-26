library(readxl)
library(dplyr)
library(lubridate)
library(fda)

setwd("D:/Hotel Research/Data")

enteros = read_xlsx("BW Data _2009-Feb2022_Final_Island.xlsx")
enteros = enteros %>% mutate(`SAMPLE TIME` = substr(`SAMPLE TIME`, 12, 13))
enteros = enteros %>% mutate(date = as.POSIXct(paste0(`SAMPLE DATE`, " ", `SAMPLE TIME`, ":00")))


enteros = enteros %>% filter(year(date) > 2012)
enteros = enteros %>% filter(`SITE ID` == "GAL055")
enteros = enteros %>% mutate(`ENTERO RESULT` = log(`ENTERO RESULT`, 10))

test_data = read.csv("D:/Hotel Research/Code/functional_enterococci_analysis/test.csv")

test = test %>% mutate(air_temperature = as.numeric(air_temperature), water_temperature = as.numeric(water_temperature),
                       wind = as.numeric(wind), air_pressure = as.numeric(air_pressure))

#binary search function that works on dates
binarySearch <- function(table, key, start.idx = 1, end.idx = length(table),
                         tol = .Machine$double.eps ^ 0.5) {
  # Takes sorted (in ascending order) vectors
  r <- length(table)
  m <- as.integer(ceiling((end.idx + start.idx) / 2)) # Midpoint
  if (table[m] > key + tol) {
    if (start.idx == end.idx) return(FALSE)
    Recall(table, key, start.idx = start.idx, end.idx = m - 1L, tol = tol)
  } else if (table[m] < key - tol) {
    if (start.idx == end.idx) return(FALSE)
    Recall(table, key, start.idx = m + 1L, end.idx = end.idx, tol = tol)
  } else return(m)
}

water_temperature_data = data.frame()
air_temperature_data = data.frame()
wind_data = data.frame()
air_pressure_data = data.frame()
for(day in 1:nrow(enteros)){
  entero_date_end = enteros$date[day]
  entero_date_begin = entero_date_end - days(1) + minutes(6)
  test_seq = seq(binarySearch(test$date, entero_date_begin), 
                 binarySearch(test$date,entero_date_end))
  new_data = test[test_seq,]
  air_temperature_data = rbind(air_temperature_data, new_data$air_temperature)
  water_temperature_data = rbind(water_temperature_data, new_data$water_temperature)
  wind_data = rbind(wind_data, new_data$wind)
  air_pressure_data = rbind(air_pressure_data, new_data$air_pressure_data)
}


basis = create.fourier.basis(c(0,240), 65)
smooth_air_temp = smooth.basis(c(1:240),y = t(air_temperature_data), basis)
Smooth_water_temp = smooth.basis(c(1:240),y = t(water_temperature_data), basis)

templist = vector("list", 3)
templist[[1]] = rep(1, dim(enteros)[1])
templist[[2]] = smooth_air_temp$fd
templist[[3]] = Smooth_water_temp$fd

conbasis = create.constant.basis(c(0,240))
betabasis = create.fourier.basis(c(0,240), 5)
betalist = vector("list", 3)
betalist[[1]] = conbasis
betalist[[2]] = betabasis
betalist[[3]] = betabasis
fRegressList = fRegress(enteros$`ENTERO RESULT`, templist, betalist)


betaestlist = fRegressList$betaestlist
tempbetafd = betaestlist[[3]]$fd
plot(tempbetafd, xlab="Day",
     ylab="Beta for temperature")

mean((enteros$`ENTERO RESULT` - fRegressList$yhatfdobj) ^ 2)


mean((lm(enteros$`ENTERO RESULT` ~ air_temperature_data[,240])$residuals) ^ 2)
