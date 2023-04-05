library(readr)
# ymcgill <- 1871:1993
# ymctavish <- 1994:2022

# detect last date of the dataset then download until today
yrlast <- max(mtl$Year)
yrtoday <- year(today())

ymctavish <- yrlast:yrtoday

idmcgill <- 5420
idmctavish <- 10761

get_url <- function(year, id){
  lnk_1 <- 'https://climate.weather.gc.ca/climate_data/bulk_data_e.html?format=csv&stationID=' 
  lnk_2 <- '&Year='
  lnk_3 <- '&Month=7&Day=1&time=&timeframe=2&submit=Download+Data'
  return(paste0(lnk_1, id, lnk_2, year, lnk_3))
}

get_data <- function(year, id){
  lnk <- get_url(year, id)
  return(read_csv(lnk))
}

mcgill <- mctavish <- list()

for(i in 1:length(ymcgill))
  mcgill[[i]] <- get_data(ymcgill[i], id = idmcgill)

for(j in 1:length(ymctavish))
  mctavish[[j]] <- get_data(ymctavish[j], id = idmctavish)

# bind_rows will encounter same col with different format logical vs character
mcg <- do.call(rbind, mcgill)
mct <- do.call(rbind, mctavish)
mtlt <- rbind(mcg, mct)

library(dplyr)
library(lubridate)

mtlt <- mtlt %>% select(Station = 'Station Name', Date = 'Date/Time', 'Year', 'Month', 'Day', 
                       Max = 'Max Temp (°C)', Min = "Min Temp (°C)", Avg = "Mean Temp (°C)",
                       Rain = "Total Rain (mm)", Snow = "Total Snow (cm)", Precip = "Total Precip (mm)") %>% 
  filter(Date > "1871-07-01" & Date < today())

library(tidyr)
mtlt <- mtlt %>% drop_na(Max)

mtl <- rbind(mtl, mtlt) %>% distinct() %>% arrange(Year, Month, Day)
  
save(mtl, file = 'mtl_temperature.rda')

library(ggplot2)
exday <- mtl %>% 
  select(year = Year, month = Month, day = Day, 
         high = `Max Temp (??C)`, low = `Min Temp (??C)`,
         rain = `Total Rain (mm)`, snow = `Total Snow (cm)`, perp = `Total Precip (mm)`) %>% 
  group_by(year) %>% 
  mutate(hot = high >= 30, cold = low <= -20)# %>% 
  # summarise(hd = sum(hot), cd = sum(cold), n = n())

# hourly data
# year <- 1994:2022
yrlast <- max(mctavish_hourly$Year)
yrtoday <- year(today())

year <- yrlast:yrtoday


get_mctavish <- function(year){
  year_lst <- list()
  for(month in 1:12){
  lnk_1 <- 'https://climate.weather.gc.ca/climate_data/bulk_data_e.html?format=csv&stationID=10761&Year='
  lnk_2 <- '&Month='
  lnk_3 <- '&Day=1&time=LST&timeframe=1&submit=Download+Data'
  lnk <- paste0(lnk_1, year, lnk_2, month, lnk_3)
  year_lst[[month]] <- readr::read_csv(lnk)
  }
  year_df <- do.call(rbind, year_lst)
  return(year_df)
}

mcth <- list()

for(j in 1:length(year))
  mcth[[j]] <- get_mctavish(year[j])

names(mcth) <- year

# mctavish_hourly <- mctavish

# for complete data
# 1994 - 2000 have different format split up
mc1 <- mc2 <- list()

for(i in 1:7)
  mc1[[i]] <- mctavish_hourly[[i]]
for(j in 1:(length(year) - 7))
  mc2[[j]] <- mctavish_hourly[[j + 7]]

mct1 <- do.call(rbind, mc1)
mct2 <- do.call(rbind, mc2)

# for appended download data
mct_dat <- do.call(rbind, mcth) %>% 
  select(`Station Name`, DateTime = `Date/Time (LST)`, Year, Month, Day, Time = `Time (LST)`, Temp = `Temp (°C)`,
         Dew = `Dew Point Temp (°C)`, RelHum = `Rel Hum (%)`, WindDir = `Wind Dir (10s deg)`,
         WindSpd = `Wind Spd (km/h)`, StnPress = `Stn Press (kPa)`, Hmdx, Precip = `Precip. Amount (mm)`) %>% 
  drop_na(Temp)

# here heat index (feels like temperature will not be recorded, computation will be done when loading to use)

mc <- bind_rows(mctavish_hourly, mct_dat) %>% distinct()
mctavish_hourly <- mc
save(mctavish_hourly, file = 'mctavish_hourly.rda')

library(dplyr)
library(tidyr)
library(weathermetrics)

mctavish <- mctavish_hourly %>%  
  mutate(HI = heat.index(t = Temp, rh = RelHum, temperature.metric = 'celsius', output.metric = 'celsius', round = 1))

# end of data scratching and manipulation
load('mtl_temperature.rda')
load('mctavish_hourly.rda')

houly <- mctavish_hourly %>% 
   select(Station = `Station Name`, DateTime, Year, Month, Day, Time, Temp, RelHum, HI)

# http://www.bom.gov.au/info/thermal_stress/#atapproximation
# computation of apparent temperature
# t is temperature, rh is relative humidity in %, ws is wind speed in m/s
at <- function(t, rh, ws){
  return(t + .33*rh/100*6.105*exp(17.27*t/(237.7+t)) -0.7*ws - 4)
}

