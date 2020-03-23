library(readr)
library(dplyr)
library(tidyr)
url <- 'https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv'

dataset <- read_csv(url)
n_days <- ncol(dataset) - 4
colnames(dataset)[5:(4 + n_days)] <- paste0('d',sprintf("%02d", as.numeric(1:n_days)))

dataset <- dataset %>% select(-c(Long, Lat)) %>% 
                       rename(province = 'Province/State', country = 'Country/Region') %>%
                       arrange(country)

dataset2 <- dataset %>% group_by(country) %>% 
                        select(-province) %>%
                        summarise_all(sum) # %>% filter(country != 'China')

ca_today <- dataset2 %>% filter(country == 'Canada')
ca_today <- ca_today[n_days + 1]
ca_today <- as.numeric(ca_today)

dataset2 <- dataset2 %>% filter(.[[n_days + 1]] > 1000)                      

wtl <- dataset2 %>% pivot_longer(-country, names_to = 'day')
wtl$date <- rep(1:n_days, nrow(wtl)/n_days)

ctys <- c('Canada', 'US', 'Italy', 'Korea, South', 'Iran')

wtl2 <- wtl %>% filter(!country %in% ctys)
wtl3 <- wtl %>% filter(country %in% ctys)

library(ggplot2)

p1 <- ggplot(wtl2, aes(x = date, y = value)) + theme_bw() + 
  geom_path(aes(group = country), colour = 'gray60') + 
  geom_path(data = wtl3, aes(group = country, colour = country), size = 1) + 
  scale_x_continuous(breaks = c(1, seq(5, n_days, by = 5))) + 
  scale_y_log10(breaks = 10^seq(1:5), label = c('10', '100', '1,000', '10,000', '100,000')) +
  ggtitle('Confirmed Coronavirus Growth for Some Severe Countries') + 
  ylab('confirmed cases') + xlab('day') + 
  theme(plot.title = element_text(hjust = 0.5))

n_country <- nrow(dataset2)

# growth after first 100
for(i in 1:n_country){
  x <- dataset2[i,]
  obs <- as.numeric(x[-1])
  obs <- obs[obs > 100]
  na <- rep(NA, n_days - length(obs))
  y <- c(obs, na)
  x[2:(n_days + 1)] <- y
  dataset2[i,] <- x
}

wtl <- dataset2 %>% pivot_longer(-country, names_to = 'day')
wtl$date <- rep(1:n_days, nrow(wtl)/n_days)

ctys <- c('Canada', 'US', 'Italy', 'Korea, South', 'Iran')

wtl2 <- wtl %>% filter(!country %in% ctys)
wtl3 <- wtl %>% filter(country %in% ctys)


p2 <- ggplot(wtl2, aes(x = date, y = value)) + theme_bw() + 
  geom_path(aes(group = country), colour = 'gray60', na.rm = TRUE) +
  geom_path(data = wtl3, aes(group = country, colour = country), na.rm = TRUE, size = 1) + 
  scale_x_continuous(breaks = c(1, seq(5, n_days, by = 5))) + 
  scale_y_log10(breaks = 10^seq(1:5), label = c('10', '100', '1,000', '10,000', '100,000')) +
  ggtitle('Growth after first 100 Confirmed Cases') + 
  ylab('confirmed cases') + xlab('day') + 
  theme(plot.title = element_text(hjust = 0.5))
