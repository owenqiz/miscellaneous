# background about match momentum
# https://theanalyst.com/na/2021/11/what-is-match-momentum
# https://theanalyst.com/eu/2021/03/what-is-possession-value/
# use "Inspect" from the browser (Chrome/Edge) to obtain momentum data from fotmob
# for alternative method, we can use json to download information from fotmob, see
# https://github.com/Rodolsky/Match_Momentum_Scraping/blob/main/Match_Momentum_FotMob.ipynb

library(ggplot2)
library(ggformula)
library(tidyr)
library(patchwork)

library(dplyr)
library(highcharter)
library(httr2)

# method 1
# this method, need to go to the match webpage, and use 'inspect' to retrive momentum data
# some manipulation of the data is required before plotting
# the data is interpolated and interval is 20 second

dat <- read.table(file.choose(), quote="\"", comment.char="")
df <- data.frame(mm = t(dat))
rownames(df) <- NULL

offset <- 96

match <- df %>% slice(7:651) %>%  filter(mm != 'C') %>% filter(row_number() %% 2 == 0) %>% 
  mutate(adj = -1 * (as.numeric(mm) - offset)) %>% 
  mutate(index = row_number(),  Team = if_else(adj >= 0, "AC Milan", "Liverpool")) %>% 
  relocate(index, .before = 1)

forest <- match
save(forest, file = 'md04_h_forest.rda')

# Assuming `milan` is a data frame with a column `adj`
# Add a column for the x-axis (e.g., row number or another relevant factor)

# Create the bar plot
# p1 <- ggplot(match, aes(x = index/3, y = adj, fill = Team)) +
#   geom_col(alpha = 0.5) +
#   labs(x = "Minutes", y = "Momentum", title = "Match Momentum") +
#   theme(legend.position = "bottom") + theme_minimal() + 
#   scale_fill_manual(values = c("AC Milan" = "black", "Liverpool" = "red4"))
# 
# p2 <- ggplot(match, aes(x = index/3, y = adj)) +
#   geom_ribbon(aes(ymax = adj, ymin = 0), alpha = 0.5) +
#   labs(x = "Minutes", y = "Momentum", title = "Match Momentum") +
#   # scale_fill_manual(values = c("AC Milan" = "black", "Liverpool" = "red4")) +
#   theme(legend.title = element_blank()) + theme_minimal()

match <- ipswich

plt_mm <- function(match, rival, home = TRUE){
  
  if (home){
    # home game
    ttl <- paste('Match Momentum for Liverpool vs', rival)
    p <- ggplot(match, aes(x = index/3, y = adj)) +
      geom_ribbon(aes(ymax = pmax(adj, 0), ymin = 0), fill = 'red4', alpha = 0.5) +
      geom_ribbon(aes(ymax = 0, ymin = pmin(0, adj)), fill = 'darkblue', alpha = 0.5) +
      ggtitle(ttl)
  } else {
    # away game
    ttl <- paste('Match Momentum for', rival, 'vs Liverpool')
    p <- ggplot(match, aes(x = index/3, y = adj)) +
      geom_ribbon(aes(ymax = pmax(adj, 0), ymin = 0), fill = 'blue4', alpha = 0.5) +
      geom_ribbon(aes(ymax = 0, ymin = pmin(0, adj)), fill = 'red4', alpha = 0.5) +
      ggtitle(ttl)
  }
  p <- p + labs(x = "Minutes", y = "Momentum") +
    theme(legend.title = element_blank(),  axis.ticks.y=element_blank()) + 
    coord_fixed(ratio = 1/3) + theme_minimal()
  
  return(p)
}


p1 <- plt_mm(match = milan, rival = 'Milan', home = FALSE)

ggsave(filename = 'milan_m1.jpg', plot = p1, dpi = 300, height = 4, width = 9, units = 'in')

# method 2
# this method use fotmob api to obtain json format info, more data can be use
# momentum data is not interpolated, interval is 1 min

plt_mt <- function(url){
  mid <- substr(url, nchar(url) - 7 + 1, nchar(url))
  url_json <- paste0('https://www.fotmob.com/api/matchDetails?matchId=', mid)
  
  match_detail <- url_json %>% 
    request() %>% 
    req_perform() %>% 
    resp_body_json(simplifyVector = T)
  
  team <- match_detail$header$teams$name
  league <- match_detail$general$leagueName
  rd <- paste('Match Day', match_detail$general$matchRound)
  dt <- substr(match_detail$general$matchTimeUTCDate, 1, 10)
  
  ttl <- paste('Match Momentum for', team[1], 'vs', team[2])
  sttl <- paste0(league,', ', rd, ', ' , dt)
  
  mt <- match_detail$content$momentum$main$data
  y_lim <- range(mt$value) * 1.05
  
  
  if(team[1] == 'Liverpool')
    p <- ggplot(mt, aes(x = minute, y = value)) +
    geom_ribbon(aes(ymax = pmax(value, 0), ymin = 0), fill = 'red4', alpha = 0.65) +
    geom_ribbon(aes(ymax = 0, ymin = pmin(0, value)), fill = 'blue4', alpha = 0.65)
  else
    p <- ggplot(mt, aes(x = minute, y = value)) +
    geom_ribbon(aes(ymax = pmax(value, 0), ymin = 0), fill = 'blue4', alpha = 0.65) +
    geom_ribbon(aes(ymax = 0, ymin = pmin(0, value)), fill = 'red4', alpha = 0.65)
  
  p <- p + ylim(y_lim) + 
    labs(x = "Minutes", y = "Momentum", title = ttl, subtitle = sttl) +
    theme(legend.title = element_blank(),  axis.ticks.y=element_blank()) + 
    coord_fixed(ratio = 1/3) + theme_minimal()
  
  return(p)
}


url <- 'https://www.fotmob.com/matches/milan-vs-liverpool/2g7yzv#4621552'
url <- 'https://www.fotmob.com/matches/liverpool-vs-brentford/2uusjv#4506278'
url <- 'https://www.fotmob.com/matches/liverpool-vs-manchester-united/2ygkcb#4506289'

p2 <- plt_mt(url)
ggsave(filename = 'milan_m2.jpg', plot = p2, dpi = 300, height = 4, width = 9, units = 'in')

# method 3, highcharter solution
# by far the best solution

plt_hcmt <- function(url){
  
  mid <- substr(url, nchar(url) - 7 + 1, nchar(url))
  url_json <- paste0('https://www.fotmob.com/api/matchDetails?matchId=', mid)
  
  match_detail <- url_json %>% 
    request() %>% 
    req_perform() %>% 
    resp_body_json(simplifyVector = T)
  
  match <- match_detail$content$momentum$main$data
  
  team <- match_detail$header$teams$name
  league <- match_detail$general$leagueName
  rd <- paste('Match Day', match_detail$general$matchRound)
  dt <- substr(match_detail$general$matchTimeUTCDate, 1, 10)
  
  ttl <- paste('Match Momentum for', team[1], 'vs', team[2])
  sttl <- paste0(league,', ', rd, ', ' , dt)
  
  if(team[1] == 'Liverpool'){
  plt <- match %>% 
    hchart(type = "areaspline", hcaes(x = minute, y = value)) %>% 
    hc_plotOptions(areaspline = list(
      zones = list(
        list(value = 0, color = "blue4", fillColor = "rgba(0, 76, 153, 0.65)"),  # Negative values
        list(color = "red4", fillColor = "rgba(180, 0, 0, 0.65)")           # Positive values
      )
    ))
  } else {
  plt <-   match %>% 
      hchart(type = "areaspline", hcaes(x = minute, y = value)) %>% 
      hc_plotOptions(areaspline = list(
        zones = list(
          list(value = 0, color = "red4", fillColor = "rgba(180, 0, 0, 0.65)"),  # Negative values
          list(color = "blue4", fillColor = "rgba(0, 76, 153, 0.65)")           # Positive values
        )
      ))
  }
  
  plt %>% hc_tooltip(enabled = FALSE) %>%
    hc_title(text = ttl) %>% 
    hc_subtitle(text = sttl) %>%
    hc_yAxis(visible = FALSE) %>%
    hc_xAxis(
      tickPositions = seq(15, 90, by = 15),
      labels = list(format = "{value}")
    ) %>%
    hc_chart(backgroundColor = "white") %>% 
    #         , width = 960, height = 600)  %>% 
    hc_exporting(enabled = TRUE, # type = "image/jpeg"
    type = "image/svg+xml"
    )
  
  # Save the plot as an HTML file
  # html_file <- "temp_plot.html"
  # saveWidget(plt, file = html_file, selfcontained = TRUE)
  
  # Automatically save the plot as a JPG
  # jpg_file <- "exported_plot.jpg"
  # webshot(html_file, file = jpg_file, delay = 3, vwidth = 800, vheight = 600)
  
  # Optionally, remove the temporary HTML file
  # file.remove(html_file)
}

plt_hcmt(url)

# some helper function
get_mt <- function(url){
  mid <- substr(url, nchar(url) - 7 + 1, nchar(url))
  url_json <- paste0('https://www.fotmob.com/api/matchDetails?matchId=', mid)
  
  match_detail <- url_json %>% 
    request() %>% 
    req_perform() %>% 
    resp_body_json(simplifyVector = T)
  
  momentum <- match_detail$content$momentum$main$data
  
  return(momentum)
}

mu <- get_mt(url)

get_md <- function(url){
  mid <- substr(url, nchar(url) - 7 + 1, nchar(url))
  url_json <- paste0('https://www.fotmob.com/api/matchDetails?matchId=', mid)
  
  match_detail <- url_json %>% 
    request() %>% 
    req_perform() %>% 
    resp_body_json(simplifyVector = T)
  
  return(match_detail)
}
