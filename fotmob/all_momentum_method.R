# background about match momentum
# https://theanalyst.com/na/2021/11/what-is-match-momentum
# https://theanalyst.com/eu/2021/03/what-is-possession-value/
# use "Inspect" from the browser (Chrome/Edge) to obtain momentum data from fotmob
# for alternative method, we can use json to download information from fotmob, see
# https://github.com/Rodolsky/Match_Momentum_Scraping/blob/main/Match_Momentum_FotMob.ipynb

library(ggplot2)
library(patchwork)
library(highcharter)
# getter can sourse fotmob_getter.r
source('fotmob_getter.r')

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

bournemouth <- match
save(bournemouth, file = 'md05_h_bournemouth.rda')

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
      geom_ribbon(aes(ymax = pmax(adj, 0), ymin = 0), fill = '#B40000', alpha = 0.5) +
      geom_ribbon(aes(ymax = 0, ymin = pmin(0, adj)), fill = '#004C99', alpha = 0.5) +
      ggtitle(ttl)
  } else {
    # away game
    ttl <- paste('Match Momentum for', rival, 'vs Liverpool')
    p <- ggplot(match, aes(x = index/3, y = adj)) +
      geom_ribbon(aes(ymax = pmax(adj, 0), ymin = 0), fill = '#004C99', alpha = 0.5) +
      geom_ribbon(aes(ymax = 0, ymin = pmin(0, adj)), fill = '#B40000', alpha = 0.5) +
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
# raw momentum data is not interpolated, interval is 1 min

plt_mt <- function(mid_url){
  
  match_detail <- get_match(mid_url)
  
  team <- match_detail$header$teams$name
  league <- match_detail$general$leagueName
  season <- match_detail$general$parentLeagueSeason
  rd <- paste('Match Day', match_detail$general$matchRound)
  dt <- substr(match_detail$general$matchTimeUTCDate, 1, 10)
  
  ttl <- paste('Match Momentum for', team[1], 'vs', team[2])
  sttl <- paste0(season, ' ', league,', ', rd, ', ' , dt)
  
  mt <- match_detail$content$momentum$main$data

  if(team[1] == 'Liverpool')
    p <- ggplot(mt, aes(x = minute, y = value)) +
    geom_ribbon(aes(ymax = pmax(value, 0), ymin = 0), fill = '#B40000', alpha = 0.65) +
    geom_ribbon(aes(ymax = 0, ymin = pmin(0, value)), fill = '#004C99', alpha = 0.65)
  else
    p <- ggplot(mt, aes(x = minute, y = value)) +
    geom_ribbon(aes(ymax = pmax(value, 0), ymin = 0), fill = '#004C99', alpha = 0.65) +
    geom_ribbon(aes(ymax = 0, ymin = pmin(0, value)), fill = '#B40000', alpha = 0.65)
  
  p <- p + 
    ggtitle(ttl, subtitle = sttl) + xlab('Minutes') + 
    scale_x_continuous(breaks = seq(0, 90, by = 15)) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),   # Center title
      plot.subtitle = element_text(hjust = 0.5),# Center subtitle
      
      # Hide y-axis line and ticks
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      
      axis.line.x = element_line(color = "black"),  # Display x-axis line
      axis.ticks.x = element_line(color = "black"), # Display x-axis ticks
      
      # Remove background and gridlines
      # panel.background = element_blank(),       # Remove panel background
      # panel.grid.major = element_blank(),       # Remove major gridlines
      # panel.grid.minor = element_blank(),       # Remove minor gridlines
      
      # Optional: You can also hide the y-axis title if desired
      axis.title.y = element_blank()
    )
  
  return(p)
}

# smooth by cubic spline, with 6x new points density
plot_momentum <- function(mid_url){
  
  match_detail <- get_match(mid_url)
  
  team <- match_detail$header$teams$name
  season <- match_detail$general$parentLeagueSeason
  league <- match_detail$general$leagueName
  rd <- paste('Match Day', match_detail$general$matchRound)
  dt <- substr(match_detail$general$matchTimeUTCDate, 1, 10)
  
  ttl <- paste('Match Momentum for', team[1], 'vs', team[2])
  sttl <- paste0(season, ' ', league,', ', rd, ', ' , dt)
  
  mt <- match_detail$content$momentum$main$data
  fit <- splines::interpSpline(mt$minute, mt$value)
  # number of points can set if the curve is not that smooth
  x <- seq(min(mt$minute), max(mt$minute), length.out = nrow(mt) * 6)
  mt2 <- as.data.frame(predict(fit, x))
  colnames(mt2) <- c('minute', 'value')
  mt2 <- mt2 %>% mutate(value = pmin(pmax(value, -100), 100))
  
  if(team[1] == 'Liverpool')
    p <- ggplot(mt2, aes(x = minute, y = value)) +
    geom_ribbon(aes(ymax = pmax(value, 0), ymin = 0), fill = '#B40000', alpha = 0.65) +
    geom_ribbon(aes(ymax = 0, ymin = pmin(0, value)), fill = '#004C99', alpha = 0.65)
  else
    p <- ggplot(mt2, aes(x = minute, y = value)) +
    geom_ribbon(aes(ymax = pmax(value, 0), ymin = 0), fill = '#004C99', alpha = 0.65) +
    geom_ribbon(aes(ymax = 0, ymin = pmin(0, value)), fill = '#B40000', alpha = 0.65)
  
  p <- p + 
    ggtitle(ttl, subtitle = sttl) + xlab('Minutes') + 
    scale_x_continuous(breaks = seq(0, 90, by = 15)) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),   # Center title
      plot.subtitle = element_text(hjust = 0.5),# Center subtitle
      
      # Hide y-axis line and ticks
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      
      axis.line.x = element_line(color = "black"),  # Display x-axis line
      axis.ticks.x = element_line(color = "black"), # Display x-axis ticks
      
      # Remove background and gridlines
      panel.background = element_blank(),       # Remove panel background
      panel.grid.major = element_blank(),       # Remove major gridlines
      panel.grid.minor = element_blank(),       # Remove minor gridlines
      
      # Optional: You can also hide the y-axis title if desired
      axis.title.y = element_blank()
    )
    
  
  return(p)
}

ggsave(filename = 'milan_m2.jpg', plot = p2, dpi = 300, height = 4, width = 9, units = 'in')

# method 3, highcharter solution
# by far the best solution

plt_momentum_hc <- function(mid_url, auto_export = FALSE){

  match_detail <- get_match(mid_url)
  
  match <- match_detail$content$momentum$main$data
  
  team <- match_detail$header$teams$name
  season <- match_detail$general$parentLeagueSeason
  league <- match_detail$general$leagueName
  rd <- paste('Match Day', match_detail$general$matchRound)
  dt <- substr(match_detail$general$matchTimeUTCDate, 1, 10)
  
  ttl <- paste('Match Momentum for', team[1], 'vs', team[2])
  sttl <- paste0(season, ' ', league,', ', rd, ', ' , dt)
  
  if(team[1] == 'Liverpool'){
  plt <- match %>% 
    hchart(type = "areaspline", hcaes(x = minute, y = value)) %>% 
    hc_plotOptions(areaspline = list(
      lineWidth = 0,
      zones = list(
        list(value = 0, color = "#004C99", fillColor = "rgba(0, 76, 153, 0.65)"),  # Negative values
        list(color = "#B40000", fillColor = "rgba(180, 0, 0, 0.65)")           # Positive values
      )
    ))
  } else {
  plt <-   match %>% 
      hchart(type = "areaspline", hcaes(x = minute, y = value)) %>% 
      hc_plotOptions(areaspline = list(
        lineWidth = 0,
        zones = list(
          list(value = 0, color = "#B40000", fillColor = "rgba(180, 0, 0, 0.65)"),  # Negative values
          list(color = "#004C99", fillColor = "rgba(0, 76, 153, 0.65)")           # Positive values
        )
      ))
  }
  
  plt <- plt %>% hc_tooltip(enabled = FALSE) %>%
    hc_title(text = ttl) %>% 
    hc_subtitle(text = sttl) %>%
    hc_yAxis(visible = FALSE) %>%
    hc_xAxis(
      tickPositions = seq(0, 90, by = 15),
      labels = list(format = "{value}")
    ) %>%
    hc_chart(backgroundColor = "white")
    #         , width = 960, height = 600)  %>% 
  
  if (auto_export == TRUE){
  # Save the plot as an HTML file
  html_file <- "temp_plot.html"
  htmlwidgets::saveWidget(plt, file = html_file, selfcontained = TRUE)
  
  # Automatically save the plot as a JPG
  jpg_file <- paste0(ttl, '.jpg')
  webshot::webshot(html_file, file = jpg_file, delay = 5, vwidth = 1280, vheight = 800)
  
  # Optionally, remove the temporary HTML file
  file.remove(html_file)
  } else {
   plt %>% hc_exporting(enabled = TRUE, # type = "image/jpeg"
                        type = "image/svg+xml"
   )
  }
    
}

plt_mt(4621552)
plt_smt(4621552)
plt_hcmt(4621552, F)
