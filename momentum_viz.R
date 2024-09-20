# background about match momentum
# https://theanalyst.com/na/2021/11/what-is-match-momentum
# https://theanalyst.com/eu/2021/03/what-is-possession-value/
# use "Inspect" from the browser (Chrome/Edge) to obtain momentum data from fotmob

library(dplyr)
library(ggplot2)
library(ggformula)
library(tidyr)
library(patchwork)
library(httr2)

# method 1

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

# away game
p <- ggplot(match, aes(x = index/3, y = adj)) +
  geom_ribbon(aes(ymax = pmax(adj, 0), ymin = 0), fill = 'blue4', alpha = 0.5) +
  geom_ribbon(aes(ymax = 0, ymin = pmin(0, adj)), fill = 'red4', alpha = 0.5) +
  labs(x = "Minutes", y = "Momentum", title = "Match Momentum") +
  theme(legend.title = element_blank(),  axis.ticks.y=element_blank()) + 
  coord_fixed(ratio = 1/3) + theme_minimal()

# home game
p <- ggplot(match, aes(x = index/3, y = adj)) +
  geom_ribbon(aes(ymax = pmax(adj, 0), ymin = 0), fill = 'red4', alpha = 0.5) +
  geom_ribbon(aes(ymax = 0, ymin = pmin(0, adj)), fill = 'darkblue', alpha = 0.5) +
  labs(x = "Minutes", y = "Momentum", title = "Match Momentum vs Brentford") +
  theme(legend.title = element_blank(),  axis.ticks.y=element_blank()) + 
  coord_fixed(ratio = 1/3) + theme_minimal()

p

# p1/p2/p3

ggsave(filename = 'mm.svg', plot = p, dpi = 300, height = 3, width = 7, units = 'in')

# method 2

plt_mt <- function(url){
  mid <- substr(url, nchar(url) - 7 + 1, nchar(url))
  url_json <- paste0('https://www.fotmob.com/api/matchDetails?matchId=', mid)
  
  match <- sub(".*/matches/([^/]+)/.*", "\\1", url)
  team <- strsplit(match, "-vs-")[[1]]
  team <- gsub("-", " ", team)
  team <- sapply(team, tools::toTitleCase)
  
  match_detail <- url_json %>% 
    request() %>% 
    req_perform() %>% 
    resp_body_json(simplifyVector = T)
  
  dt <- substr(match_detail$general$matchTimeUTCDate, 1, 10)
  league <- match_detail$general$leagueName
  
  ttl <- paste('Match Momentum for', team[1], 'vs', team[2])
  sttl <- paste0(league, ', ', dt)
  
  momentum <- match_detail$content$momentum$main$data
  y_lim <- range(momentum$value) * 1.05
  
  
  if(team[1] == 'Liverpool')
    p <- ggplot(momentum, aes(x = minute, y = value)) +
    geom_ribbon(aes(ymax = pmax(value, 0), ymin = 0), fill = 'red4', alpha = 0.65) +
    geom_ribbon(aes(ymax = 0, ymin = pmin(0, value)), fill = 'blue4', alpha = 0.65)
  else
    p <- ggplot(momentum, aes(x = minute, y = value)) +
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

plt_mt(url)
