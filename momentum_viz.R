# background about match momentum
# https://theanalyst.com/na/2021/11/what-is-match-momentum
# https://theanalyst.com/eu/2021/03/what-is-possession-value/
# use "Inspect" from the browser (Chrome/Edge) to obtain momentum data from fotmob

library(dplyr)
library(ggplot2)
library(ggformula)
library(tidyr)
library(patchwork)

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
