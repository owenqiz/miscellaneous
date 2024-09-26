library(dplyr)
library(ggplot2)
library(ggsoccer)
source('fotmob_getter.r')

# plot shot map of match details, liverpool are always red, opponent are blue
# considering statsbomb style color

# for local test, we use list, for future plot, we can embeding get_match function with url or mid
# consider team_shot/player_shot in the future
# not sure what OWN GOAL looks like
# use helper function, the data strucutre should be more simpler, only match information and shot data

plot_team_shot <- function(match_detail, all_path = FALSE){
  
  team <- match_detail$header$teams$name
  league <- match_detail$general$leagueName
  rd <- paste('Match Day', match_detail$general$matchRound)
  dt <- substr(match_detail$general$matchTimeUTCDate, 1, 10)
  
  ttl <- paste('Shot Map for', team[1], 'vs', team[2])
  sttl <- paste0(league,', ', rd, ', ' , dt)
  
  home_id <- match_detail$general$homeTeam$id
  home_name <- match_detail$general$homeTeam$name
  lfc_color <- '#911712'
  opp_color <- '#4974a5'
  
  if(home_name == 'Liverpool'){
    home_color <- lfc_color
    away_color <- opp_color
  } else{
    home_color <- opp_color
    away_color <- lfc_color
  }
  
  # if home team is liverpool, assign red, else assign away team red, opponent use blue
  # for non-liverpool match, away team are red can be improve from the data in the future

  df_shots <- match_detail$content$shotmap$shots
  # home_goal, away_goal, home_shot, away_shot
  home_goal <- df_shots %>% filter(teamId == home_id & eventType == 'Goal')
  home_shot <- df_shots %>% filter(teamId == home_id & eventType != 'Goal')
  away_goal <- df_shots %>% filter(teamId != home_id & eventType == 'Goal')
  away_shot <- df_shots %>% filter(teamId != home_id & eventType != 'Goal')
  
  # use pitch_international, dimension is 105 x 68
  p <- ggplot() +  annotate_pitch(dimensions = pitch_international) + 
                             # , colour = "white", fill = "steelblue4") + 
    geom_segment(data = home_goal, aes(x = 105 - x, y= 68 - y, xend = goalCrossedZ, yend = 68 - goalCrossedY),  colour = home_color,
                 arrow = arrow(length = unit(0.15, "cm"), type = "closed")) +
    geom_segment(data = away_goal, aes(x = x, y = y, xend = 105 - goalCrossedZ, yend = goalCrossedY),  colour = away_color,
                 arrow = arrow(length = unit(0.15, "cm"), type = "closed")) +
    geom_point(data = home_goal, aes(x = 105 - x, y = 68 - y), size = 4, colour = home_color) + 
    geom_point(data = home_shot, aes(x = 105 - x, y = 68 - y), size = 2, shape = 1, stroke = 1.5, alpha = 0.8, colour = home_color) + 
    geom_point(data = away_goal, aes(x = x, y = y), size = 4, colour = away_color) + 
    geom_point(data = away_shot, aes(x = x, y = y), size = 2, shape = 1, stroke = 1.5, alpha = 0.8, colour = away_color) + 
    labs(title = ttl, subtitle = sttl, caption = "Source: Fotmob") + 
    theme(
      # plot.background = element_rect(fill='steelblue4', color='steelblue4'),
      # panel.background = element_rect(fill='steelblue4', color='steelblue4'),
      plot.title = element_text(hjust=0.5, vjust=0, size=14),
      plot.subtitle = element_text(hjust=0.5, vjust=0, size=8),
      plot.caption = element_text(hjust=0.5),
      # text = element_text(color='white'),
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank(),
      legend.position = 'none'
    ) + 
    theme_pitch()
  
  if(all_path == TRUE){
  p <- p + geom_segment(data = home_shot, aes(x = 105 - x, y= 68 - y, xend = goalCrossedZ, yend = 68 - goalCrossedY),  
                        colour = home_color, linetype = 2, alpha = 0.7,
                        arrow = arrow(length = unit(0.15, "cm"), type = "closed")) + 
    geom_segment(data = away_shot, aes(x = x, y = y, xend = 105 - goalCrossedZ, yend = goalCrossedY),  
                   colour = away_color, linetype = 2, alpha = 0.7,
                   arrow = arrow(length = unit(0.15, "cm"), type = "closed"))
  }
  return(p)
  
}

plot_single_team_shot <- function(match_detail, home = TRUE, all_path = FALSE){
  
  team <- match_detail$header$teams$name
  tname <- ifelse(home, team[1], team[2])
  league <- match_detail$general$leagueName
  rd <- paste('Match Day', match_detail$general$matchRound)
  dt <- substr(match_detail$general$matchTimeUTCDate, 1, 10)
  
  ttl <- paste(tname, 'all Shots in', team[1], 'vs', team[2])
  sttl <- paste0(league,', ', rd, ', ' , dt)
  
  home_id <- match_detail$general$homeTeam$id
  home_name <- match_detail$general$homeTeam$name
  
  display_color <- 'white'
  
  # if home team is liverpool, assign red, else assign away team red, opponent use blue
  # for non-liverpool match, away team are red can be improve from the data in the future
  
  df_shots <- match_detail$content$shotmap$shots
  # home_goal, away_goal, home_shot, away_shot
  
  # home_goal, away_goal, home_shot, away_shot
  if(home){
    goal_df <- df_shots %>% filter(teamId == home_id & eventType == 'Goal')
    shot_df <- df_shots %>% filter(teamId == home_id & eventType != 'Goal')
  } else{
    goal_df <- df_shots %>% filter(teamId != home_id & eventType == 'Goal')
    shot_df <- df_shots %>% filter(teamId != home_id & eventType != 'Goal')
  }
  
  p <- ggplot() +  annotate_pitch(dimensions = pitch_international 
     , colour = "white", fill = "steelblue4") + 
    geom_segment(data = goal_df, aes(x = x, y = y, xend = 105 - goalCrossedZ, yend = goalCrossedY),  colour = display_color,
                 arrow = arrow(length = unit(0.15, "cm"), type = "closed")) + 
    geom_point(data = goal_df, aes(x = x, y = y), size = 4, colour = display_color) + 
    geom_point(data = shot_df, aes(x = x, y = y), size = 2, shape = 1, stroke = 1.5, alpha = 0.8, colour = display_color) + 
    labs(title = ttl, subtitle = sttl, caption = "Source: Fotmob") + 
    theme(
      plot.background = element_rect(fill='steelblue4', color='steelblue4'),
      panel.background = element_rect(fill='steelblue4', color='steelblue4'),
      plot.title = element_text(hjust=0.5, vjust=0, size=14),
      plot.subtitle = element_text(hjust=0.5, vjust=0, size=8),
      plot.caption = element_text(hjust=0.5),
      text = element_text(color='white'),
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank()
    ) + coord_flip(xlim = c(52.5, 107.5)) + scale_y_reverse()
  
  if(all_path == TRUE){
    p <- p + geom_segment(data = shot_df, aes(x = x, y = y, xend = 105 - goalCrossedZ, yend = goalCrossedY),  
                          colour = display_color, linetype = 2, alpha = 0.7,
                          arrow = arrow(length = unit(0.15, "cm"), type = "closed"))
  }
  return(p)

}


match_detail <- get_match(4621552)
shot_teams(match_detail, all_path = T)
shot_team(match_detail, home = T, all_path = T)

# use get function combination to perform two types
# player shot data_set should provided
# 1. single match shots
# 2. season shots
plot_player_shot <- function(player_df, all_path = FALSE){
  
  pname <- unique(player_df$playerName)
  
  ttl <- paste(pname, 'all shots')
  # sttl <- paste0(league,', ', rd, ', ' , dt)
  
  goal_df <- player_df %>% filter(eventType == 'Goal')
  shot_df <- player_df %>% filter(eventType != 'Goal')

  display_color <- 'white'
  
  p <- ggplot() +  annotate_pitch(dimensions = pitch_international 
                                  , colour = "white", fill = "steelblue4") + 
    geom_segment(data = goal_df, aes(x = x, y = y, xend = 105 - goalCrossedZ, yend = goalCrossedY),  colour = display_color,
                 arrow = arrow(length = unit(0.15, "cm"), type = "closed")) + 
    geom_point(data = goal_df, aes(x = x, y = y), size = 4, colour = display_color) + 
    geom_point(data = shot_df, aes(x = x, y = y), size = 2, shape = 1, stroke = 1.5, alpha = 0.8, colour = display_color) + 
    labs(title = ttl, caption = "Source: Fotmob") + 
    theme(
      plot.background = element_rect(fill='steelblue4', color='steelblue4'),
      panel.background = element_rect(fill='steelblue4', color='steelblue4'),
      plot.title = element_text(hjust=0.5, vjust=0, size=14),
      plot.subtitle = element_text(hjust=0.5, vjust=0, size=8),
      plot.caption = element_text(hjust=0.5),
      text = element_text(color='white'),
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank()
    ) + coord_flip(xlim = c(52.5, 107.5)) + scale_y_reverse()
  
  if(all_path == TRUE){
    p <- p + geom_segment(data = shot_df, aes(x = x, y = y, xend = 105 - goalCrossedZ, yend = goalCrossedY),  
                          colour = display_color, linetype = 2, alpha = 0.7,
                          arrow = arrow(length = unit(0.15, "cm"), type = "closed"))
  }
  return(p)
}

player <- get_player_season_shot('https://www.fotmob.com/players/737066/erling-haaland')
plot_player_shot(player_df = player, all_path = T)

plot_team_shot_on_goal <- function(mid_url_lst, home = TRUE){
  
  match_detail <- get_match(mid_url_lst)
  st <- get_team_shot_table(match_detail)
  
  st2 <- st %>% select(playerName,lastName, eventType, min, teamId, playerName,
                isBlocked, isOnTarget) %>% 
    mutate(label = paste0(lastName,', ', min, 'min'))
  st2 <- tibble(st2, st %>% select(onGoalShot) %>% purrr::reduce(data.frame))
  
  team <- match_detail$header$teams$name
  tname <- ifelse(home, team[1], team[2])
  otname <- ifelse(home, team[2], team[1])
  league <- match_detail$general$leagueName
  season <- match_detail$general$parentLeagueSeason
  rd <- paste('Match Day', match_detail$general$matchRound)
  dt <- substr(match_detail$general$matchTimeUTCDate, 1, 10)
  
  ttl <- paste(tname, 'all shots on Goal vs', otname)
  sttl <- paste0(season, ' ', league,', ', rd, ', ' , dt)
  
  home_id <- match_detail$general$homeTeam$id
  home_name <- match_detail$general$homeTeam$name
  
  if(home){
    st <- st2 %>% filter(teamId == home_id & zoomRatio == 1) %>% filter(!isBlocked)
  } else {
    st <- st2 %>% filter(teamId != home_id & zoomRatio == 1) %>% filter(!isBlocked)
  }
  
  p <- ggplot(st, aes(x, y, label = label, colour = eventType)) + 
    geom_segment(aes(x = 0, y = 0, xend = 0, yend = .65), linewidth = 2, colour = 'black') + 
    geom_segment(aes(x = 2, y = 0, xend = 2, yend = .65), linewidth = 2, colour = 'black') + 
    geom_segment(aes(x = 0, y = .65, xend = 2, yend = .65), linewidth = 2, colour = 'black') + 
    geom_segment(aes(x = -.15, y = 0, xend = 2.15, yend = 0), linetype = 'dashed', colour = '#2E8B57') + 
    geom_point(size = 5) + geom_text(nudge_y = .05) + 
    scale_colour_manual(values = c('#004C99', '#B40000', '#004C99')) + 
    ggtitle(ttl, subtitle = sttl) + 
    coord_equal() + 
    theme_void() + 
    theme(plot.title = element_text(hjust = 0.5),   # Center title
          plot.subtitle = element_text(hjust = 0.5),# Center subtitle
          legend.position = 'none')
  return(p)
}

plot_team_shot_on_goal(mid_url_lst, home = F)

ft<- ot[[2]]
min_ratio <- min(ft$zoomRatio)
# wrong, should use 1 - x to find the position
ft2 <- ft %>% mutate(x = x/zoomRatio*min_ratio + 1, y = y/zoomRatio*min_ratio)
maxd <- max(abs(ft2$x-1))


ggplot(ft2, aes(x, y, label = lastName, colour = eventType)) + 
  geom_segment(aes(x = 1 - min_ratio, y = 0, xend = 1 - min_ratio, yend = .65 * min_ratio), linewidth = 2, colour = 'black') + 
  geom_segment(aes(x = 1 + min_ratio, y = 0, xend = 1 + min_ratio, yend = .65 * min_ratio), linewidth = 2, colour = 'black') + 
  geom_segment(aes(x = 1 - min_ratio, y = .65 * min_ratio, xend = 1 + min_ratio, yend = .65 * min_ratio), linewidth = 2, colour = 'black') + 
  geom_point(size = 5) + geom_text(nudge_y = .07 * min_ratio) + 
  scale_colour_manual(values = c('#004C99', '#B40000', '#004C99', '#004C99')) +
  xlim(1 - maxd, 1 + maxd) + 
  coord_equal() + 
  theme_minimal() + 
  theme(legend.position = 'none')

# plot_player_shot_goal