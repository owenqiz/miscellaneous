# This is the helper functions for Fotmob data scraping
# Currently, the following data can be scraped using R
# Team information, determined by team_id = tid
# League information, determined by league_id = lid
# Match information by date
# Player information, determined by player_id = pid
# Match information, determined by match_id = mid

library(dplyr)
library(httr2)

# input either url or tid, return a list of team_detail
get_team <- function(tid_url){
  if (is.character(tid_url)){
   tid <- gsub("[^0-9]", "", tid_url)
   url_json <- paste0('https://www.fotmob.com/api/teams?id=', tid) 
  }
  else{
  # here the input is a number tid
   url_json <- paste0('https://www.fotmob.com/api/teams?id=', tid_url) 
  } 
  team_detail <- url_json %>% 
    request() %>% 
    req_perform() %>% 
    resp_body_json(simplifyVector = T)
  return(team_detail)
}

get_league <- function(lid_url){
  if (is.character(lid_url)){
    lid <- gsub("[^0-9]", "", lid_url)
    url_json <- paste0('https://www.fotmob.com/api/leagues?id=', lid) 
  }
  else{
    url_json <- paste0('https://www.fotmob.com/api/leagues?id=', lid_url) 
  } 
  league_detail <- url_json %>% 
    request() %>% 
    req_perform() %>% 
    resp_body_json(simplifyVector = T)
  return(league_detail)
}

# type = {1 = popular, 2 = international, 3 = by_country}
get_league_list <- function(type = 1){
  
  url <- 'https://www.fotmob.com/api/allLeagues'
  
  leagues <- url %>% 
    request() %>% 
    req_perform() %>% 
    resp_body_json(simplifyVector = T)
  
  if(type == 1){
    tbl <- leagues$popular
  } else if (type == 2){
    tbl <- leagues$international$leagues[[1]]
  } else {
    league_lst <- leagues$countries$leagues
    n_time <- sapply(league_lst, nrow)
    ccode <- leagues$countries$ccode
    cname <- leagues$countries$name
    league_tbl <- bind_rows(leagues$countries$leagues)
    
    tbl <- tibble(league_tbl, ccode = rep(ccode, n_time), cname = rep(cname, n_time))
  }
  return(tbl)
}

get_match_by_date <- function(date){
  url_json <- paste0('https://www.fotmob.com/api/matches?date=', date)
  date_match <- url_json %>% 
    request() %>% 
    req_perform() %>% 
    resp_body_json(simplifyVector = T)
  
  date_match <- bind_rows(date_match$leagues$matches)
  
  return(date_match)
}

get_player <- function(pid_url){
  if (is.character(pid_url)){
    pid <- gsub("[^0-9]", "", pid_url)
    url_json <- paste0('https://www.fotmob.com/api/playerData?id=', pid) 
  }
  else{
    url_json <- paste0('https://www.fotmob.com/api/playerData?id=', pid_url) 
  } 
  player_detail <- url_json %>% 
    request() %>% 
    req_perform() %>% 
    resp_body_json(simplifyVector = T)
  return(player_detail)
}

get_match <- function(mid_url){
  if (is.character(mid_url)){
    # to improve in the future
    mid <- substr(mid_url, nchar(mid_url) - 7 + 1, nchar(mid_url))
    url_json <- paste0('https://www.fotmob.com/api/matchDetails?matchId=', mid) 
  }
  else{
    url_json <- paste0('https://www.fotmob.com/api/matchDetails?matchId=', mid_url)
  }
  match_detail <- url_json %>% 
    request() %>% 
    req_perform() %>% 
    resp_body_json(simplifyVector = T)
  return(match_detail)
}

get_player_recent_match <- function(pid_url){
  
  player <- get_player(pid_url)
  
  player_match <- player$recentMatches %>% 
    select(id, teamId, teamName, opponentTeamId, opponentTeamName, 
           matchDate, 
           leagueId, leagueName, 
           minutesPlayed, goals, assists, yellowCards, redCards, ratingProps)
  
  # handle the ISO-8601 time format into lubridate
  player_match$matchDate <- lubridate::ymd_hms(player_match$matchDate$utcTime)
  # here not sure why using lubridate::ymd does not work for most date time
  datetime <- as.character(player_match$matchDate)
  player_match$date <- lubridate::ymd(substr(datetime, 1, 10))
  player_match <- player_match %>% relocate(date, .before = matchDate)

  return(player_match)
}

# TODO
# taking either match_id, url or downloaded json_lst return dataset of shotmap for a single match
get_team_shot_table <- function(mid_url_lst){

  # may contain match info such as date, league, team etc
  # if the plot shot function want to use this as getter
  if(is.list(mid_url_lst)){
    return(mid_url_lst$content$shotmap$shots)
  } else {
    match_detail <- get_match(mid_url_lst)
    return(match_detail$content$shotmap$shots)
  }
}

# get player shot dataset, combining with some match day info
# need player_id = pid and match_id = mid to find player shot in specific match
# player use id to filter across match_detail
get_player_match_shot <- function(pid_url, mid_url){
  
  if(!is.numeric(pid_url)){
    pid <- gsub("[^0-9]", "", pid_url)
  } else {
    pid <- as.character(pid_url)
  } 
  
  match_detail <- get_match(mid_url)
  
  shotmap <- match_detail$content$playerStats[[pid]]$shotmap
  
  return(shotmap)
}


get_player_season_shot <- function(pid_url){
  
  player <- get_player(pid_url)
  return(player$firstSeasonStats$shotmap)
  
}
