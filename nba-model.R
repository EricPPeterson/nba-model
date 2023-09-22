####################################################################################################################
#NBA 2023
####################################################################################################################
library(hoopR)
library(tidyr)
library(dplyr)
library(tidybayes)
library(lubridate)

####################################################################################################################
#download pbp and schedule
####################################################################################################################
nba_pbp <- load_nba_pbp(2017:2021) %>%
  progressr::with_progress()
schedule <- load_nba_schedule(2017:2021) %>%
  dplyr::filter(status_type_description != 'Postponed') %>%
  dplyr::filter(type_id == 1) %>%
  dplyr::select(-c(recent, uid, date, start_date, home_uid, home_color, home_alternate_color, 
                   home_logo, away_color, away_alternate_color, away_logo, PBP, team_box, player_box, 
                   game_date_time
  ))
schedule$game_date <- as.Date(schedule$game_date)

####################################################################################################################
#manipulate schedule for rest days and travel
####################################################################################################################
sched_func <- function(yr, df, df2){
  sched_year <- df %>% dplyr::filter(season == yr) 
  
  home_year <- sched_year %>% dplyr::select(game_id, home_id, home_name, game_date, home_venue_id, 
                                            venue_address_city, venue_address_state)
  colnames(home_year)[2:3] <- c('team_id', 'team_name')
  away_year <- sched_year %>% dplyr::select(game_id, away_id, away_name, game_date, home_venue_id,
                                            venue_address_city, venue_address_state)
  colnames(away_year)[2:3] <- c('team_id', 'team_name')
  
  out <- bind_rows(home_year, away_year) %>%
    dplyr::arrange(team_id, game_date) %>%
    dplyr::mutate(days_rest = 0) %>%
    dplyr::mutate(travel = 0)
  out$days_rest[1] = 0
  
  for(j in 2:nrow(out)){
    if(out$team_id[j] == out$team_id[j-1]){
      out$days_rest[j] <- out$game_date[j] - out$game_date[j-1]    
    } else {
      out$days_rest[j] = 0
    }
  }
  
  for(j in 2:nrow(out)){
    if(out$venue_address_city[j] == out$venue_address_city[j-1]){
      out$travel[j] = 0
    } else {
      out$travel[j] = 1
    }
  }
  out$home_id <- lookup(out$game_id, df2$game_id, df2$home_team_id)
  out$away_id <- lookup(out$game_id, df2$game_id, df2$away_team_id)
  return(out)
}

sched_2017 <- sched_func(2017, schedule, nba_pbp)
sched_2018 <- sched_func(2018, schedule, nba_pbp)
sched_2019 <- sched_func(2019, schedule, nba_pbp)
sched_2020 <- sched_func(2020, schedule, nba_pbp)
sched_2021 <- sched_func(2021, schedule, nba_pbp)

###################################################################################################################
#count possessions changes
###################################################################################################################
nba_pbp$team_id <- as.numeric(nba_pbp$team_id)
nba_pbp <- nba_pbp[complete.cases(nba_pbp$team_id),]

nba_pbp <- nba_pbp %>%
  group_by(game_id) %>%
  mutate(change = cumsum(c(0,as.numeric(diff(team_id))!=0))) %>%
  ungroup()

poss_check <- nba_pbp %>%
  dplyr::select(game_id, change, team_id, type_id, type_text)

game_1 <- poss_check %>%
  dplyr::filter(game_id == 400954514)
###################################################################################################################
#max possessions by team / season
###################################################################################################################
poss_count_game <- nba_pbp %>%
  group_by(game_id) %>%
  summarize(sum_poss = max(change)) %>%
  mutate(team_poss = sum_poss / 2)