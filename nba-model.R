####################################################################################################################
#NBA 2023
####################################################################################################################
library(hoopR)
library(tidyr)
library(dplyr)
library(lubridate)
library(lookup)
library(pracma)
####################################################################################################################
#download pbp and schedule
############################################################6########################################################
pbp_func <- function(yr1,yr2){
  out <- load_nba_pbp(yr1:yr2) %>%
    progressr::with_progress()
  return(out)
}

nba_pbp <- pbp_func(2017,2021)

clean_schedule <- function(yr1,yr2){
  schedule <- load_nba_schedule(yr1:yr2) %>%
    dplyr::filter(status_type_description != 'Postponed') %>%
    dplyr::filter(type_id == 1) %>%
    dplyr::select(-c(recent, uid, date, start_date, home_uid, home_color, home_alternate_color, 
                     home_logo, away_color, away_alternate_color, away_logo, PBP, team_box, player_box, 
                     game_date_time
    ))
  schedule$game_date <- as.Date(schedule$game_date)
  return(schedule)
}

schedule <- clean_schedule(2017,2021)
####################################################################################################################
#manipulate schedule for rest days and travel
####################################################################################################################
sched_func <- function(yr, df, df2){
  sched_year <- df %>% dplyr::filter(season == yr) 
  
  home_year <- sched_year %>% dplyr::select(game_id, home_abbreviation, home_display_name, game_date, home_venue_id, 
                                            venue_address_city, venue_address_state)
  away_year <- sched_year %>% dplyr::select(game_id, away_abbreviation, away_display_name, game_date, home_venue_id,
                                            venue_address_city, venue_address_state)

  out <- left_join(home_year, away_year, by = c('game_id', 'game_date', 'home_venue_id', 'venue_address_city', 
                                                'venue_address_state'))
  
  colnames(home_year)[2:3] <- c('team_id', 'team_name')
  colnames(away_year)[2:3] <- c('team_id', 'team_name')
  
  
  out2 <- bind_rows(home_year, away_year) %>%
    dplyr::arrange(team_id, game_date) %>%
    dplyr::mutate(days_rest = 0) %>%
    dplyr::mutate(travel = 0)
  out2$days_rest[1] = 0
  
  for(j in 2:nrow(out2)){
    if(out2$team_id[j] == out2$team_id[j-1]){
      out2$days_rest[j] <- out2$game_date[j] - out2$game_date[j-1]    
    } else {
      out2$days_rest[j] = 0
    }
  }
  
  for(j in 2:nrow(out2)){
    if(out2$venue_address_city[j] == out2$venue_address_city[j-1]){
      out2$travel[j] = 0
    } else {
      out2$travel[j] = 1
    }
  }
  out2$home_abbreviation <- lookup(out2$game_id, df2$game_id, df2$home_team_abbrev)
  out2$away_abbreviation <- lookup(out2$game_id, df2$game_id, df2$away_team_abbrev)
  
  out_home <- out2 %>%
    dplyr::filter(team_id == home_abbreviation) %>%
    dplyr::select(game_id, home_abbreviation, days_rest, travel)
  colnames(out_home)[3:4] <- c('home_rest', 'home_travel')
  out_away <- out2 %>%
    dplyr::filter(team_id == away_abbreviation) %>%
    dplyr::select(game_id, away_abbreviation, days_rest, travel)
  colnames(out_away)[3:4] <- c('away_rest', 'away_travel')
  
  out <- left_join(out, out_home, by = c('game_id', 'home_abbreviation'))
  out <- left_join(out, out_away, by = c('game_id', 'away_abbreviation'))
  return(out)
}

sched_2017 <- sched_func(2017, schedule, nba_pbp)
sched_2017$season <- 2017
sched_2018 <- sched_func(2018, schedule, nba_pbp)
sched_2018$season <- 2018
sched_2019 <- sched_func(2019, schedule, nba_pbp)
sched_2019$season <- 2019
sched_2020 <- sched_func(2020, schedule, nba_pbp)
sched_2020$season <- 2020
sched_2021 <- sched_func(2021, schedule, nba_pbp)
sched_2021$season <- 2021

total_sched <- bind_rows(sched_2017, sched_2018, sched_2019, sched_2020, sched_2021)
###################################################################################################################
#count possessions changes
###################################################################################################################
clean_possessions <- function(df){
  df$team_id <- as.numeric(df$team_id)
  df <- df[complete.cases(df$team_id),]
  return(df)  
}

pbp_clean <- clean_possessions(nba_pbp)

count_poss <- function(df){
  df <- df %>%
    group_by(game_id) %>%
    mutate(change = cumsum(c(0,as.numeric(diff(team_id))!=0))) %>%
    ungroup()
  return(df)
}
poss_df <- count_poss(pbp_clean)
###################################################################################################################
#max possessions by team / season
###################################################################################################################
max_poss <- function(df, df2){
  out <- df %>%
    dplyr::group_by(game_id, home_team_abbrev, away_team_abbrev) %>%
    dplyr::summarize(sum_poss = max(change)) %>%
    dplyr::mutate(team_poss = sum_poss / 2)
  out$venue_id <- lookup(out$game_id, df2$game_id, df2$home_venue_id)
  out$season <- lookup(out$game_id, df2$game_id, df2$season)
  out <- out %>% ungroup()
  
  out_home <- out %>%
    dplyr::select(game_id, season, home_team_abbrev, venue_id, team_poss)
  colnames(out_home)[3] <- 'team_id'
  out_away <- out %>%
    dplyr::select(game_id, season, away_team_abbrev, venue_id, team_poss)
  colnames(out_away)[3] <- 'team_id'
  out_complete <- bind_rows(out_home, out_away)
  all_star <- list('EAST','GIA','LEB','USA', 'WEST','WORLD')
  out_complete <- out_complete %>%
    dplyr::filter(!team_id %in% all_star) %>%
    dplyr::filter_all(~!is.na(.))
  return(out_complete)
}

poss_count_complete <- max_poss(poss_df, total_sched)

poss_wtavg <- function(df){
  out <- df %>%
    dplyr::arrange(season, game_id) %>%
    dplyr::group_by(team_id, season) %>%
    dplyr::mutate(mov_avg_poss = pracma::movavg(team_poss, n = 10, type = 'w'))
  return(out)
}
poss_count_wtavg <- poss_wtavg(poss_count_complete)
###################################################################################################################
#add home and poss count to sched df
###################################################################################################################
home_away_poss <- function(df,df2){
  home_poss_wtavg <- df %>%
    dplyr::select(game_id,team_id,mov_avg_poss)
  home_poss_wtavg <- home_poss_wtavg %>%
    dplyr::ungroup() %>%
    dplyr::select(-season)
  colnames(home_poss_wtavg)[2:3] <- c('home_abbreviation','home_wavg_poss')
  
  away_poss_wtavg <- df %>%
    dplyr::select(game_id,team_id,mov_avg_poss)
  away_poss_wtavg <- away_poss_wtavg %>%
    dplyr::ungroup() %>%
    dplyr::select(-season)
  colnames(away_poss_wtavg)[2:3] <- c('away_abbreviation','away_wavg_poss')
  
  df2 <- left_join(df2, home_poss_wtavg, by = c('game_id', 'home_abbreviation'))
  df2 <- left_join(df2, away_poss_wtavg, by = c('game_id', 'away_abbreviation'))
  df2 <- df2[complete.cases(df2),]
  
  return(df2)
}

total_sched <- home_away_poss(poss_count_wtavg, total_sched)
###################################################################################################################
#load player box scores
###################################################################################################################
tm_list <- list('DUR', 'EAST', 'GIA', 'LEB', 'STE', 'USA', 'WEST', 'WORLD')

player_box_func <- function(yr1,yr2,teams){
  player_box <- load_nba_team_box(yr1:yr2) %>%
    dplyr:: filter(season_type != 3) %>%
    dplyr::select(-c(game_date_time, team_uid, team_alternate_color, team_winner, 
                     opponent_team_uid, opponent_team_alternate_color, team_slug, team_color, team_logo,
                     opponent_team_slug, opponent_team_color, opponent_team_logo, team_display_name,
                     team_short_display_name, opponent_team_name,
                     opponent_team_short_display_name, team_location, team_name, opponent_team_location, 
                     opponent_team_display_name, team_id, season_type)) %>%
    dplyr::mutate(total_score = team_score + opponent_team_score) %>%
    dplyr::filter(!team_abbreviation %in% teams)
  box_home <- player_box %>% dplyr::filter(team_home_away == 'home') %>%
    dplyr::select(-c(team_home_away))
  colnames(box_home)[4:36] <- paste(colnames(box_home)[4:36],'home',sep="_")
  box_away <- player_box %>% dplyr::filter(team_home_away == 'away') %>%
    dplyr::select(-c(total_score, team_home_away))
  colnames(box_away)[4:36] <- paste(colnames(box_away)[4:36],'away',sep="_")
  
  box_long <- left_join(box_home, box_away, by = c('game_id', 'season', 'game_date'))
  box_total <- bind_rows(box_home, box_away)
  
  player_box <- player_box %>% dplyr::mutate_at(c(6:33,35:36), as.numeric)
  return(player_box)
}
player_box <- player_box_func(2017,2021,tm_list)

team_stats <- function(df, team_opp, wt, typ){
  out <- df %>%
    dplyr::arrange(season, game_id) %>%
    dplyr::group_by(season,!!sym(team_opp)) %>%
    dplyr::mutate(asst_mov = pracma::movavg(assists, n = wt, type = typ),
                  block_mov = pracma::movavg(blocks, n = wt, type = typ),
                  dreb_mov = pracma::movavg(defensive_rebounds, n = wt, type = typ),
                  fbreak_mov = pracma::movavg(fast_break_points, n = wt, type = typ),
                  fgm_mov = pracma::movavg(field_goals_made, n = wt, type = typ),
                  fga_mov = pracma::movavg(field_goals_attempted, n = wt, type = typ),
                  ftm_mov = pracma::movavg(free_throws_made, n = wt, type = typ),
                  fta_mov = pracma::movavg(free_throws_attempted, n = wt, type = typ),
                  oreb_mov = pracma::movavg(offensive_rebounds, n = wt, type = typ),
                  paint_mov = pracma::movavg(points_in_paint, n = wt, type = typ),
                  steals_mov = pracma::movavg(steals, n = wt, type = typ),
                  turn_mov = pracma::movavg(team_turnovers, n = wt, type = typ))
  return(out)
}

team_stats_team <- team_stats(player_box, 'team_abbreviation', 5, 'w')
team_stats_opposition <- team_stats(player_box, 'opponent_team_abbreviation', 5, 'w')
##################################################################################################################
#complete df by adding 
##################################################################################################################
team_stats_final_df <- function(df, team_opp){
  tm <- df %>% select(c(game_id, season,!!sym(team_opp)), team_home_away)
  rest <- df %>% select(c(37:48)) %>%
    dplyr::ungroup() 
  
  if(team_opp == 'team_abbreviation'){
    rest <- rest %>% dplyr::select(-c(season, team_abbreviation))
  } else {
    rest <- rest %>% dplyr::select(-c(season, opponent_team_abbreviation)) 
  }
  
  df <- bind_cols(tm, rest)
  team_stats_home <- df %>%
    dplyr::filter(team_home_away == 'home')
  colnames(team_stats_home)[5:16] <- paste(colnames(team_stats_home)[5:16],'home',sep = "_")
  colnames(team_stats_home)[3] <- 'home_abbreviation'
  team_stats_away <- df %>%
    dplyr::filter(team_home_away == 'away')
  colnames(team_stats_away)[5:16] <- paste(colnames(team_stats_away)[5:16],'away',sep = "_")
  colnames(team_stats_away)[3] <- 'away_abbreviation'
  
  if(team_opp == 'team_abbreviation'){
    colnames(team_stats_home)[5:16] <- paste(colnames(team_stats_home)[5:16],'off', sep = '_')
    colnames(team_stats_away)[5:16] <- paste(colnames(team_stats_away)[5:16], 'off', sep = '_')
  } else {
    colnames(team_stats_home)[5:16] <- paste(colnames(team_stats_home)[5:16], 'def', sep = '_')
    colnames(team_stats_away)[5:16] <- paste(colnames(team_stats_away)[5:16], 'def', sep = '_')
  }
  
  return(list(team_stats_home, team_stats_away))
}

off_df <- team_stats_final_df(team_stats_team, 'team_abbreviation')
def_df <- team_stats_final_df(team_stats_opposition, 'opponent_team_abbreviation')
##################################################################################################################
#combine all dfs
##################################################################################################################
combine_df <- function(lst, lst2){
  off_1 <- lst[[1]] %>% dplyr::select(-team_home_away)
  colnames(off_1)[3] <- 'team_abbreviation'
  off_2 <- lst[[2]] %>% dplyr::select(-team_home_away)
  colnames(off_2)[3] <- 'team_abbreviation'
  def_1 <- lst2[[1]] %>% dplyr::select(-team_home_away)
  colnames(def_1)[3] <- 'team_abbreviation'
  def_2 <- lst2[[2]] %>% dplyr::select(-team_home_away)
  colnames(def_2)[3] <- 'team_abbreviation'
  home <- left_join(off_1, def_2, by = c('game_id', 'season', 'team_abbreviation'))
  away <- left_join(off_2, def_1, by = c('game_id', 'season', 'team_abbreviation'))
  return(list(home,away))
}

combo_df <- combine_df(off_df, def_df)
##################################################################################################################
#game stats
##################################################################################################################
model_df <- function(lst, schedule){
  df1 <- lst[[1]]
  colnames(df1)[3] <- 'home_team'
  df2 <- lst[[2]]
  colnames(df2)[3] <- 'away_team'
  out <- full_join(df1,df2,by = c('game_id', 'season'))
  out <- left_join(schedule, out, by = c('game_id'))
  return(out)
}

final_sched <- model_df(combo_df, total_sched)
##################################################################################################################
#add total game points
##################################################################################################################
final_sched$total_score <- lookup(final_sched$game_id, player_box$game_id, player_box$total_score)
##################################################################################################################
#add possessions
##################################################################################################################
final_sched$wt_avg_poss <- lookup(final_sched$game_id, poss_count_wtavg$game_id, poss_count_wtavg$mov_avg_poss)
##################################################################################################################
#add league wide scoring
##################################################################################################################
lg_score <- function(df, wt, typ){
  df$week <- strftime(df$game_date, format = '%V')
  df <- df %>%
    dplyr::select(c(game_id, season, week, total_score)) %>%
    unique()
  out <- df %>%
    dplyr::group_by(season) %>%
    dplyr::mutate(wt_avg_league_score = pracma::movavg(total_score, n = wt, type = typ))
  return(out)
}

league_scoring <- lg_score(player_box, 10, 'w')
final_sched$league_scoring <- lookup(final_sched$game_id, league_scoring$game_id, league_scoring$wt_avg_league_score)
#####################################################################################################################
#add opening lines to model
#####################################################################################################################
library(jsonlite)
theODDS_base <- 'https://api.the-odds-api.com/v4/sports/'
theODDS_sport  <- 'basketball_nba/'
theODDS_key <- Sys.getenv('theODDS_key')
theODDS_region <- 'regions=au&'
theODDS_markets <- '&markets=spreads&oddsFormat=decimal&'
start_date <- as.Date('2022-10-17') 
theODDS_date <- paste0('date=',start_date,'T12:00:00Z')

#pull API data
odds_df <- data.frame()
y <- 1
while(start_date <= as.Date('2022-10-19')){
  theODDS_fullAPI <- paste0(theODDS_base, theODDS_sport, theODDS_key, theODDS_region, theODDS_markets,theODDS_date)
  pull_API <- fromJSON(theODDS_fullAPI)
  nba_odds <- pull_API$data
  odds_df <- rbind(odds_df, nba_odds)
  start_date <- start_date + 1
  theODDS_date <- paste0('date=',start_date,'T12:00:00Z')
  print(y)
  y <- y+1
}

totals_function <- function(df){
  final_df <- df %>% dplyr::select(commence_time,home_team,away_team)
  final_df$spread <- NA
  final_df$over_vig <- NA
  final_df$under_vig <- NA
  for(i in 1:nrow(df)){
    books <- df[[7]][[i]]
    if('TAB' %in% books$title){
      books <- books %>% dplyr::filter(title == 'TAB')
    } else {
      next
    }
    markets <- books[[4]][[1]] %>% dplyr::filter(key == 'spreads')
    odds <- markets[[3]][[1]]
    final_df$spread[i] <- odds$point[1]
    final_df$over_vig[i] <- odds$price[1]
    final_df$under_vig[i] <- odds$price[2]
    print(i)
  }
  return(final_df)
}

setwd("/Users/ericp/OneDrive/Documents/GitHub/nba-model")
odds_all <- totals_function(odds_df)
odds_2022 <- totals_function(odds_df)
odds_2022 <- odds_2022 %>%
  separate(commence_time,c('game_date', 'time'),'T')
odds_2022$game_date <- as.Date(odds_2022$game_date)
odds_all <- odds_all %>%
  separate(commence_time, c('game_date', 'time'), 'T') %>%
  dplyr::select(-time)
odds_all$game_date <- as.Date(odds_all$game_date)

write.csv(odds_all, 'odds_all.csv', row.names = FALSE)
write.csv(odds_2022, 'odds_2022.csv', row.names = FALSE)

odds_2022 <- read.csv("~/GitHub/nba-model/odds_2022.csv")
odds_all <- read.csv("~/GitHub/nba-model/odds_all.csv")
odds_2022 <- odds_2022 %>% dplyr::select(-time)
odds_all <- odds_all %>% dplyr::select(-time)
odds_all <- odds_all %>%
  mutate(lean = over_vig - under_vig) %>%
  select(-c(over_vig, under_vig))
odds_all$game_date <- as.Date(odds_all$game_date)
odds_2022 <- odds_2022 %>%
  mutate(lean = over_vig - under_vig) %>%
  select(-c(over_vig, under_vig))
odds_2022$game_date <- as.Date(odds_2022$game_date)
#####################################################################################################################
#combine odds with final_sched
#####################################################################################################################
colnames(odds_all)[2:3] <- c('home_display_name', 'away_display_name')
final_sched <- left_join(final_sched, odds_all, by = c('game_date', 'home_display_name', 'away_display_name'))
#####################################################################################################################
#see if adding opening spread helps
#####################################################################################################################
final_sched_spread <- final_sched[complete.cases(final_sched),]
final_sched <- final_sched %>% dplyr::select(-c(spread, lean))
#####################################################################################################################
#build first model
#####################################################################################################################
library(h2o)
h2o.init()

prep_df <- function(df, cols){
  df <- df %>% ungroup() %>%
    select(-all_of(cols))
  smp_size <- floor(0.80 * nrow(df))
  set.seed(5)
  train_test <- sample(seq_len(nrow(df)), size = smp_size)
  df_train <- df[train_test, ]
  df_test <- df[-train_test, ]
  return(list(df_train, df_test, df))
}

model_build <- prep_df(final_sched, c(1:9,14,17,18,43))
model_spread <- prep_df(final_sched_spread, c(1:9, 14,17,18,43))

ensemble_train_h2o <- as.h2o(model_build[[1]])
ensemble_test_h2o <- as.h2o(model_build[[2]])
ensemble_spread_train <- as.h2o(model_spread[[1]])
ensemble_spread_test <- as.h2o(model_spread[[2]])

#train GBM
gbm_model <- function(df, y, x, nfolds, seed){
  out <- h2o.gbm(x = x,
                 y = y,
                 training_frame = df,
                 nfolds = nfolds,
                 keep_cross_validation_predictions = TRUE,
                 seed = 5)
  return(out)
}
total_gbm <- gbm_model(ensemble_train_h2o,'total_score', 
                     setdiff(names(ensemble_train_h2o), 'total_score'), 5, 5)
total_gbm_spread <- gbm_model(ensemble_spread_train,'total_score', 
                              setdiff(names(ensemble_spread_train), 'total_score'), 5, 5)
#train RF
RF_model <- function(df, y, x, nfolds, seed){
  out <- h2o.randomForest(x = x,
                          y = y,
                          training_frame = df,
                          nfolds = nfolds,
                          keep_cross_validation_predictions = TRUE,
                          seed = 5)
  return(out)
}
total_RF <- gbm_model(ensemble_train_h2o, 'total_score', 
                    setdiff(names(ensemble_train_h2o), 'total_score'), 5, 5)
spread_RF <- gbm_model(ensemble_spread_train, 'total_score', 
                       setdiff(names(ensemble_spread_train), 'total_score'), 5, 5)
#train glm
lr_model <- function(df, y, x, nfolds, seed){
  out <- h2o.glm(x = x,
                 y = y,
                 training_frame = df,
                 nfolds = nfolds,
                 keep_cross_validation_predictions = TRUE,
                 seed = 5)
  return(out)
}
total_lr <- lr_model(ensemble_train_h2o, 'total_score', 
                   setdiff(names(ensemble_train_h2o), 'total_score'), 5, 5)
spread_lr <- lr_model(ensemble_spread_train, 'total_score', 
                     setdiff(names(ensemble_spread_train), 'total_score'), 5, 5)

#train neural net
nn_model <- function(df, y, x, nfolds, seed){
  out <- h2o.deeplearning(
    x = x,
    y = y,
    training_frame = df,
    nfolds = nfolds,
    keep_cross_validation_predictions = TRUE,
    seed = 5
  )
  return(out)  
}
total_nn <- nn_model(ensemble_train_h2o, 'total_score', 
                   setdiff(names(ensemble_train_h2o), 'total_score'), 5, 5)
spread_nn <- nn_model(ensemble_spread_train, 'total_score', 
                     setdiff(names(ensemble_spread_train), 'total_score'), 5, 5)


# Train a stacked random forest ensemble using the GBM, RF and LR above
ensemble_model <- function(mod, mod2, mod3, mod4, df, y, x){
  out <- h2o.stackedEnsemble(x = x, y = y,
                             metalearner_algorithm = 'glm',
                             training_frame = df,
                             base_models = list(mod, mod2, mod3, mod4))
  
  return(out)
}
##################################################################################################################
#ensemble test no spread
##################################################################################################################
total_ensemble_test <- ensemble_model(total_lr, total_RF, total_nn, total_gbm, ensemble_train_h2o, 
                                    'total_score', setdiff(names(ensemble_train_h2o), 'total_score'))
##################################################################################################################
#ensemble test with spread
##################################################################################################################
total_ensemble_spread <- ensemble_model(spread_lr, spread_RF, spread_nn, total_gbm_spread, ensemble_spread_train, 
                                        'total_score', setdiff(names(ensemble_spread_train), 'total_score'))
#################################################################################################################
#check performance
#################################################################################################################
mod_performance <- function(model, test_df){
  out <- h2o.performance(model, test_df)
  return(out)
}
###################################################################################################################
#performance no spread
###################################################################################################################
total_gbm_test <- mod_performance(total_gbm_spread, ensemble_spread_test)
total_rf_test <- mod_performance(spread_RF, ensemble_spread_test)
total_glm_test <- mod_performance(spread_lr, ensemble_spread_test)
total_nn_test <- mod_performance(spread_nn, ensemble_spread_test)
min(h2o.rmse(total_gbm_test), h2o.rmse(total_rf_test), h2o.rmse(total_glm_test), h2o.rmse(total_nn_test))
total_ensemble <- mod_performance(total_ensemble_test, ensemble_train_h2o)
h2o.rmse(total_ensemble)
###################################################################################################################
#performance with spread
###################################################################################################################
spread_gbm_test <- mod_performance(total_gbm, ensemble_test_h2o)
spread_rf_test <- mod_performance(total_RF, ensemble_test_h2o)
spread_glm_test <- mod_performance(total_lr, ensemble_test_h2o)
spread_nn_test <- mod_performance(total_nn, ensemble_test_h2o)
min(h2o.rmse(spread_gbm_test), h2o.rmse(spread_rf_test), h2o.rmse(spread_glm_test), h2o.rmse(spread_nn_test))
total_ensemble <- mod_performance(total_ensemble_spread, ensemble_spread_train)
h2o.rmse(total_ensemble)
###################################################################################################################
#pull 2022 season
###################################################################################################################
nba_pbp_2022 <- pbp_func(2023,2023)
schedule_2022 <- clean_schedule(2023,2023)
sched_2022 <- sched_func(2023, schedule_2022, nba_pbp_2022)
sched_2022$season <- 2023
pbp_clean_2022 <- clean_possessions(nba_pbp_2022)
poss_df_2022 <- count_poss(pbp_clean_2022)
poss_count_complete_2022 <- max_poss(poss_df_2022, sched_2022)
poss_count_wtavg_2022 <- poss_wtavg(poss_count_complete_2022)
sched_2022 <- home_away_poss(poss_count_wtavg_2022, sched_2022)
tm_list <- list('DUR', 'EAST', 'GIA', 'LEB', 'STE', 'USA', 'WEST', 'WORLD')
player_box_2022 <- player_box_func(2023,2023,tm_list)
team_stats_team_2022 <- team_stats(player_box_2022, 'team_abbreviation', 5, 'w')
team_stats_opposition_2022 <- team_stats(player_box_2022, 'opponent_team_abbreviation', 5, 'w')
off_df_2022 <- team_stats_final_df(team_stats_team_2022, 'team_abbreviation')
def_df_2022 <- team_stats_final_df(team_stats_opposition_2022, 'opponent_team_abbreviation')
combo_df_2022 <- combine_df(off_df_2022, def_df_2022)
final_sched_2022 <- model_df(combo_df_2022, sched_2022)
final_sched_2022$total_score <- lookup(final_sched_2022$game_id, player_box_2022$game_id, player_box_2022$total_score)
final_sched_2022$wt_avg_poss <- lookup(final_sched_2022$game_id, poss_count_wtavg_2022$game_id, poss_count_wtavg_2022$mov_avg_poss)
league_scoring_2022 <- lg_score(player_box_2022, 10, 'w')
final_sched_2022$league_scoring <- lookup(final_sched_2022$game_id, league_scoring_2022$game_id, league_scoring_2022$wt_avg_league_score)
colnames(odds_2022)[2:3] <- c('home_display_name', 'away_display_name')
library(lubridate)
final_sched_2022$game_date <- ymd(final_sched_2022$game_date)
odds_2022$game_date <- ymd(odds_2022$game_date)
##########################################################################start here###############################
#final_sched_2022 <- final_sched_2022 %>% unique()
#final_sched_2022 <- merge(final_sched_2022, odds_2022, by = c('game_date', 'home_display_name', 'away_display_name'))
#########################################################################################################################################
#make predictions using 2022 data
#########################################################################################################################################
library(h2o)
games_2022 <- final_sched_2022 %>% dplyr::select(game_id, total_score)
model_2022 <- prep_df(final_sched_2022, c(1:9, 14,17,18,43,68))
ensemble_2022_h2o <- as.h2o(model_2022[[3]])
####################################################################################################################
#no spread preds
####################################################################################################################
no_spread_preds <- h2o.predict(total_ensemble_test,ensemble_2022_h2o)
no_spread_preds <- as.data.frame(no_spread_preds)
colnames(no_spread_preds)[1] <- 'predictions_2022'
games_2022 <- bind_cols(games_2022, no_spread_preds)

library(Metrics)
rmse(games_2022$total_score, games_2022$predictions_2022)
####################################################################################################################
#pull totals data
####################################################################################################################
theODDS_base <- 'https://api.the-odds-api.com/v4/sports/'
theODDS_sport  <- 'basketball_nba/'
theODDS_key <- Sys.getenv('theODDS_key')
theODDS_region <- 'regions=us&'
theODDS_markets <- '&markets=totals&oddsFormat=decimal&'
start_date <- as.Date('2022-10-17') 
theODDS_date <- paste0('date=',start_date,'T22:00:00Z')

#pull API data
odds_df <- data.frame()
y <- 1
while(start_date <= as.Date('2023-04-20')){
  theODDS_fullAPI <- paste0(theODDS_base, theODDS_sport, theODDS_key, theODDS_region, theODDS_markets,theODDS_date)
  pull_API <- fromJSON(theODDS_fullAPI)
  nba_odds <- pull_API$data
  odds_df <- rbind(odds_df, nba_odds)
  start_date <- start_date + 1
  theODDS_date <- paste0('date=',start_date,'T12:00:00Z')
  print(y)
  y <- y+1
}

totals_function <- function(df, book, key_mkt){
  final_df <- df %>% dplyr::select(commence_time,home_team,away_team)
  final_df$spread <- NA
  final_df$over_vig <- NA
  final_df$under_vig <- NA
  for(i in 1:nrow(df)){
    books <- df[[7]][[i]]
    if(book %in% books$title){
      books <- books %>% dplyr::filter(title == book)
    } else {
      next
    }
    markets <- books[[4]][[1]] %>% dplyr::filter(key == key_mkt)
    odds <- markets[[3]][[1]]
    final_df$spread[i] <- odds$point[1]
    final_df$over_vig[i] <- odds$price[1]
    final_df$under_vig[i] <- odds$price[2]
    print(i)
  }
  return(final_df)
}

totals_2022 <- totals_function(odds_df,'BetOnline.ag','totals')
fanduel_2022 <- totals_2022
betonline_2022 <- totals_2022
write.csv(fanduel_2022, 'fanduel_2022.csv', row.names = FALSE)
write.csv(betonline_2022, 'betonline_2022.csv', row.names = FALSE)

fanduel_2022 <- totals_2022 %>%
  separate(commence_time,c('game_date', 'time'),'T')
fanduel_2022$game_date <- as.Date(fanduel_2022$game_date)
fanduel_2022 <- fanduel_2022 %>% select(-time)
games_2022$home_team <- lookup(games_2022$game_id, final_sched_2022$game_id, final_sched_2022$home_display_name)
games_2022$away_team <- lookup(games_2022$game_id, final_sched_2022$game_id, final_sched_2022$away_display_name)
games_2022$game_date <- lookup(games_2022$game_id, final_sched_2022$game_id, final_sched_2022$game_date)
games_2022 <- left_join(games_2022, totals_2022, by = c('home_team', 'away_team', 'game_date'))