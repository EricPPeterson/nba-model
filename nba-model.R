####################################################################################################################
#NBA 2023
####################################################################################################################
library(hoopR)
library(tidyr)
library(dplyr)
library(tidybayes)
library(lubridate)

nba_pbp <- load_nba_pbp(2017:2021) %>%
  progressr::with_progress()
schedule <- load_nba_schedule(2017:2021)

#count possessions changes
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