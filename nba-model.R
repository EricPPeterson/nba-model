library(nbastatR)
library(tidyr)
library(dplyr)
library(tidybayes)
library(lubridate)

Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)
game_data <- game_logs(seasons = c(2020:2022), season_types = 'Regular Season', result_types = c('team', 'player'))
teams_data <- teams_tables(teams = c('Boston Celtics'), seasons = 2022, tables = c('splits', 'shooting'),
                           measures = c('Base'), modes = c('Per48'))