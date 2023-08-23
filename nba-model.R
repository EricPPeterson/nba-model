library(nbastatR)
library(tidyr)
library(dplyr)
library(tidybayes)
library(lubridate)

game_data <- game_logs(seasons = c(2020:2022), season_types = 'Regular Season', result_types = c('team', 'player'))