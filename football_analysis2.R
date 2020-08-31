#' ---
#' title: "Scrap FC Zurich Spectator Data"
#' author: "Lucas Jamar"
#' ---

#' setup, include=FALSE
knitr::opts_chunk$set(eval = FALSE)

#' Load libraries
library(ggplot2)
library(esquisse)
library(data.table)

dt <- fread("raw_data/fc_basel.csv", encoding = "UTF-8")

dt[, most_common_stadium := names(sort(-table(stadium))[1]), by = home_team]
dt[stadium == "", stadium := most_common_stadium]

stadiums <- data.table::data.table(stadiums = unique(dt$stadium))
#' Feature engineering
dt[home_team == "FC BASEL 1893", `:=` (opposing_club = away_team,
                                goals = home_goals,
                                opposing_goals = away_goals)]
dt[away_team == "FC BASEL 1893", `:=` (opposing_club = home_team,
                                goals = away_goals,
                                opposing_goals = home_goals)]

dt[, goal_difference := goals - opposing_goals]
dt[, time_of_day := hour(match_start)*60 + minute(match_start)]
dt[, day_of_week := lubridate::wday(date, label = TRUE)]
dt[, week_of_year := week(date)]
dt[, year := year(date)]

hour_of_day <- dt[, .(mean = mean(spectators, na.rm = TRUE),
                      count = .N),
                  by = hour_of_day]
day_of_week <- dt[, .(mean = mean(spectators, na.rm = TRUE),
                      count = .N),
                  by = day_of_week]

opposing_team <- dt[, .(
  mean = mean(spectators, na.rm = TRUE),
  count = .N,
  mean_goal_difference = mean(goal_difference, na.rm = TRUE),
  mean_goals = mean(goals, na.rm = TRUE),
  mean_opposing_goals = mean(opposing_goals, na.rm = TRUE)
),
by = opposing_club]

league <- dt[, .(mean = mean(spectators, na.rm = TRUE),
                 count = .N),
             by = league]

ggplot(dt) +
  aes(x = as.character(hour_of_day), y = spectators) +
  geom_boxplot()

ggplot(dt) +
  aes(x = day_of_week, y = spectators) +
  geom_boxplot()

ggplot(dt) +
  aes(x = reorder(league, league), y = spectators) +
  geom_boxplot() +
  coord_flip()

ggplot(dt) +
  aes(x = reorder(round, round), y = spectators) +
  geom_boxplot() +
  coord_flip()

ggplot(dt) +
  aes(x = spectators, fill = league) +
  geom_density()

ggplot(dt) +
  aes(x = time_of_day, y = spectators) +
  geom_point()

ggplot(dt) +
  aes(x = as.character(time_of_day), y = spectators) +
  geom_boxplot()

ggplot(dt) +
  aes(x = days_since_season_start, y = spectators) +
  geom_point()

ggplot(dt) +
  aes(x = days_till_season_end, y = spectators) +
  geom_point()

setorder(opposing_team, -mean)
ggplot(opposing_team[1:40]) +
  aes(x = reorder(opposing_club, mean), y = mean) +
  geom_col(fill = "blue") +
  facet_wrap(vars(club)) +
  coord_flip()

setorder(opposing_team, -mean_goal_difference)
ggplot(opposing_team[1:40]) +
  aes(x = reorder(opposing_club, mean_goal_difference), y = mean_goal_difference) +
  geom_col(fill = "blue") +
  facet_wrap(vars(club)) +
  coord_flip()
