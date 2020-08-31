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

columns <- c(
  "spectators",
  "match_start",
  "link",
  "date",
  "home_team",
  "away_team",
  "home_goals",
  "away_goals",
  "home_goals_first_half",
  "away_goals_first_half",
  "league",
  "season"
)

fc_zurich <- fread("raw_data/fc_zurich.csv",
                   encoding = "UTF-8",
                   select = columns)
fc_zurich[, club := "Fc Zürich"]

bscyb <- fread("raw_data/bscyb.csv",
               encoding = "UTF-8",
               select = columns)
bscyb[, club := "Yb"]

dt <- rbindlist(list(fc_zurich, bscyb), fill = TRUE)
rm(fc_zurich, bscyb)

clean_team <- function(team){
  team <- team %>%
    stringr::str_replace_all("[[:punct:] ]+"," ") %>%
    stringr::str_squish() %>%
    stringr::str_to_title()
}

dt[, home_team := clean_team(home_team)]
dt[, away_team := clean_team(away_team)]
dt[home_team == "Breitenr Yb", home_team := "Breitenrain"]
dt[away_team == "Breitenr Yb", away_team := "Breitenrain"]

#' Filters
dt <- dt[spectators < 10 ^ 5]
dt[home_goals > 100, home_goals := NA]
dt[away_goals > 100, away_goals := NA]

#' Feature engineering
dt[home_team == club, `:=` (opposing_club = away_team,
                                goals = home_goals,
                                opposing_goals = away_goals)]
dt[away_team == club, `:=` (opposing_club = home_team,
                                goals = away_goals,
                                opposing_goals = home_goals)]

for(row in 1:nrow(dt)){
  row_club <- dt[row, opposing_club]
  dt[opposing_club %like% row_club, opposing_club := row_club]
}

dt[, goal_difference := goals - opposing_goals]
dt[, hour_of_day := hour(match_start)]
dt[, day_of_week := lubridate::wday(date, label = TRUE)]
dt[, week_of_year := week(date)]
dt[, year := week(date)]
dt[, start_of_season := min(date), by = .(season, club)]
dt[, end_of_season := max(date), by = .(season, club)]
dt[, days_since_season_start := date - start_of_season]
dt[, days_till_season_end := end_of_season - date]

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
by = .(opposing_club, club)]
league <- dt[, .(mean = mean(spectators, na.rm = TRUE),
                 count = .N),
             by = league]

ggplot(dt) +
  aes(x = day_of_week, y = spectators, fill = club) +
  geom_boxplot()

ggplot(dt) +
  aes(x = reorder(league, league), y = spectators, fill = club) +
  geom_boxplot() +
  coord_flip()

ggplot(dt) +
  aes(x = days_since_season_start, y = spectators, color = club) +
  geom_point()

ggplot(dt) +
  aes(x = days_till_season_end, y = spectators, color = club) +
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
