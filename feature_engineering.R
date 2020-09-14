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

#'Find missing stadiums through most common stadium per home team
character_columns <- colnames(dt)[sapply(dt, is.character)]
dt[stadium == "", stadium := NA]
dt[!is.na(stadium), most_common_stadium := names(sort(-table(stadium))[1]), by = home_team]
dt[, most_common_stadium := max(most_common_stadium, na.rm = TRUE), by = home_team]
dt[is.na(stadium), stadium := most_common_stadium]
dt[, most_common_stadium := NULL]

# League cleaning
league <- data.table::data.table()
league$old <- c(
  "EUROPA LEAGUE",
  "UEFA-CUP",
  "CHAMPIONS LEAGUE",
  "MEISTERSCHAFT",
  "RAIFFEISEN SUPER LEAGUE",
  "AXPO SUPER LEAGUE",
  "HELVETIA SCHWEIZER CUP",
  "WÜRTH SCHWEIZER CUP",
  "SWISSCOM CUP",
  "CUP"
)

league$new <- c(
  "UEFA EUROPE LEAGUE",
  "UEFA EUROPE LEAGUE",
  "UEFA CHAMPIONS LEAGUE",
  "SUPER LEAGUE",
  "SUPER LEAGUE",
  "SUPER LEAGUE",
  "SCHWEIZER CUP",
  "SCHWEIZER CUP",
  "SCHWEIZER CUP",
  "SCHWEIZER CUP"
)

for(row in 1:nrow(league)){
  old <- league[row, old]
  new <- league[row, new]
  dt[league == old, league := new]
}

#' Rounds
round <- data.table::data.table()

round$old <- c(
  "1. QUALIFIKATIONSRUNDE",
  "1. RUNDE",
  "1/16-FINAL",
  "1/16-FINALE",
  "1/16-FINALS",
  "1/2-FINAL",
  "1/32-FINAL",
  "1/32-FINALE",
  "1/4-FINAL",
  "1/4-FINALS",
  "1/8-FINAL",
  "1/8-FINALS",
  "2. QUALIFIKATIONSRUNDE",
  "ACHTELFINAL",
  "FINAL",
  "FINALRUNDE",
  "HALBFINAL",
  "HALBFINALE",
  "PLAY-OFF",
  "PLAY-OFFS",
  "QUALIFIKATIONSRUNDE",
  "QUALIFIKATIONSRUNDE 2",
  "QUALIFIKATIONSRUNDE 3",
  "VIERTELFINAL",
  "VIERTELFINALE",
  "VORBEREITUNG"
)

round$new <- c(
  "1. QUALIFIKATIONSRUNDE",
  "1. QUALIFIKATIONSRUNDE",
  "1/16-FINALE",
  "1/16-FINALE",
  "1/16-FINALE",
  "1/2-FINALE",
  "1/32-FINALE",
  "1/32-FINALE",
  "1/4-FINALE",
  "1/4-FINALE",
  "1/8-FINALE",
  "1/8-FINALE",
  "2. QUALIFIKATIONSRUNDE",
  "1/8-FINALE",
  "1-FINALE",
  "1-FINALE",
  "1/2-FINALE",
  "1/2-FINALE",
  "PLAY-OFFS",
  "PLAY-OFFS",
  "1. QUALIFIKATIONSRUNDE",
  "2. QUALIFIKATIONSRUNDE",
  "3. QUALIFIKATIONSRUNDE",
  "1/4-FINALE",
  "1/4-FINALE",
  "VORBEREITUNG"
)

for(row in 1:nrow(round)){
  old <- round[row, old]
  new <- round[row, new]
  dt[round == old, round := new]
}

dt[, round2 := round]
dt[, round := NULL]
dt[round2 %like% "-FINALE", round_type := "FINALE"]
dt[round2 %like% "-FINALE", round := stringr::str_remove_all(round2, "1/")]
dt[round2 %like% "-FINALE", round := stringr::str_remove_all(round, "-FINALE")]

dt[round2 %like% ". SPIELTAG", round_type := "SPIELTAG"]
dt[round2 %like% ". SPIELTAG", round := stringr::str_remove_all(round2, ". SPIELTAG")]

dt[round2 %like% ". QUALIFIKATIONSRUNDE", round_type := "QUALIFIKATIONSRUNDE"]
dt[round2 %like% ". QUALIFIKATIONSRUNDE", round := stringr::str_remove_all(round2, ". QUALIFIKATIONSRUNDE")]

dt[round2 %like% "PLAY-OFFS|VORBEREITUNG", round_type := round2]

dt[, round := as.numeric(round)]
dt[, round2 := NULL]

# Find stadium capacity and coordinates
stadiums <- fread("raw_data/stadiums.csv", encoding = "UTF-8")
stadiums[, other_name := capacity]
stadiums[, capacity := stringr::str_remove_all(capacity,",")]
stadiums[capacity != other_name, other_name := NA]
stadiums[, capacity := as.integer(capacity)]
stadiums[capacity == other_name, other_name := NA]
stadiums[other_name == "", other_name := NA]


other_names <- stadiums[!is.na(other_name), other_name]
real_names <- stadiums[!is.na(other_name), stadium]

for(nth_name in 1:length(other_names)){
  old <- other_names[nth_name]
  new <- real_names[nth_name]
  dt[stadium == old, stadium := new]
}
dt[stadiums, on =.(stadium), capacity := capacity]
dt[, load_factor := spectators/capacity]

#' Feature engineering
dt[, alphabetically_first_team := pmin(home_team, away_team)]
dt[, alphabetically_second_team := pmax(home_team, away_team)]

dt[home_team == "FC BASEL 1893", opposing_team := away_team]
dt[away_team == "FC BASEL 1893", opposing_team := home_team]

dt[, hour_of_day := hour(match_start)]
dt[, time_of_day := hour(match_start)*60 + minute(match_start)]
dt[, day_of_week := wday(date)]
dt[, day_of_week_labeled := lubridate::wday(date, label = TRUE)]
dt[, week_of_year := week(date)]
dt[, year := year(date)]

fwrite(dt, "clean_data/fc_basel.csv")
