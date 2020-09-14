library(ggplot2)
library(data.table)

#' Read in data
dt <- fread("clean_data/fc_basel.csv", encoding = "UTF-8")
dt <- dt[!is.na(spectators)]

hour_of_day <- dt[, .(mean = mean(spectators, na.rm = TRUE),
                      count = .N),
                  by = hour_of_day]
day_of_week <- dt[, .(mean = mean(spectators, na.rm = TRUE),
                      count = .N),
                  by = day_of_week]

opposing_team <- dt[, .(
  mean_spectators = mean(spectators, na.rm = TRUE),
  mean_load_factor = mean(load_factor, na.rm = TRUE),
  count = .N
),
by = opposing_team]

stadium <- dt[, .(
  mean_spectators = mean(spectators, na.rm = TRUE),
  mean_load_factor = mean(load_factor, na.rm = TRUE),
  count = .N
),
by = stadium]

league <- dt[, .(mean = mean(spectators, na.rm = TRUE),
                 mean_load_factor = mean(load_factor, na.rm = TRUE),
                 count = .N),
             by = league]

ggplot(dt) +
  aes(x = hour_of_day, y = spectators) +
  geom_point()

ggplot(dt) +
  aes(x = day_of_week, y = spectators) +
  geom_boxplot()

ggplot(dt) +
  aes(x = reorder(league, league), y = spectators) +
  geom_boxplot() +
  coord_flip()

ggplot(dt) +
  aes(x = reorder(round_type, round_type), y = spectators) +
  geom_boxplot() +
  coord_flip()

ggplot(dt) +
  aes(x = reorder(league, league), y = load_factor) +
  geom_boxplot() +
  coord_flip()

ggplot(dt) +
  aes(x = reorder(round_type, round_type), y = load_factor) +
  geom_boxplot() +
  coord_flip()

ggplot(dt) +
  aes(x = spectators) +
  geom_histogram() +
  facet_wrap(vars(league))

setorder(opposing_team, -mean_spectators)
ggplot(opposing_team[1:40]) +
  aes(x = reorder(opposing_team, mean_spectators), y = mean_spectators) +
  geom_col(fill = "blue") +
  coord_flip()

setorder(stadium, -mean_spectators)
ggplot(stadium[1:40]) +
  aes(x = reorder(stadium, mean_spectators), y = mean_spectators) +
  geom_col(fill = "blue") +
  coord_flip()

setorder(opposing_team, -mean_load_factor)
ggplot(opposing_team[1:40]) +
  aes(x = reorder(opposing_team, mean_load_factor), y = mean_load_factor) +
  geom_col(fill = "blue") +
  coord_flip()

setorder(stadium, -mean_load_factor)
ggplot(stadium[1:40]) +
  aes(x = reorder(stadium, mean_load_factor), y = mean_load_factor) +
  geom_col(fill = "blue") +
  coord_flip()

ggplot(dt) +
  aes(x = capacity, y = spectators, color = league) +
  geom_point()
