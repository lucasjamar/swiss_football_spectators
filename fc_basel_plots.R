
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
  aes(x = spectators) +
  geom_histogram() +
  facet_wrap(vars(league))

setorder(opposing_team, -mean)
ggplot(opposing_team[1:40]) +
  aes(x = reorder(opposing_club, mean), y = mean) +
  geom_col(fill = "blue") +
  coord_flip()

setorder(opposing_team, -mean_goal_difference)
ggplot(opposing_team[1:40]) +
  aes(x = reorder(opposing_club, mean_goal_difference), y = mean_goal_difference) +
  geom_col(fill = "blue") +
  coord_flip()

ggplot(dt) +
  aes(x = capacity, y = spectators, color = league) +
  geom_point()
