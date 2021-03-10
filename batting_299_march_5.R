# Riddler Classic for March 5, 2021
# Riddle was published on FiveThirtyEight 
# at https://fivethirtyeight.com/features/can-you-bat-299-in-299-games/

library("tidyverse")
theme_set(theme_bw())

max_games = function(games) {
  while (games > 0) {
    hits = round(4 * games^2 / 1000)
    ba = round(hits / (4 * games), 3)
    if (ba != games / 1000) {
      return(games)
    } else {
      games = games - 1
    }
  }
}
max_games(999)
#returns 239

# What about for different numbers of at-bats per game?
max_games_varied_atbats = function(games, atbats) {
  while (games > 0) {
    hits = round(atbats * games^2 / 1000)
    ba = round(hits / (atbats * games), 3)
    if (ba != games / 1000) {
      return(games)
    } else {
      games = games - 1
    }
  }
}
result_data_frame <- data.frame(at_bats = c(2:10), max_games = rep(0, 9))
for (i in 2:10) {
  result_data_frame[i-1, 2] = max_games_varied_atbats(999, i)
}

result_data_frame %>%
  ggplot(aes(at_bats, max_games)) +
  geom_line(color = "cyan", size = 2) +
  geom_point(color = "darkcyan", size = 5) +
  labs(x = "Average At-Bats Per Game",
       y = "",
       title = "Max Number of Games When BA Can Match Games Played")
