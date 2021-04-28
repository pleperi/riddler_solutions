library(tidyverse)
theme_set(theme_bw())

trials <- 10000
votes <- 10000
pct_in_person <- .8
votes_in_person <- votes * pct_in_person
result_df <- data.frame(in_person_results = rep(0, trials), overall_results = rep(0, trials))
for (i in 1:trials) {
  vote_list <- round(runif(votes), 0)
  result_df[i, 1] <- sum(vote_list[1:votes_in_person])
  result_df[i, 2] <- sum(vote_list)
}

result_df %>%
  mutate(in_person_results = in_person_results / votes_in_person,
         overall_results = overall_results / votes) %>%
  ggplot(aes(in_person_results, overall_results)) +
  geom_point(aes(color = ((in_person_results > .5) == (overall_results > .5)))) +
  scale_color_discrete(name = "Come-From-Behind Win?", labels = c("Yes", "No")) +
  labs(x = "Election Day Votes for Candidate A",
       y = "Total Votes for Candidate A") +
  scale_x_continuous(labels = scales::percent) + 
  scale_y_continuous(labels = scales::percent)

calculate_pct_win <- function(trials, votes, pct_in_person) {
  votes_in_person <- votes * pct_in_person
  result_df <- data.frame(in_person_results = rep(0, trials), overall_results = rep(0, trials))
  for (i in 1:trials) {
    vote_list <- round(runif(votes), 0)
    result_df[i, 1] <- sum(vote_list[1:votes_in_person])
    result_df[i, 2] <- sum(vote_list)
  }
  same_winner <- result_df %>%
    mutate(candidate_initial_win = ifelse(in_person_results > votes_in_person/2, TRUE, FALSE),
           candidate_final_win = ifelse(overall_results > votes/2, TRUE, FALSE)) %>%
    filter(candidate_initial_win == candidate_final_win) %>%
    nrow()
  return (same_winner/trials)
}

vary_turnout_pct <- data.frame(pct_in_person = seq(.01, .99, .01), pct_same_winner = rep(0, 99))
for (i in seq(.01, .99, .01)) {
  vary_turnout_pct[i * 100, 2] <- calculate_pct_win(10000, 10000, i)
}

vary_turnout_pct %>%
  ggplot(aes(pct_in_person, 1 - pct_same_winner)) +
  geom_line(size = 2, color = "lightcyan") +
  geom_point(color = "darkcyan") +
  labs(x = "Percentage Voted In Person",
       y = "Chance of Come-From-Behind Win") +
  scale_x_continuous(labels = scales::percent) + 
  scale_y_continuous(labels = scales::percent)
