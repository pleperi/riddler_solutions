library("tidyverse")
library("Rfast")
theme_set(theme_bw())

### Solve the case with only two competitors
races <- 100000
set.seed(2021)
myFirstRun <- rnorm(races)
mySecondRun <- rnorm(races)
opponentFirstRun <- rnorm(races)
opponentSecondRun <- rnorm(races)

times <- as_tibble(cbind(myFirstRun, mySecondRun, opponentFirstRun, opponentSecondRun)) 

times %>%
  filter(myFirstRun < opponentFirstRun) %>%
  summarize(pct = mean(myFirstRun + mySecondRun < opponentFirstRun + opponentSecondRun))

### Plot the results, color-coded by winner of the first round
times %>%
  ggplot(aes(myFirstRun + mySecondRun, opponentFirstRun + opponentSecondRun)) +
  geom_point(aes(color = (myFirstRun > opponentFirstRun),), size = .5) +
  scale_color_discrete(name = "First Round Winner", labels = c("I won", "Opponent won")) +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  xlim(-6, 6) +
  ylim(-6, 6) +
  geom_abline(slope = 1, intercept = 0) +
  labs(x = "My combined time",
       y = "Opponent's combined time")

### A function for two competitors
# Choose the number of races to simulate
ski_federation <- function(races) {
  myFirstRun <- rnorm(races)
  mySecondRun <- rnorm(races)
  opponentFirstRun <- rnorm(races)
  opponentSecondRun <- rnorm(races)
  
  wins <- as_tibble(cbind(myFirstRun, mySecondRun, opponentFirstRun, opponentSecondRun)) %>%
    filter(myFirstRun < opponentFirstRun) %>%
    summarize(pct = mean(myFirstRun + mySecondRun < opponentFirstRun + opponentSecondRun))
  
  print(wins[1][1])
}

ski_federation(500000)

### Extra credit
ec_runs = 1000000
first_times <- matrix(rnorm(30 * ec_runs), ncol = 30)
second_times <- matrix(rnorm(30 * ec_runs), ncol = 30)
total_times <- first_times + second_times
min_first_time <- rowMins(first_times)
min_total_time <- rowMins(total_times)
print(mean(min_first_time == min_total_time))

### Wrap this in a function where you can dynamically choose the number of competitors
calculate_win_probability <- function(runs, total_competitors) {
  first_times <- matrix(rnorm(total_competitors * runs), ncol = total_competitors)
  second_times <- matrix(rnorm(total_competitors * runs), ncol = total_competitors)
  total_times <- first_times + second_times
  min_first_time <- rowMins(first_times)
  min_total_time <- rowMins(total_times)
  print(mean(min_first_time == min_total_time))
}
calculate_win_probability(1000000, 30)

### Loop through different numbers of competitors
get_all_probs <- function(loop_runs, min_competitors, max_competitors) {
  results <- as_tibble(cbind(min_competitors:max_competitors, 0))
  colnames(results) <- c("total_competitors", "win_probability")
  for (i in min_competitors:max_competitors) {
    first_times <- matrix(rnorm(i * loop_runs), ncol = i)
    second_times <- matrix(rnorm(i * loop_runs), ncol = i)
    total_times <- first_times + second_times
    min_first_time <- rowMins(first_times)
    min_total_time <- rowMins(total_times)
    results[i - min_competitors + 1, "win_probability"] <- mean(min_first_time == min_total_time)
  }
  return(results)
}

### Plot the result for any number of competitors between 2 and 30
result_range <- as_tibble(get_all_probs(100000, 2, 30))

result_range %>%
  ggplot(aes(total_competitors, win_probability)) +
  geom_line(color = "darkcyan", size = 2) +
  labs(x = "Total Competitors",
       y = "",
       title = "Probability of Having Fastest Total Time After Winning First Run") +
  scale_y_continuous(labels = scales::percent)