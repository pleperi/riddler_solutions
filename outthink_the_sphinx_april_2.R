library("tidyverse")
theme_set(theme_bw())

calculate_winnings <- function() {
  current_money <- 1
  answer_previous <- -1
  answer_consecutive <- 0
  # Include for debugging:
  #bets <- rep(0, 4)
  #answers <- rep(0, 4)
  #guesses <- rep(0, 4)
  #winnings <- rep(0, 4)
  
  for (i in 1:4) {
    current_bet <- if(answer_consecutive == 2) {
      current_money
    } else {
      runif(1) * current_money
    }
    #bets[i] <- current_bet
    
    answer <- if (answer_consecutive == 2) {
      if (answer_previous == 0) {
        1
      } else if (answer_previous == 1) {
        0
      }
    } else {
      round(runif(1), 0) 
    }
    #answers[i] <- answer
    
    guess <- if (answer_consecutive == 2) {
      if (answer_previous == 0) {
        1
      } else if (answer_previous == 1) {
        0
      }
    } else {
      round(runif(1), 0) 
    }
    #guesses[i] <- guess
    
    if (answer == guess) {
      current_money = current_money + current_bet
    } else {
      current_money = current_money - current_bet
    }
    #winnings[i] <- current_money
    
    if (answer == answer_previous) {
      answer_consecutive = answer_consecutive + 1
    } else {
      answer_consecutive = 1
    }
    
    answer_previous = answer
  }
  
  #Uncomment for debugging:
  #print(paste0("guess: ", round(guesses, 0)))
  #print(paste0("answer: ", round(answers, 0)))
  #print(paste0("bet: ", round(bets, 2)))
  #print(paste0("result: ", round(winnings, 2)))
  return (current_money)
}

trials <- 100000
result_array <- rep(0, trials)

for (i in 1: trials) {
  result_array[i] <- calculate_winnings()
}

result_df <- data.frame(winnings = result_array)
result_df %>%
  filter(winnings >= 1) %>%
  ggplot(aes(winnings)) + 
  geom_histogram(color = "darkcyan", fill = "lightcyan") +
  scale_x_continuous(trans = "log2", labels = scales::dollar) +
  labs(x = "Final Winnings", 
       y = "", 
       title = "Distribution of Winnings with Random Bets",
       subtitle = "Based on 100,000 Trials")

