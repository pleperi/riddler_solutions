library(readr)
library(tidyverse)
library(stringr)
library(scales)
theme_set(theme_bw())

# Word list provided by FiveThirtyEight
word_list <- read_csv("https://norvig.com/ngrams/enable1.txt", 
                        col_names = FALSE)
names(word_list)[1] <- "word"

# Filter the word list
# The first column of the resulting dataset is the word
# Then there are five columns with one letter each
five_letter_words <- word_list %>%
  filter(str_length(word) == 5) %>%
  mutate(letters = .$word %>% map(function(x) str_split(x, ""))) %>%
  unnest(letters) %>%
  separate(letters, c("c", "letter1", "letter2", "letter3", "letter4", "letter5", "blank")) %>%
  select(word, letter1:letter5)

# Calculate letter frequencies by position
letter1 <- five_letter_words %>% 
  count(letter1) %>% 
  rename(letter = letter1, freq_letter1 = n)

letter2 <- five_letter_words %>% 
  count(letter2) %>% 
  rename(letter = letter2, freq_letter2 = n)

letter3 <- five_letter_words %>% 
  count(letter3) %>% 
  rename(letter = letter3, freq_letter3 = n)

letter4 <- five_letter_words %>% 
  count(letter4) %>% 
  rename(letter = letter4, freq_letter4 = n)

letter5 <- five_letter_words %>% 
  count(letter5) %>% 
  rename(letter = letter5, freq_letter5 = n)

letter_counts <- letter1 %>%
  left_join(letter2) %>%
  left_join(letter3) %>%
  left_join(letter4) %>%
  left_join(letter5) %>%
  replace(is.na(.), 0) %>%
  mutate(freq_overall = freq_letter1 + freq_letter2 + freq_letter3 + freq_letter4 + freq_letter5) %>%
  arrange(desc(freq_overall))

letter_counts %>% 
  pivot_longer(!letter, names_to = "character", values_to = "frequency") %>%
  filter(character != "freq_overall") %>%
  ggplot(aes(letter, frequency)) +
  geom_col(aes(fill = character)) + 
  facet_wrap(~character, scales = "free", 
             labeller = labeller(character =
                c("freq_letter1" = "First Letter",
                  "freq_letter2" = "Second Letter",
                  "freq_letter3" = "Third Letter",
                  "freq_letter4" = "Fourth Letter",
                  "freq_letter5" = "Fifth Letter"))) +
  labs(title="Frequency of Letters in 5-Letter Words", x="", y = "") +
  theme(legend.position="none")

# Cleaning step
rm(letter1, letter2, letter3, letter4, letter5)

# 25 most frequent letters/positions overall
letter_counts %>% 
  pivot_longer(!letter, names_to = "character", values_to = "frequency") %>% 
  filter(character != "freq_overall") %>% 
  arrange(desc(frequency)) %>% 
  slice(1:25) %>%
  count(letter)

# Top 5 letters by position
position_summary <- letter_counts %>% 
  pivot_longer(!letter, names_to = "character", values_to = "frequency") %>% 
  filter(character != "freq_overall") %>%
  group_by(character) %>%
  arrange(character, desc(frequency)) %>%
  mutate(rank = row_number()) %>%
  filter(rank <= 5)

# How many times does a letter appear multiple times per word?
double_letters <- as_tibble(cbind(letters, 0))
colnames(double_letters) <- c("letter", "multiple_occurances")
double_letters$multiple_occurances <- as.double(double_letters$multiple_occurances)
for (i in 1:26) {
  occurances <- five_letter_words %>%
    filter(str_count(as.character(word), letters[i]) > 1)
  double_letters[i, 2] <- nrow(occurances)
}

# Explore outcomes for random mystery words
test_guesses = function(tries = 10, wordtries = 10) {
  # A list of column names to access
  letters = c("letter1", "letter2", "letter3", "letter4", "letter5")
  
  # Initialize a tibble to store the results
  results <- as_tibble(cbind(1:tries, "", "", "", "", 0))
  colnames(results) <- c("loop_num", "word1", "word2", "word3", "word4", "average_possibilities")
  results$average_possibilities <- as.double(results$average_possibilities)
  
  # Choose four words to guess
  for (t in 1:tries) {
    
    # A blank array to hold the results for each randomly chosen mystery word
    result_arr <- rep(0, wordtries)
    
    # Randomly generate guess words and put them in the result table
    guesses <- sample(1:nrow(five_letter_words), 4)
    for (i in 1:4) {
      results[t, i+1] <- five_letter_words[guesses[i], 1]
    }
      
    for (w in 1:wordtries) {
      # Select a mystery word
      mystery_word <- sample(1:nrow(five_letter_words), 1)
      mystery_letters <- five_letter_words[mystery_word,]
        
      # Initialize the list of possible words
      possible_words <- five_letter_words
      
      # Filter the possible words based on matches
      for (i in 1:4) {
        guess_letters <- five_letter_words[guesses[i],]
        for (j in 2:6) {
          if (guess_letters[j] == mystery_letters[j]) {
            # If that character matches, keep only words that also have that character in that position
            possible_words <- possible_words %>%
              filter(.data[[letters[j-1]]] == as.character(guess_letters[j]))
          } else if (str_detect(mystery_letters$word, as.character(guess_letters[j])) == FALSE) {
            # If the character doesn't appear, keep only words that don't have the character
            possible_words <- possible_words %>%
              filter(!str_detect(word, as.character(guess_letters[j])))
          } else {
            # How many times does the letter appear in the guess?
            letter_freq <- str_count(as.character(guess_letters[1]), as.character(guess_letters[j]))
            # How many times does it appear in the mystery word?
            mystery_freq <- str_count(as.character(mystery_letters[1]), as.character(guess_letters[j]))
            if (letter_freq > mystery_freq) {
              # In this case you'll know exactly how many times the letter appears
              # and it is NOT in the same position as in the guess
              possible_words <- possible_words %>%
                filter(str_count(word, as.character(guess_letters[j])) == mystery_freq &
                         .data[[letters[j-1]]] != as.character(guess_letters[j]))
            } else {
              # In this case you'll know the minimum times the letter appears
              # and it is NOT in the same position as in the guess
              possible_words <- possible_words %>%
                filter(str_count(word, as.character(guess_letters[j])) >= letter_freq & 
                         .data[[letters[j-1]]] != as.character(guess_letters[j]))
            }
          }
        }
      }
      
      # In the temporary array, hold the percentage chance of being right 
      # i.e. 1/number of possibilities
      # Remember that we are told the first letter
      result_arr[w] <- 1/(nrow(possible_words %>% filter(letter1 == as.character(mystery_letters[2]))))
    }
    
    # After the number of mystery words specified in wordtries,
    # update the total result for those four guesses
    results[t, 6] = mean(result_arr)
  }
  
  return(results %>% arrange(average_possibilities) %>% select(-loop_num))
}

# For a random set of four words, visualize how much they 
# narrow down the possible mystery word
guess_table <- test_guesses(tries = 100, wordtries = 50)
guess_table %>%
  ggplot(aes(average_possibilities)) +
  geom_histogram(color = "darkcyan", fill="lightcyan") +
  scale_x_continuous(labels = scales::percent) +
  #scale_x_continuous(trans='log10') +
  scale_y_continuous(breaks=seq(10)) +
  labs(title = "Mean Probability of Guessing 'Mystery Word' for 100 Sets of Four Randomly Chosen Guesses",
       subtitle = "Tested Against 50 Randomly Chosen Mystery Words",
       x = "Probability of Guessing The Mystery Word",
       y = "Frequency")



############################################################################################
# The following function checks four guess words against any possible mystery word
# to determine average number of possibilities remaining

possibilities_for_guess <- function(mystery_letters, my_guess) {
  # mystery_letters should be a row of the five_letter_words tibble
  # my_guess is a list of words
  
  # If the guess happens to include the mystery word, there is only one possibility
  if (mystery_letters$word %in% my_guess) {
    return(1)
  }
  
  # Filter the rows that for the guess words
  my_guess_letters <- five_letter_words %>%
    filter(word %in% my_guess)
  
  # A list of column names to dynamically filter in the loop
  letters = c("letter1", "letter2", "letter3", "letter4", "letter5")
  
  # Initialize the list of possibilities
  possible_words <- five_letter_words
  
  # Loop through each letter of each guess, filtering the possibilities
  for (j in 1:4) {
    for (k in 2:6) {
      if (as.character(my_guess_letters[j, k]) == as.character(mystery_letters[k])) {
        # If that character matches, keep only words that also have that character in that position
        possible_words <- possible_words %>%
          filter(.data[[letters[k-1]]] == as.character(my_guess_letters[j, k]))
      } else if (str_detect(mystery_letters$word, as.character(my_guess_letters[j, k])) == FALSE) {
        # If the character doesn't appear, keep only words that don't have the character
        possible_words <- possible_words %>%
          filter(!str_detect(word, as.character(my_guess_letters[j, k])))
      } else {
        # How many times does the letter appear in the guess?
        letter_freq <- str_count(as.character(my_guess_letters[j, 1]), as.character(my_guess_letters[j, k]))
        # How many times does it appear in the mystery word?
        mystery_freq <- str_count(as.character(mystery_letters[1]), as.character(my_guess_letters[j, k]))
        if (letter_freq > mystery_freq) {
          # In this case you'll know exactly how many times the letter appears
          # and it is NOT in the same position as in the guess
          possible_words <- possible_words %>%
            filter(str_count(word, as.character(my_guess_letters[j, k])) == mystery_freq & 
                     .data[[letters[k-1]]] != as.character(my_guess_letters[j, k]))
        } else {
          # In this case you'll know the minimum times the letter appears
          # and it is NOT in the same position as in the guess
          possible_words <- possible_words %>%
            filter(str_count(word, as.character(my_guess_letters[j, k])) >= letter_freq &
                     .data[[letters[k-1]]] != as.character(my_guess_letters[j, k]))
        }
      }
    }
  }
  # Remember that you're given the first letter of the word
  return(nrow(possible_words %>% filter(letter1 == as.character(mystery_letters[2]))))
}

# How well does my guess perform?
my_guess <- c("cares", "soapy", "feint", "lucid")
my_guess_for_all_words <- rep(0, nrow(five_letter_words))
for (i in 1:nrow(five_letter_words)) {
  current_word <- five_letter_words[i, ]
  my_guess_for_all_words[i] <- possibilities_for_guess(current_word, my_guess)
}

mean(my_guess_for_all_words)
mean(my_guess_for_all_words == 1)
# On average, there were 1.47 possibilities remaining
# There was one possibility 73% of the time

five_letter_words[which.max(my_guess_for_all_words), ]
# The word that tripped my guess up most was ragee

# Best random guess
random_guess <- c("webby", "riper", "brunt", "yacks")

# Worst random guess
#random_guess <- c("loony", "along", "scums", "numbs")
random_guess_for_all_words <- rep(0, nrow(five_letter_words))
for (i in 1:nrow(five_letter_words)) {
  current_word <- five_letter_words[i, ]
  random_guess_for_all_words[i] <- possibilities_for_guess(current_word, random_guess)
}

mean(random_guess_for_all_words)
mean(random_guess_for_all_words == 1)


# Based on my guesses, view the possibilities remaining for mystery word ragee
five_letter_words %>%
  filter(!str_detect(word, "c|s|o|p|y|f|i|n|t|l|u|d") &
           letter1=="r" &
           letter2=="a" &
           letter4=="e" &
           letter3!="r" &
           letter3!="a" &
           letter2!="e")