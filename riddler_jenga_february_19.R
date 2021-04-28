### Solving FiveThirtyEight's Riddler Classic from February 19, 2021
# Resource to explain book-stacking problem:
# https://mathworld.wolfram.com/BookStackingProblem.html

library("tidyverse")

iterate_stacks <- function(iterations = 100) {
  # Wrapper function to run a certain number of iterations
  result_array <- rep(0, iterations)
  
  for (i in 1:iterations) {
    result_array[i] <- get_stack_height()
  }
  
  return (result_array)
}

get_stack_height <- function () {
  stack_count <- 2 # Tower is guaranteed to always be at least 2 blocks tall
  current_direction_count <- 1
  current_overhang <- runif(1) - .5 # Center at 0 to simplify calculation
  max_overhang <- .5
  if (current_overhang < 0) {
    current_direction <- "L"
  } else {
    current_direction <- "R"
  }
  
  while(TRUE) {
    new_block <- runif(1) - .5
    if ((new_block < 0 & current_direction == "R") | 
        (new_block >= 0 & current_direction == "L")) {
      # Switch directions
      stack_count <- stack_count + 1
      current_direction_count <- 1
      current_overhang <- new_block
      max_overhang <- .5
      if (current_overhang < 0) {
        current_direction <- "L"
      } else {
        current_direction <- "R"
      }
    } else {
      stack_count <- stack_count + 1
      current_direction_count <- current_direction_count + 1
      current_overhang <- current_overhang + new_block
      max_overhang <- max_overhang + 1/(2*current_direction_count)
      
      if (abs(current_overhang) > max_overhang) {
        # Part of the tower falls over
        return (stack_count)
      }
    }
  }
}

# Histogram of the results shows a long tail
result_array <- iterate_stacks(100000)
result_array %>%
  as_tibble() %>%
  #ggplot(aes(log(value))) +
  ggplot(aes(value)) +
  geom_histogram(fill = "lightcyan", color="darkcyan") +
  labs(x = "", y = "", title = "Number of Blocks Stacked in Riddler Jenga", subtitle = "100,000 Iterations")
