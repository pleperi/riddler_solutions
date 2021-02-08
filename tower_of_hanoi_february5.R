library("tidyverse")
theme_set(theme_light())

random_moves <- function(number_of_moves, number_of_disks=3, number_of_rods=3) {
  
  # initialize variables to count total moves and total completed switches
  total_moves <- 0
  total_completed <- 0
  moves_array <- rep(0, floor(number_of_moves/(2^number_of_disks - 1)))
  
  # need a way to determine if the stack has moved to a different tower
  current_base <- 1
  
  # two arrays of random samples will help determine moves
  a <- sample(1:number_of_rods, number_of_moves, replace=TRUE)
  b <- sample(1:number_of_rods, number_of_moves, replace=TRUE)
  
  # keep track of current disks on all stacks
  stacks <- vector(mode = "list", length = number_of_rods)
  disks <- seq(1, number_of_disks)
  stacks[[1]] <- disks
  
  
  for (i in 1:number_of_moves) {
    
    # if a and b have the same stack sampled, continue
    if (a[i] == b[i]) {
      next
    }
    
    lengthA <- length(stacks[[a[i]]])
    lengthB <- length(stacks[[b[i]]])
    
    # if neither sampled stack has a disk, no move is possible
    if (lengthA == 0 & lengthB == 0) {
      next
    } else if (lengthA == 0) {
      
      # the top disk on B moves to A
      # if B only had one disk, it's now empty
      # otherwise, it now has the remaining disks
      topElementB <- stacks[[b[i]]][1]
      stacks[[a[i]]] <- topElementB
      if (lengthB == 1) {
        stacks[b[i]] <- list(NULL)
      } else {
        stacks[[b[i]]] <- stacks[[b[i]]][2:lengthB]
      }
    } else if (lengthB == 0) {
      
      # the top disk on A moves to B
      # if A only had one disk, it's now empty
      # otherwise, it now has the remaining disks
      topElementA <- stacks[[a[i]]][1]
      stacks[[b[i]]] <- topElementA
      if (lengthA == 1) {
        stacks[a[i]] <- list(NULL)
      } else {
        stacks[[a[i]]] <- stacks[[a[i]]][2:lengthA]
      }
    } else {
      
      # both stacks have disks
      # figure out which top disk is smaller and move it
      topElementA <- stacks[[a[i]]][1]
      topElementB <- stacks[[b[i]]][1]
      
      if (topElementA > topElementB) {
        
        # in this case the top disk from B moves to A
        stacks[[a[i]]] <- append(stacks[[a[i]]], stacks[[b[i]]][1], 0)
        if (lengthB == 1) {
          stacks[b[i]] <- list(NULL)
          
          # check if this was a completed switch
          # if so, increase the count and change the current base
          if (lengthA == (number_of_disks - 1) & a[i] != current_base) {
            total_completed <- total_completed + 1
            moves_array[total_completed] <- total_moves + 1
            total_moves <- 0
            current_base <- a[i]
            next
          }
        } else {
          stacks[[b[i]]] <- stacks[[b[i]]][2:lengthB]
        }
      } else if (topElementB > topElementA) {
        
        # in this case the top disk from A moves to B
        stacks[[b[i]]] <- append(stacks[[b[i]]], stacks[[a[i]]][1], 0)
        if (lengthA == 1) {
          stacks[a[i]] <- list(NULL)
          
          # check if this was a completed switch
          # if so, increase the count and change the current base
          if (lengthB == (number_of_disks - 1) & b[i] != current_base) {
            total_completed <- total_completed + 1
            moves_array[total_completed] <- total_moves + 1
            total_moves <- 0
            current_base <- b[i]
            next
          }
        } else {
          stacks[[a[i]]] <- stacks[[a[i]]][2:lengthA]
        }
      } else {
        
        # shouldn't get here, but just in case...
        next
      }
    }
    total_moves <- total_moves + 1
  }
  
  return(moves_array[1:total_completed])
}

moves_to_complete <- random_moves(5000000, 4, 4)
# Gives about 70.6 for 3 disks and 3 towers
# About 405.9 for 4 disks and 3 towers
# About 2148.5 for 5 disks and 3 towers

moves_to_complete %>%
  as.data.frame() %>%
  ggplot(aes(moves_to_complete)) +
  geom_histogram(color="darkcyan", fill = "lightcyan") +
  scale_x_continuous(trans='log10') +
  labs(
    title = "Number of Moves to Complete Reve's Puzzle with 4 Disks",
    subtitle = "Moves Selected Randomly",
    x = "Number of Moves",
    y = ""
  )

loop_results <- rep(0, 6)
for (i in 2:7) {
  loop_results[i-1] <- mean(random_moves(5000000, i, 3))
}

# for 11 and 12 disks, the task isn't completed a single time with 5000000 moves
data.frame(number_disks=2:7, loop_results) %>%
  ggplot(aes(number_disks, loop_results)) +
  scale_y_continuous(trans='log10') +
  geom_point(size = 5) +
  geom_smooth(se = FALSE) +
  labs(
    title = "Number of Moves to Complete Tower of Hanoi Challenge with N Disks",
    subtitle = "Moves Selected Randomly",
    x = "Number of Disks",
    y = "Average Moves"
  )

# data.frame(number_disks=2:7, loop_results) %>%
#   mutate(log_results = log10(loop_results)) %>%
#   lm(log_results ~ number_disks, data = .)
