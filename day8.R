library(adventdrob)
library(tidyverse)

input <- advent_input(8, year = 2022, parse = T) |> grid_matrix(x, '')

# part 1
edges <- (nrow(input)*2)+(ncol(input)*2)-4
inside <- 0

for (i in 2:98) {
  
  for (j in 2:98) {
      
      flag <- 0 
      up <- input[1:(j-1), i]
      if (all(up < input[j,i])) flag <- flag+1
      
      down <- input[99:(j+1), i]
      if (all(down < input[j,i])) flag <- flag+1
      
      left <- input[j, 1:(i-1)]
      if (all(left < input[j,i])) flag <- flag+1
      
      right <- input[j, 99:(i+1)]
      if (all(right < input[j,i])) flag <- flag+1
      
      inside <- inside + (flag>0)
  }
}

edges + inside

# part 2

distance <- matrix(0, ncol=99, nrow=99)

for (i in 2:98) {
  
  for (j in 2:98) {
    
    up <- input[(j-1):1, i]
    down <- input[(j+1):99, i]
    left <- input[j, (i-1):1]
    right <- input[j, (i+1):99]
    
    v1 <- min( min(which(up >= input[j,i])), length(up))
    v2 <- min( min(which(down >= input[j,i])), length(down))
    v3 <- min( min(which(left >= input[j,i])), length(left))
    v4 <- min( min(which(right >= input[j,i])), length(right))
        
    distance[j,i] <- v1*v2*v3*v4
  }
}

max(distance)
