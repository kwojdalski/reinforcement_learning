# https://deeplearningcourses.com/c/artificial-intelligence-reinforcement-learning-in-python
# https://www.udemy.com/artificial-intelligence-reinforcement-learning-in-python
source('./mdp/grid_world.R')
source('./mdp/print_functions.R')
SMALL_ENOUGH = 1e-3 # threshold for convergence
GAMMA        = 1.0 # discount factor
VERBOSE      = T
require(glue)




# iterative policy evaluation

# create standard gri
grid = standard_grid()

# get all possible states
states = grid$all_states()

### uniformly random actions ###
# initialize V(s) = 0

V <- cbind(states, value = 0)

  

# repeat until convergence

while (TRUE){
  
  biggest_change = 0
  
  
  
  
  new_V <- plyr::adply(states, 1, function(x){
    state_index <- which(x$row == grid$actions$row & x$col == grid$actions$col)
    value_index <- which(x$row == V$row & x$col == V$col)
    
    old_v = V[value_index, 'value']
    
    
    # V(s) only has value if it's not a terminal state
    
    if(any(state_index)){
      new_v = 0 # we will accumulate the answer
      avail_actions <- unlist(grid$actions[state_index,]$avail_actions) # TO DO
      p_a   = 1.0 / length(avail_actions) # each action has equal probability
      for(a in avail_actions){
        
        grid$set_state(x)
        grid$current_state()
        #if(x$row == 1 & x$col == 2) browser()
        r = grid$move(a)
        grid$current_state()
        new_v = new_v + p_a * (r + GAMMA * filter(V, 
                                                  row == grid$current_state()[1], 
                                                  col == grid$current_state()[2])%$%value)
      }
      
      V[value_index, 'value'] = new_v
      biggest_change = max(biggest_change, abs(old_v - V[value_index, 'value']))
    }
    
    return(V[value_index, ])
  })

    biggest_change <- max(abs(new_V$value - V$value))
    if(VERBOSE) print(new_V)
    V <- new_V
    if(VERBOSE) print(biggest_change)
    if(biggest_change < SMALL_ENOUGH){
      break
    }
    
 
}
  
if(VERBOSE){
  print ("values for uniformly random actions:")
  print_values(V, grid)
  
}
