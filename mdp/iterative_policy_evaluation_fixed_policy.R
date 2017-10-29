
### fixed policy ###
policy = data.frame(
  row    = c(2, 1, 0, 0, 0, 1, 2, 2, 2),
  col    = c(0, 0, 0, 1, 2, 2, 1, 2, 3),
  reward = c('U','U','R','R','R','R','R','R','U')
)
print_policy(policy, grid)

# initialize V(s) = 0
V <- states
V$value = 0

# let's see how V(s) changes as we get further away from the reward
gamma = 0.9 # discount factor

# repeat until convergence
while (TRUE){
  
  biggest_change = 0
  new_V <- plyr::adply(states, 1, function(x){
    
    state_idx <- row_matches(x, grid$actions)
    value_idx <- row_matches(x, V)
    
    old_v = V[value_idx, 'value']
    
    
    # V(s) only has value if it's not a terminal state
    
    if(any(state_idx)){
      new_v = 0 # we will accumulate the answer
      avail_actions <- unlist(grid$actions[state_idx,]$avail_actions) # TO DO
      p_a   = 1.0 / length(avail_actions) # each action has equal probability
      for(a in avail_actions){
        a = policy$reward[policy$row ==x$row & policy$col == x$col]
        grid$set_state(x)
        grid$current_state()
        
        
        #if(x$row == 1 & x$col == 2) browser()
        r = grid$move(a)
        grid$current_state()
        new_v = new_v + p_a * (r + gamma * filter(V, 
                                                  row == grid$current_state()[1], 
                                                  col == grid$current_state()[2])%$%value)
      }
      
      V[value_idx, 'value'] = new_v
      biggest_change = max(biggest_change, abs(old_v - V[value_idx, 'value']))
    }
    
    return(V[value_idx, ])
  })
  biggest_change <- max(abs(new_V$value - V$value))
  V <- new_V
  print(biggest_change)
  if(biggest_change < SMALL_ENOUGH){
    break
  }
  
  
}  
print ("values for fixed policy:")
print_policy(policy, grid)
print_values(V, grid)
