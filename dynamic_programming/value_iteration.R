# https://deeplearningcourses.com/c/artificial-intelligence-reinforcement-learning-in-python
# https://www.udemy.com/artificial-intelligence-reinforcement-learning-in-python
source('./mdp/grid_world.R')
source('./mdp/print_functions.R')
SMALL_ENOUGH = 1e-3 # threshold for convergence
GAMMA = 0.9
ALL_POSSIBLE_ACTIONS = c('U', 'D', 'L', 'R')
VERBOSE = T
require(glue)





grid = negative_grid()
grid$all_states()
# states will be positions (i,j)
# simpler than tic-tac-toe because we only have one "game piece"
# that can only be at one position at a time
#  grid$rewards
print('Rewards:\n')
print_values(grid$rewards, grid)

### uniformly random actions ###
# initialize V(s) = 0


# random policy at first
policy <- by_row(grid$actions, 
                 ..f = function(x){ (unlist(x$avail_actions)) %>% base::sample(1)}, 
                 .collate = 'row', .to = 'action') %>% select(-avail_actions)

# repeat until convergence
print('Initial policy')
initial_policy <- print_policy(policy, grid)
biggest_change = 0
states <- grid$all_states()


V <- plyr::adply(states, 1, function(x){
  
  
  state_index <- which(x$row == grid$actions$row & x$col == grid$actions$col)
  if(any(state_index)){
    x$value <- runif(1)
  }else{
    x$value = 0
  }
  return(x)
})




rm(new_V)


  
  # policy evaluation
  while(TRUE){
    biggest_change <- 0
    new_V <- plyr::adply(states, 1, function(x){
      
      policy_index <- which(x$row == policy$row & x$col == policy$col)
      value_index <- which(x$row == V$row & x$col == V$col)
      
      # V(s) only has value if it's not a terminal state
      if(any(policy_index)){
        new_v = -Inf
        for(a in ALL_POSSIBLE_ACTIONS){
          grid$set_state(x)
          r = grid$move(a)
          v = r + GAMMA * filter(V, row == grid$current_state()[1], col == grid$current_state()[2])%$%value
          
          if(v > new_v) new_v = v
        }
      }
       
        #biggest_change = max(biggest_change, abs(old_v - V[value_index, 'value']))
      return(cbind(x, value = new_v))
    })
    
    
    biggest_change <- max(abs(new_V$value - V$value))
    
    if(VERBOSE) print(glue("Biggest change in values: {round(biggest_change,5)}"))
    
    if(biggest_change < SMALL_ENOUGH){
      break
    }
    V = new_V
    # Find a policy that leads to max_V
    # Here we do no

    policy <- plyr::adply(policy[,1:2], 1, function(x){ # policy keys rather than states
      best_a = NA
      best_value = -Inf
      
      for(a in ALL_POSSIBLE_ACTIONS){
        grid$set_state(x)
        r = grid$move(a)
        v = r + GAMMA * filter(V, row == grid$current_state()[1], col == grid$current_state()[2])%$%value
        if( v > best_value){
          # best action and best value, or better than the previous ones
          best_value = v
          best_a = a 
        }
      }
      
      policy$action[x$row == policy$row & x$col == policy$col] <- best_a
      
      return(policy[x$row == policy$row & x$col == policy$col,])
    })
    
    
    
  }

print('Values: ')  
print_values(V, grid)
print('Policy: ')
print_policy(policy, grid)

