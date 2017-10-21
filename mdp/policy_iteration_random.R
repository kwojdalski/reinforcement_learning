# https://deeplearningcourses.com/c/artificial-intelligence-reinforcement-learning-in-python
# https://www.udemy.com/artificial-intelligence-reinforcement-learning-in-python
source('./mdp/grid_world.R')
source('./mdp/print_functions.R')
SMALL_ENOUGH = 1e-3 # threshold for convergence
GAMMA = 0.9
ALL_POSSIBLE_ACTIONS = c('U', 'D', 'L', 'R')
VERBOSE = T
require(glue)





grid = negative_grid(step_cost = -1.0)
grid$all_states()
# states will be positions (i,j)
# simpler than tic-tac-toe because we only have one "game piece"
# that can only be at one position at a time
#  grid$rewards
print('Rewards:\n')
print_values(grid$rewards, grid)

### uniformly random actions ###
# initialize V(s) = 0


# random actions
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

# repeat until convergence - will break out when policy does not change
while(TRUE){
  
  # policy evaluation
  while(TRUE){
    biggest_change <- 0
    new_V <- plyr::adply(states, 1, function(x){
      policy_index <- which(x$row == policy$row & x$col == policy$col)
      value_index <- which(x$row == V$row & x$col == V$col)
      
      
      
      # V(s) only has value if it's not a terminal state
      if(any(policy_index)){
        for(a in ALL_POSSIBLE_ACTIONS){
          
          if(a == policy$action[policy_index]){
            p = 0.5
          } else {
            p = 0.5/3
          }
        
        grid$set_state(x)
        r = grid$move(a)
        
        new_v = new_v + p * (r + GAMMA * filter(V, row == grid$current_state()[1], col == grid$current_state()[2])%$%value)
        }
      }
      return(cbind(x, value = new_v))
    })
    
    
    biggest_change <- max(abs(new_V$value - V$value))
    
    if(VERBOSE) print(glue("Biggest change in values: {round(biggest_change,5)}"))
    
    if(biggest_change < SMALL_ENOUGH){
      break
    }
    V = new_V
    # Policy evaluation ended and V data frame is the new set of values for each position
  }
   
  # POLICY IMPROVEMENT
  is_policy_converged <- TRUE
  old_policy <- policy
  policy <- plyr::adply(states, 1, function(x){
    policy_index <- which(x$row == policy$row & x$col == policy$col)
    
    if(any(policy_index)){
      old_a <- policy$action[policy_index]        # old action to replace
      new_a <- NA                                 # new action, yet to be set
      best_value <- -Inf                           
      for(a in ALL_POSSIBLE_ACTIONS){
        new_v = 0
        for(a2 in ALL_POSSIBLE_ACTIONS){
          if(a == a2){
            p = 0.5
          }else{
            p = 0.5/3
          }
          grid$set_state(x)
          r = grid$move(a2)                          # if not in a set of possible actions it bounces back
          new_v = new_v + p * (r + GAMMA * filter(V, row == grid$current_state()[1], col == grid$current_state()[2])%$%value)
          if(new_v > best_value){
            best_value <- new_v
            new_a  = a  
          }
        
        }
      }
      policy$action[policy_index] <- new_a  
      
      
    }
    return(policy[policy_index,])
  })
  
  if(!identical(old_policy$action, policy$action)) {
    if(VERBOSE) print(policy)
    is_policy_converged <- FALSE
    #print_values(V, grid)
  }
  
  if(is_policy_converged) break
  
}

if(VERBOSE){
  print("Initial policy once again:")
  initial_policy
  print ("Values for uniformly random actions:")
  print_values(V, grid)
  
}
