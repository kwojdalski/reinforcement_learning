# https://deeplearningcourses.com/c/artificial-intelligence-reinforcement-learning-in-python
# https://www.udemy.com/artificial-intelligence-reinforcement-learning-in-python

SMALL_ENOUGH         = 1e-3
GAMMA                = 0.9
ALL_POSSIBLE_ACTIONS = c('U', 'D', 'L', 'R')
NUMBER_OF_ITERATIONS = seq_len(100)
# NOTE: this is only policy evaluation, not optimization



  # use the standard grid again (0 for every step) so that we can compare
  # to iterative policy evaluation
grid = standard_grid()

# print rewards
print("rewards:")
print_values(grid$rewards, grid)

# state -> action
policy = data.frame(
  row    = c(2, 1, 0, 0, 0, 1, 2, 2, 2),
  col    = c(0, 0, 0, 1, 2, 2, 1, 2, 3),
  action = c('U','U','R','R','R','R','R','R','U')
)

# initialize V(s) and returns
V <- data_frame(row = integer(0), col = integer(0), value = numeric(0)) # list of tuples of (state, reward)
returns = c() # dictionary of state -> list of returns we've received
states = grid$all_states()

returns <- plyr::alply(states, 1, function(x){
  action_idx <- x$row == grid$actions$row & x$col == grid$actions$col
  value_idx  <- x$row == V$row & x$col == V$col
  if(any(action_idx)){
    to_ret <- c(return = numeric(0)) # list of tuples of (state, reward)
    return(to_ret)
  
  }
})
terminal_idx <- sapply(returns, is.null)
V <- states[!terminal_idx, ]
returns_coord <- attr(returns, 'split_labels')


#### INITALIZE GAMES

for(t in NUMBER_OF_ITERATIONS){
  states_and_returns <- play_game(grid, policy)
  seen_states <- data.frame(row = numeric(0), col = numeric(0))
  
  a_ply(states_and_returns, 1, function(x){
    
    seen_states_idx <- if(nrow(seen_states)) x$row == seen_states$row & x$col == seen_states$col else FALSE
    value_idx <- x$row == V$row & x$col == V$col
    ret_idx <- which(x$row == returns_coord$row & x$col ==returns_coord$col)
    # check if we have already seen s
    # called "first-visit" MC policy evaluation
    if(!any(seen_states_idx)){
      # returns for each state will always be the same in each episode as policy is FIXED
      returns[[ret_idx]] <<- c(returns[[ret_idx]], x$return) 
      V[value_idx, 'value'] <<- mean(returns[[ret_idx]]) # value updated
    }
    
  })
  seen_states <- V[!is.na(V$value),c('row','col')] 
}
  
returns
  
  


