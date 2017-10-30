    
    
    
play_game <- function(grid, policy, exploring_start = T, 
                      start_state = c(2, 0), windy = F, verbose = F){
  # returns a list of states and corresponding returns
  
  assert_that(all(c('row','col','action') %in% colnames(policy)), 
              msg = 'Wrong colnames. They should be "row", "col", and "action"')

  # reset game to start at a random position
  # we need to do this, because given our current deterministic policy
  # we would never end up at certain states, but we still want to measure their value
  grid = standard_grid()
  start_states = grid$actions[, c('row', 'col')]
  if(exploring_start){
    start_idx    = sample.int(nrow(start_states), 1)
    grid$set_state(start_states[start_idx,])  
  } else{
    assert_that(length(start_state) > 0, all(!is.na(start_state)), msg = 'Set proper start_state')
    start_state <- start_state %>% when(is.data.frame(.) ~ .,
                                        is.numeric(.)  ~data.frame(row = .[1], col = .[2]))
    grid$set_state(start_state)  
  }
  
  s = grid$current_state()
  
  states_and_rewards = data_frame(row = s[1], col = s[2], reward = 0) # list of tuples of (state, reward)
  while (!grid$game_over()){
    policy_idx <- row_matches(s, policy[,1:2], same_cols = F)
    a = policy$action[policy_idx]
    if(windy){
      r = grid$move(random_action(a))
    } else{
      r = grid$move(a)
    }
    s = grid$current_state()
    states_and_rewards <- rbind(states_and_rewards,
                                data_frame(row = s[1], col = s[2], reward = r))
  }
   
  # calculate the returns by working backwards from the terminal state
  G <- 0
  states_and_returns = data.frame(row = numeric(0), col = numeric(0), return = numeric(0))
  
  reversed_idx <- rev(seq_len(nrow(states_and_rewards)))
  states_and_returns <- plyr::adply(reversed_idx, 1, function(idx){
    r <- states_and_rewards[idx, ]
    # the value of the terminal state is 0 by definition
    # we should ignore the first state we encounter
    # and ignore the last G, which is meaningless since it doesn't correspond to any move
    
    if (!idx == max(reversed_idx)){
      states_and_returns <- add_row(states_and_returns, row = r$row, col = r$col, return = G)
      
    } else{
      states_and_returns <- NULL
    }
    G <<-  r$reward + GAMMA*G
    
   # we want it to be in order of state visited
    return(states_and_returns)
  }, .id = NULL)
  
  
  states_and_returns <- arrange(states_and_returns, -row_number())
  return(states_and_returns)
  
}

#states_and_returns <- play_game(grid, policy)
