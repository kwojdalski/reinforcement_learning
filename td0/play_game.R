
play_game <- function(grid, policy, 
                      exploring_start = TRUE, start_state = c(2, 0),
                      verbose = F, windy = F){
  # returns a list of states and corresponding returns
  
  assert_that(all(c('row','col','action') %in% colnames(policy)), msg = 'Wrong colnames. They should be "row", "col", and "action"')
  
  # reset game to start at a random position
  # we need to do this, because given our current deterministic policy
  # we would never end up at certain states, but we still want to measure their value
  
  
  
  grid$set_state(start_state)
  s = start_state
  states_and_rewards = data_frame(row = s[1], col = s[2], reward = 0)
  while(!grid$game_over()){
    a <- policy[row_matches(s, policy[,1:2], same_cols = F), ]$action
    a <- random_action(a)
    
    r <- grid$move(a)
    s = grid$current_state()
    # sar stands for state-action-reward triple
    states_and_rewards <- rbind(states_and_rewards, 
                                data.frame(row = s[1], col = s[2], reward = r))
  }
  
  
  
  return(states_and_rewards)
}



