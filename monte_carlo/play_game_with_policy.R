max_dict <- function(d, col = NA) {
  assert_that(!is.na(col), msg = 'Pick a column with col argument. Currently, it is empty')
  
  
  max_coord <- d[which.max(d[[col]]),c('row', 'col')]
  max_value <- d[[col]][which.max(d[[col]])]
  return(data.frame(max_coord, max_value))
}
max_dict(V, col = "value")

play_game(grid, policy)

play_game <- function(grid, policy, verbose = F, windy = F){
  # returns a list of states and corresponding returns
  
  assert_that(all(c('row','col','action') %in% colnames(policy)), msg = 'Wrong colnames. They should be "row", "col", and "action"')
  
  # reset game to start at a random position
  # we need to do this, because given our current deterministic policy
  # we would never end up at certain states, but we still want to measure their value
  grid = standard_grid()
  start_states = grid$actions[, c('row', 'col')]
  start_idx    = sample.int(nrow(start_states), 1)
  grid$set_state(start_states[start_idx,])
  s = grid$current_state()
  a = base::sample(ALL_POSSIBLE_ACTIONS, 1)   # First action completely random
  # sar stands for state-action-reward triple
  sar = data_frame(row = s[1], col = s[2], action = a, reward = 0)
  
  seen_states <- data_frame(row = numeric(0), col = numeric(0))
  # generating an episode
  while (TRUE){
    
    old_s = grid$current_state()
    r = grid$move(random_action(a))
    s = grid$current_state()
    is_seen = if(nrow(seen_states)) s[1] == seen_states$row & s[2] == seen_states$col else FALSE
    if(any(is_seen)) {
      # if the episode is seen more than once, and we assume the environment to be constant, assign it a highly negative reward to avoid that space in the future
      sar %<>% add_row(row = s[1], col = s[2], action = NA, reward = -100)
    } else if(grid$game_over()){
      sar %<>% add_row(row = s[1], col = s[2], action = NA, reward = r) # game over so we do not take any action
      break # break when the game ends
    } else{ 
      # probably the most frequent scenario
      a = filter(policy, row == s[1], col == s[2])%$%action
      sar %<>% add_row(row = s[1], col = s[2], action = a, reward = r)
    }
    seen_states %<>% add_row(row = s[1], col = s[2])
  }
  
  # calculate the returns by working backwards from the terminal state
  G        <-  0
  first <- TRUE
  sa_ret <- adply(arrange(sar, -row_number()), 1, function(x){
      
    to_ret <- if(first) {
      first <<- FALSE; NULL
    } else  {
      data.frame(row = x[1] , col = x[2], return = G) 
    }
    G <<- r + GAMMA * G
    return(to_ret)
  }, .id = NULL)
  sa_ret <- arrange(sa_ret, -row_number()) %>% select(row, col, return)
  return(sa_ret)
}

#states_and_returns <- play_game(grid, policy)


