max_dict <- function(d, val_col = NA, coord_col = c('row', 'col', 'action'), 
                     group_by = NA, val_col_ = NA) {
  assert_that(!is.na(val_col), msg = 'Pick a column with col argument. Currently, it is empty')
  assert_that(val_col %in% colnames(d), msg = 'Pick a proper val col')
  assert_that(all(coord_col %in% colnames(d)), msg = 'Pick proper coord col(s)')
  if(any(is.na(group_by))){
    max_coord <- d[which.max(d[[val_col]]), coord_col]
    max_value <- d[[val_col]][which.max(d[[val_col]])]  
    ret <- data.frame(max_coord, max_value)
  }else{
    to_sum <- enquo(val_col_)
    ret <- d %>% group_by_(.dots = group_by) %>%
      filter(UQE(to_sum) == max(!!to_sum))
  }
  
  return(ret)
}


play_game <- function(grid, policy, 
                      exploring_start = TRUE, start_state = c(2, 0),
                      verbose = F, windy = F){
  # returns a list of states and corresponding returns
  
  assert_that(all(c('row','col','action') %in% colnames(policy)), msg = 'Wrong colnames. They should be "row", "col", and "action"')
  
  # reset game to start at a random position
  # we need to do this, because given our current deterministic policy
  # we would never end up at certain states, but we still want to measure their value
  
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
    old_s <- grid$current_state()
    #if(identical(old_s, c(2,3))) browser()
    r       <- grid$move(a)
    s       <- grid$current_state()
    is_seen <- row_matches(s, seen_states, same_cols = F)
    
    if(any(is_seen) & exploring_start){
      # if the episode is seen more than once, and we assume the environment to be constant, assign it a highly negative reward to avoid that space in the future
      sar %<>% rbind(data.frame(row = s[1], col = s[2], action = NA, reward = -100))
      break
    } else if(grid$game_over()){
      # game over so we do not take any action
      sar %<>% rbind(data.frame(row = s[1], col = s[2], action = NA, reward = r)) 
      break # break when the game ends
    } else { 
      # probably the most frequent scenario
      
      policy_idx <- row_matches(s, policy[,c('row', 'col')], same_cols = F)
      a <- if(exploring_start) policy[policy_idx, 'action'] else random_action(policy[policy_idx, 'action'])
      sar %<>% rbind(data.frame(row = s[1], col = s[2], action = a, reward = r))
    }
    seen_states %<>% rbind(data.frame(row = s[1], col = s[2]))
  }
  
  # calculate the returns by working backwards from the terminal state
  G        <-  0
  first    <- TRUE
  
  
  sa_ret <- adply(sar[rev(seq_len(nrow(sar))),], 1, function(x){ # TO CHECK 
    
    to_ret <- if(first) {
      first <<- FALSE
      NULL
    } else  {
      data.frame(row = x[1] , col = x[2], return = G) 
    }
    G <<- x$reward + GAMMA * G
    return(to_ret)
  }, .id = NULL)
  sa_ret <- sa_ret[rev(order(as.numeric(row.names(sa_ret)))),]
  
  
  #sa_ret <- arrange(sa_ret, -row_number()) %>% select(row, col, action, return)
  
  return(sa_ret)
}


