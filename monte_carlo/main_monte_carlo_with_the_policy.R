ITERATIONS = 100
p_load(prodlim)  
grid = negative_grid(step_cost = -0.9)
  
  
  
  # print rewards
  print("rewards:")
  print_values(grid$rewards, grid)
  
  # state -> action
  # initialize a random policy
  policy        <-  data.frame(grid$actions[,1:2])
  policy$action <- base::sample(ALL_POSSIBLE_ACTIONS, nrow(policy), replace = TRUE) #random policy
    
  
  # initialize Q_(s,a) and returns
  Q_ = data.frame(row = numeric(0), col = numeric(0), action = numeric(0), reward = numeric(0))
  # dictionary of state -> list of returns we've received
profvis({}
  states <- grid$all_states()
  for (s in 1:nrow(states)){
    state_idx <- states[s,]$row == grid$actions$row & states[s,]$col == grid$actions$col
    
    if(any(state_idx)){
      Q_ %<>% rbind(data.frame(row = states[which(state_idx),]$row, col = states[which(state_idx),]$col,
                    action = NA, reward = NA))
      for(a in ALL_POSSIBLE_ACTIONS){
        if(is.na(Q_[nrow(Q_),'action'])){
          Q_[nrow(Q_), 'action'] <- a
        } else {
          Q_ <- bind_rows(Q_, Q_[nrow(Q_),])
          Q_[nrow(Q_), "action"] <- a
        }
          #returns[(s,a)] = []
      }
    } else {
      #pass
    }
    
  }
  
  
  Q_$reward <- 0# not a terminal state
  returns <- plyr::alply(Q_, 1, function(x){})
  
  
  # repeat until convergence
  deltas = c()
  for (t in seq_len(ITERATIONS)){
    if (t %% 50 == 0) print(t)
    # generate an episode using pi
    biggest_change <- 0
    sa_ret         <- play_game(grid, policy) # ADD ACTIONS IN OUTPUT
    seen_sa_pairs = data.frame(row = numeric(0), col = numeric(0), action = character(0))
    for(i in 1:nrow(sa_ret)){
      # check if we have already seen s
      # called "first-visit" MC policy evaluation
      i <- 1
      sa = sa_ret[i, c('row', 'col', 'action')]
      if(nrow(seen_sa_pairs) == 0 || is.na(row.match(sa , seen_sa_pairs))){
        q_idx <- sa$row == Q_$row & sa$col == Q_$col & sa$action == Q_$action
        old_q = Q_[q_idx, 'reward'] # sa_ret must return action
        returns_ls_idx <- sa$row == attr(returns, 'split_labels')$row & sa$col == attr(returns, 'split_labels')$col & sa$action == attr(returns, 'split_labels')$action
        returns[[which(returns_ls_idx)]] %<>% c(sa_ret[i, 'return'])
        
        # New Q
        Q_[q_idx, 'reward'] = median(returns[[which(returns_ls_idx)]], na.rm = T)
        biggest_change = max(biggest_change, abs(old_q - Q_[q_idx, 'reward']))
        
        seen_sa_pairs %<>% union_all(sa)
      }
    }
    deltas%<>%c(biggest_change)
    
    # update policy
    
    old_policy <-  policy
    for(i in 1:nrow(policy)){
      
      q_idx <- which(policy[i, 'row'] == Q_$row & policy[i, 'col'] == Q_$col)
      
    
      policy[i, ] <- max_dict(Q_[q_idx,], val_col = 'reward', coord_col = c('row', 'col', 'action'))
      
    } 
      
    #if(!identical(old_policy, policy)) print(policy)
    
  }
})
  
  print ("final policy:")
  print_policy(policy, grid)
  print( "final values:")
  colnames(Q_)
  max_dict(Q_, coord_col = c('row', 'col', 'action'), 
           group_by = c('row', 'col'), 
           val_col_ = reward,
           val_col = 'reward')

    
  
  
  
  require(profvis)
  profvis(for(i in 1:100) sa_ret <- arrange(sa_ret, -row_number()) )
  
  
  ?rev
  