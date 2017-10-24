  grid = negative_grid(step_cost=-0.9)

  
  # print rewards
  print("rewards:")
  print_values(grid$rewards, grid)
  
  # state -> action
  # initialize a random policy
  policy = data.frame(grid$actions[,1:2])
  policy$action <- base::sample(ALL_POSSIBLE_ACTIONS, nrow(policy), replace = TRUE) #random policy
    
  
  # initialize Q_(s,a) and returns
  Q_ = data.frame(row = numeric(0), col = numeric(0), action = numeric(0), reward = numeric(0))
  # dictionary of state -> list of returns we've received
  
  states = grid$all_states()
  for (s in 1:nrow(states)){
    state_idx <- states[s,]$row == grid$actions$row & states[s,]$col == grid$actions$col
    
    if(any(state_idx)){
      Q_ %<>%add_row(row = states[which(state_idx),]$row, col = states[which(state_idx),]$col,
                                      action = NA, reward = NA)
      for(a in ALL_POSSIBLE_ACTIONS){
        if(is.na(Q_[nrow(Q_),'action'])){
          Q_[nrow(Q_),'action'] <- a
        } else {
          Q_ <- bind_rows(Q_, Q_[nrow(Q_),]); Q_[nrow(Q_), "action"] <- a
        }
          
          #returns[(s,a)] = []
      }
    }else{
      #pass
    }
    
  }
  Q_$reward <- 0# not a terminal state
  returns <- list()
  returns <- plyr::alply(Q_, 1, function(x){
    # TO DO 0
     
  })
  
  
  
  
  
  
  
  
  
  
  # repeat until convergence
  deltas = c()
  for (t in seq_len(200)){
    if (t %% 100 == 0) print(t)
    browser()
    # generate an episode using pi
    biggest_change <-  0
    sa_ret = play_game(grid, policy)
    seen_sa_pairs = c()
    for(i in 1:nrow(sa_ret)){
      # check if we have already seen s
      # called "first-visit" MC policy evaluation
      sa = sa_ret[i, c('row', 'col')]
      if(!sa %in% seen_sa_pairs){
        old_q = Q_[sa_ret$row == Q_$row & sa_ret$col == Q_$col & sa_ret$action == Q_$action, 'reward'] # sa_ret must return action
        browser()
        returns
        Q_[s][a] = np.mean(returns[sa])
        biggest_change = max(biggest_change, np.abs(old_q - Q_[s][a]))
        seen_state_action_pairs.add(sa)
      }
        
      
    }
      
    # deltas.append(biggest_change)
    # 
    # # update policy
    # for s in policy.keys():
    #   policy[s] = max_dict(Q_[s])[0]
    # 
    # plot(deltas)
    # 
    # 
    print ("final policy:")
    print_policy(policy, grid)
    # 
    # # find V
    # V = {}
    # for s, Qs in Q_.iteritems():
    #   V[s] = max_dict(Q_[s])[1]
    # 
    print( "final values:")
    print_values(V, grid)
  }
    