source('./monte_carlo/util.R')

ITERATIONS = 1000
GAMMA      = 0.9
EPS        = 0.1
grid       = negative_grid(step_cost = -0.1)
ALPHA      = 0.1
ALL_POSSIBLE_ACTIONS = c('U', 'D', 'L', 'R')

# print rewards
print("rewards:")
print_values(grid$rewards, grid)

# state -> action
# initialize a random policy


policy = data.frame(
  row    = c(2, 1, 0, 0, 0, 1, 2, 2, 2),
  col    = c(0, 0, 0, 1, 2, 2, 1, 2, 3),
  action = c('U','U','R','R','R','R','R','R','U')
)

Q_ = data.frame(row = numeric(0), col = numeric(0), action = numeric(0), reward = numeric(0))
# dictionary of state -> list of returns we've received

states <- grid$all_states()
for (s in 1:nrow(states)){
  
  state_idx <- row_matches(states[s,], grid$actions)
  
  if(any(state_idx)){
    Q_ %<>% rbind(data.frame(row = states[state_idx,]$row, col = states[state_idx,]$col,
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
Q_$reward <- 0


update_counts_sa    <- data.frame(row    = Q_$row,
                                  col    = Q_$col,
                                  action = Q_$action,
                                  count  = 1.0)


update_counts <- data.frame(row = numeric(0), col = numeric(0), action = numeric(0), count = numeric(0))

t = 1
deltas <- 0
for(it in seq(1, 10000, by = 1)){
  if(it %% 100 == 0){
    t = t + 10e-3
  }
  if(it %% 2000){
    print('it:', it)
  }
  browser()
  s = c(2,0)
  grid$set_state(s)
  # maximing action from a state
  q_idx_ <- row_matches(s, Q_[, c('row', 'col')], same_cols = F) 
  a = max_dict(Q_[q_idx,], val_col = 'reward')$action
  # taking greedy-epsilon action
  a = random_action(a, eps = 0.5 / t)
  biggest_change <- 0
  while(!grid$game_over()){
    r      = grid$move(a)
    s2     = grid$current_state()
    # maximing action from a state
    q2_idx_ = row_matches(s2, Q_[, c('row', 'col')], same_cols = F)
    a2     = max_dict(Q_[q2_idx_,], val_col = 'reward')$action
    a2     = random_action(a2, eps = 0.5 / t)
    
    # indices
    update_counts_idx = row_matches(cbind(data.frame(s[1], s[2]), a),
                                    update_counts_sa[,1:3], 
                                    same_cols = F)
    q_idx = row_matches(cbind(data.frame(s[1], s[2]), a),
                                    Q_[,1:3], 
                                    same_cols = F)
    q2_idx = row_matches(cbind(data.frame(s2[1], s2[2]), a2),
                        Q_[,1:3], 
                        same_cols = F)
                                    
    alpha = ALPHA / update_counts_sa[update_counts_idx, "count"]
    update_counts_sa[update_counts_idx, "count"] %<>% add(0.005)
    old_qsa = Q_[q_idx, 'reward']
    Q_[q_idx, 'reward'] <- Q_[q_idx, 'reward'] + alpha * (r + GAMMA * Q_[q2_idx, 'reward'] - Q_[q_idx, 'reward'])
    
    # change in Q value
    biggest_change = max(biggest_change, abs(old_qsa - Q_[q_idx, 'reward']))
    # Q updates
    if(!row_matches(s, update_counts[,1:2], same_cols = F)){
      update_counts <- rbind(update_counts, data.frame(row = s[1], col = s[2], count = 1))
    } else{
      update_counts[row_matches(s, update_counts[,1:2], same_cols = F), 'count'] %<>% add(1)
    }
    
    # next state is now current state
    s = s2
    a = a2
      
    
    
  }
  deltas <- c(deltas, biggest_change)
  
  
}







  
