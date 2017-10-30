source('./monte_carlo/util.R')

ITERATIONS = 500
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
  
  state_idx <- row_matches(states[s,], grid$all_states())
  
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
  
}
Q_$reward <- 0


update_counts_sa    <- data.frame(row    = Q_$row,
                                  col    = Q_$col,
                                  action = Q_$action,
                                  count  = 1.0)


update_counts <- data.frame(row = numeric(0), col = numeric(0), action = numeric(0), count = numeric(0))

t      <- 1
deltas <- 0
for(it in 1:ITERATIONS){
  
  if(it %% 100 == 0){
    t = t + 10e-3
  }
  if(it %% 100 == 0){
    print(glue('it: {it}'))
  }
  
  
  s = c(2,0)
  grid$set_state(s)
  # maximing action from a state
  q_idx_ <- row_matches(s, Q_[, c('row', 'col')], same_cols = F) 
  a = max_dict(Q_[q_idx,], val_col = 'reward')$action
  # taking greedy-epsilon action
  
  biggest_change <- 0
  while(!grid$game_over()){
    a      = random_action(a, eps = 0.5 / t)
    r      = grid$move(a)
    s2     = grid$current_state()
    # maximizing action from a state
    q2_idx_ = row_matches(s2, Q_[, c('row', 'col')], same_cols = F)
    # indices
    update_counts_idx = row_matches(cbind(data.frame(s[1], s[2]), a),
                                    update_counts_sa[,1:3], 
                                    same_cols = F)
    q_idx = row_matches(cbind(data.frame(s[1], s[2]), a),
                        Q_[,1:3], 
                        same_cols = F)
    q2_idx = row_matches(cbind(data.frame(s2[1], s2[2])),
                         Q_[,c('row', 'col')], 
                         same_cols = F)
    
    alpha = ALPHA / update_counts_sa[update_counts_idx, "count"]
    update_counts_sa[update_counts_idx, "count"] %<>% add(0.005)
    old_qsa = Q_[q_idx, 'reward']
    tmp <- max_dict(Q_[q2_idx,], val_col = 'reward', group_by = c('row', 'col'), val_col_ = reward)
    max_q_s2_a2 <- tmp$reward[1]# - reward
    a2          <- tmp$action[1]
    Q_[q_idx, 'reward'] <- Q_[q_idx, 'reward'] + alpha * (r + GAMMA * max_q_s2_a2 - Q_[q_idx, 'reward'])
    
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

ggplot(data = NULL, aes(x = seq_along(deltas), y = deltas)) + geom_line()

policy = max_dict(Q_, val_col = 'reward', group_by = c('row', 'col'), val_col_ = reward)[, -4] # - reward
policy <- policy[row_matches(grid$actions, policy), ] 
V_ <- max_dict(Q_, val_col = 'reward',group_by = c('row', 'col'), val_col_ = reward)[, -3] # - action
V_ <- V_[row_matches(grid$actions, V_), ] 



### what is the fraction of time we spend on updating each part of Q
total <- sum(update_counts)
update_counts$count <- update_counts$count / total
print_values(update_counts, grid)
print_policy(policy,        grid)
print_values(V_,            grid)

