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

V_ <- data.frame(row    = grid$all_states()$row, 
                 col    = grid$all_states()$col,
                 reward = 0)

for(it in 1:ITERATIONS){
  
  states_and_rewards = play_game(grid, policy, start_state = c(2, 0))
  
  
  
  for(t in seq_len(nrow(states_and_rewards) - 1)){
    
    state   <- states_and_rewards[t, c('row', 'col')] 
    state2  <- states_and_rewards[t + 1, c('row', 'col')] 
    reward2 <- states_and_rewards[t + 1, 'reward'] 
    v_idx <- row_matches(state, V_)
    v2_idx <- row_matches(state2, V_)
    V_[v_idx, 'reward'] <- V_[v_idx, 'reward'] +  ALPHA * (reward2 + GAMMA * V_[v2_idx,'reward'] - V_[v_idx, 'reward']) 
    
  }
  
  
  if(it %% 100 == 0) print(it)
  
}
print('Values')
print_values(V_, grid)
print('Policy:')
print_policy(policy, grid)
