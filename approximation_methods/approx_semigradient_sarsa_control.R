ITERATIONS = 10

# NOTE: if we use the standard grid, there's a good chance we will end up with
# suboptimal policies
# e.g.
# ---------------------------
#   R  |   R  |   R  |      |
# ---------------------------
#   R* |      |   U  |      |
# ---------------------------
#   U  |   R  |   U  |   L  |
# since going R at (1,0) (shown with a *) incurs no cost, it's OK to keep doing that.
# we'll either end up staying in the same spot, or back to the start (2,0), at which
# point we whould then just go back up, or at (0,0), at which point we can continue
# on right.
# instead, let's penalize each movement so the agent will find a shorter route.
#
# grid = standard_grid()
grid = negative_grid(step_cost = -0.1)

# print rewards
print("rewards:")
print_values(grid$rewards, grid)

# no policy initialization, we will derive our policy from most recent Q
# enumerate all (s,a) pairs, each will have its own weight in our "dumb" model
# essentially each weight will be a measure of Q(s,a) itself
states = grid$all_states()

SA2IDX <- data.frame(row = states$row, 
                     col = states$col, 
                     action = rep(ALL_POSSIBLE_ACTIONS, nrow(states))) %>% 
  arrange(row, col) %>% mutate(idx = seq_along(action)) 
                     

# initialize model
model = Model_SARSA$new()

# repeat until convergence
t   = 1.0
t2  = 1.0
deltas = c()

for(it in 1:ITERATIONS){
  if(it %% 100 == 0){
    t  <- t + 0.01
    t2 <- t2+ 0.01
  }
  if(it %% 1000 == 0){
    print(glue("it: {it}"))
  }
  alpha = ALPHA / t2
  
  
  
  # instead of 'generating' an epsiode, we will PLAY
  # an episode within this loop
  s = c(2, 0) # start state
  grid$set_state(s)
  
  # get Q(s) so we can choose the first action
  
  Qs = getQs(model, s)
  
  # the first (s, r) tuple is the state we start in and 0
  # (since we don't get a reward) for simply starting the game
  # the last (s, r) tuple is the terminal state and the final reward
  # the value for the terminal state is by definition 0, so we don't
  # care about updating it.
  a = max_dict(Qs, val_col = 'prediction', coord_col = 'action')[1]
  a = random_action(a, eps = 0.5 / t) # epsilon-greedy
  biggest_change = 0
  
  while(!grid$game_over()){
    
    r  = grid$move(a)
    s2 = grid$current_state()
    s2 = data.frame(row = s2[1], col = s2[2])
    # we need the next action as well since Q(s,a) depends on Q(s',a')
    # if s2 not in policy then it's a terminal state, all Q are 0
    old_theta = model$theta
    if(grid$is_terminal(s2)){
      model$theta <- model$theta +  alpha*(r - model$predict(s, a))*model$grad(s, a)
    } else {
      
      # not terminal
      Qs2 = getQs(model, s2)
      
      a2 = max_dict(Qs2, val_col = 'prediction', coord_col = 'action')[1]
      a2 = random_action(a2, eps=0.5/t) # epsilon-greedy
      
      # we will update Q(s,a) AS we experience the episode
      model$theta  <- model$theta +  alpha*c(r + GAMMA*model$predict(s2, a2) - model$predict(s, a))*model$grad(s, a)
      
      # next state becomes current state
      s = s2
      a = a2
    }
    biggest_change = max(biggest_change, sum(abs(model$theta - old_theta)))
  }
  deltas <- c(deltas, biggest_change)
  
}

ggplot(data = NULL, aes(x = seq_along(deltas), y = deltas)) + geom_line()



# obtain predicted values
V <- data.frame(row = numeric(0), col = numeric(0), value = numeric(0))
Q <- data.frame(row = numeric(0), col = numeric(0), 
                action = character(0), reward = numeric(0))
states = grid$all_states()
policy = data_frame(row = numeric(0), col = numeric(0), action = character(0))
for(i in 1:nrow(states)){
  s <- states[i, ]
  Qs <- getQs(model, s)
  Q %<>% rbind(data_frame(row = s$row, col = s$col, prediction = Qs$prediction))
  max_q <- max_dict(Qs, val_col = 'prediction', coord_col = 'action')
  V %<>% rbind(data_frame(row = s$row, col = s$col, value = max_q$max_value))
  policy %<>% rbind(data_frame(row = s$row, col = s$col, action = max_q$action))
}


print( "values:")
print_values(V, grid)
print("policy:")
print_policy(policy, grid)
