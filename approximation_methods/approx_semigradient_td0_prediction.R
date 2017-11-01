# https://deeplearningcourses.com/c/artificial-intelligence-reinforcement-learning-in-python
# https://www.udemy.com/artificial-intelligence-reinforcement-learning-in-python

ITERATIONS = 5000
GAMMA      = 0.9
EPS        = 0.1
grid       = negative_grid(step_cost = -0.1)
ALPHA      = 0.1
ALL_POSSIBLE_ACTIONS = c('U', 'D', 'L', 'R')

source('./approximation_methods/model.R')


# Main ----------------------------------------------------------------------------------------

# use the standard grid again (0 for every step) so that we can compare
# to iterative policy evaluation
grid = standard_grid()

# print rewards
print("rewards:")
print_values(grid$rewards, grid)

# state -> action
policy = data.frame(
  row    = c( 2 , 1 , 0 , 0 , 0 , 1 , 2 , 2 , 2 ),
  col    = c( 0 , 0 , 0 , 1 , 2 , 2 , 1 , 2 , 3 ),
  action = c('U','U','R','R','R','R','R','R','U')
)


model = Model$new()
deltas = c()

# repeat until convergence
k = 1.0
for(it in 1:ITERATIONS){
  if(it %% 10 == 0){
    k <- k + 0.01
  }
  if(it %% 500 == 0) print(it)
  alpha = ALPHA / k
  biggest_change = 0
  # generate an episode using pi
  states_and_rewards = play_game(grid, policy)
  
  # the first (s, r) tuple is the state we start in and 0
  # (since we don't get a reward) for simply starting the game
  # the last (s, r) tuple is the terminal state and the final reward
  # the value for the terminal state is by definition 0, so we don't
  # care about updating it.
  for(t in 1:(length(states_and_rewards)-1)){
    
    s  <- states_and_rewards[t, ]
    s2 <- states_and_rewards[t + 1, ]
    r  <- s2$reward
    # we will update V(s) AS we experience the episode
    old_theta = model$theta
    if(grid$is_terminal(s2)){
      target = r
    }else{
      target = r + GAMMA * model$predict(s2)
    }
    model$theta = model$theta + alpha * c(target - model$predict(s)) * (model$grad(s))
    biggest_change = max(biggest_change, sum(abs(old_theta - model$theta)))
    
  }
  deltas = c(deltas, biggest_change)
}

ggplot(data = NULL, aes(x = seq_along(deltas), y = deltas)) + geom_line()


# obtain predicted values
V <- data.frame(row = numeric(0), col = numeric(0), reward = numeric(0))
states = grid$all_states()
for(i in 1:nrow(states)){
  s <- states[i,]
  grid_actions_idx <- row_matches(s, grid$actions)
  if(grid_actions_idx){
    V <- rbind(V, data.frame(s, theta = model$predict(s)))
  } else {
    # terminal state or state we can't otherwise get to
    V <- rbind(V, data.frame(s, theta = 0))
  }
}


print("values:")
print_values(V, grid)
print("policy:")
print_policy(policy, grid)
