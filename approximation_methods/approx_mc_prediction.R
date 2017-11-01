
LEARNING_RATE = 0.001
ITERATIONS    = 1000
grid = standard_grid()

policy = data.frame(
  row    = c( 2 , 1 , 0 , 0 , 0 , 1 , 2 , 2 , 2 ),
  col    = c( 0 , 0 , 0 , 1 , 2 , 2 , 1 , 2 , 3 ),
  action = c('U','U','R','R','R','R','R','R','U')
)


# initialize theta
# our model is V_hat = theta.dot(x)
# where x = [row, col, row*col, 1] - 1 for bias term
theta = rnorm(4) / 2

# feature function
s2x <- function(s){
  s <- c(s[[1]], s[[2]])
  matrix(c(s[1] - 1, s[2] - 1.5, s[1]*s[2] - 3, 1))
}

# repeat until convergence
deltas = c()
t = 1.0
for(it in 1:ITERATIONS){
  if(it %% 100 == 0){
    t <- t + 0.01
    print(glue('it: {it}'))
  }
  alpha = LEARNING_RATE / t
  # generate an episode using pi
  biggest_change = 0
  states_and_returns = play_game(grid, policy) 
  seen_states <- data.frame(row = numeric(0), col = numeric(0))
  for(i in 1:nrow(states_and_returns)){
      x <- states_and_returns[i, ]
      
      # check if we have already seen s
      seen_states_idx <- row_matches(x, seen_states)
      if(!seen_states_idx){
        old_theta <- theta
        s <- x[,1:2]
        x_ <- s2x(s)
        
        V_hat <- theta%*%x_
        # grad(V_hat) wrt theta = x
        theta <- c(theta + alpha * c(x$reward - V_hat) * x_)
        biggest_change = max(biggest_change, sum(abs(old_theta - theta)))
        seen_states <- rbind(seen_states, s)
      }
  } 
  deltas <- c(deltas, biggest_change)
}
  
  

ggplot(data = NULL, aes(x = seq_along(deltas), y = deltas)) + geom_line()

# obtain predicted values
V <- data.frame(row = numeric(0), col = numeric(0), reward = numeric(0))
states = grid$all_states()
for(i in 1:nrow(states)){
  s <- states[i,]
  grid_actions_idx <- row_matches(s, grid$actions)
  if(grid_actions_idx){
    V <- rbind(V, data.frame(s, theta = c(theta %*% s2x(s))))
  } else {
  # terminal state or state we can't otherwise get to
    V <- rbind(V, data.frame(s, theta = 0))
  }
}


p_load(forecast)

print("values:")
print_values(V, grid)
print("policy:")
print_policy(policy, grid)
