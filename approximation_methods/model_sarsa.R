# https://deeplearningcourses.com/c/artificial-intelligence-reinforcement-learning-in-python
# https://www.udemy.com/artificial-intelligence-reinforcement-learning-in-python
# Note: you may need to update your version of future
# sudo pip install -U future

# NOTE: this is only policy evaluation, not optimization
require(R6)
Model_SARSA <- R6Class('Model_SARSA', public = list(
  initialize = function(){
    self$theta = rnorm(25) / sqrt(25)
  },
  theta = NA, x = NA, s = NA,
  predict = function(s, a){
    x = self$sa2x(s, a)
    
    return (self$theta %*% x)
  },
  grad = function(s, a){
    return(self$sa2x(s, a))
  },
  # if we use SA2IDX, a one-hot encoding for every (s,a) pair
  # in reality we wouldn't want to do this b/c we have just
  # as many params as before
  # print "D:", IDX
  # self.theta = np.random.randn(IDX) / np.sqrt(IDX)
  sa2x = function(s, a){
    # NOTE: using just (r, c, r*c, u, d, l, r, 1) is not expressive enough
    s <- c(s[[1]], s[[2]])
    
    c(
      if(a == 'U') s[1] - 1              else 0,
      if(a == 'U') s[1] - 1.5            else 0,
      if(a == 'U') (s[1]*s[1] - 3)/3     else 0,
      if(a == 'U') (s[1]*s[1] - 2)/2     else 0,
      if(a == 'U') (s[1]*s[1] - 4.5)/4.5 else 0,
      if(a == 'U') 1                     else 0,
      if(a == 'D') s[1] - 1              else 0,
      if(a == 'D') s[1] - 1.5            else 0,
      if(a == 'D') (s[1]*s[1] - 3)/3     else 0,
      if(a == 'D') (s[1]*s[1] - 2)/2     else 0,
      if(a == 'D') (s[1]*s[1] - 4.5)/4.5 else 0,
      if(a == 'D') 1                     else 0,
      if(a == 'L') s[1] - 1              else 0,
      if(a == 'L') s[1] - 1.5            else 0,
      if(a == 'L') (s[1]*s[1] - 3)/3     else 0,
      if(a == 'L') (s[1]*s[1] - 2)/2     else 0,
      if(a == 'L') (s[1]*s[1] - 4.5)/4.5 else 0,
      if(a == 'L') 1                     else 0,
      if(a == 'R') s[1] - 1              else 0,
      if(a == 'R') s[1] - 1.5            else 0,
      if(a == 'R') (s[1]*s[1] - 3)/3     else 0,
      if(a == 'R') (s[1]*s[1] - 2)/2     else 0,
      if(a == 'R') (s[1]*s[1] - 4.5)/4.5 else 0,
      if(a == 'R') 1                     else 0,
      1
    )
  }
  )
)




# getQs function ------------------------------------------------------------------------------


getQs = function(model, s){
  # we need Q(s,a) to choose an action
  # i.e. a = argmax[a]{ Q(s,a) }
  Qs = data_frame(action = character(0), prediction = numeric(0))
  for(a in ALL_POSSIBLE_ACTIONS){
    
    q_sa = model$predict(s, a)
    Qs <- rbind(Qs, data_frame(action = a, prediction = as.numeric(q_sa)))
  }
  return(Qs)
}


