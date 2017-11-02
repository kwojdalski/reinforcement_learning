# NOTE: this is only policy evaluation, not optimization
require(R6)
Model <- R6Class('Model', public = list(
  initialize = function(){
    self$theta = rnorm(4) / 2
  },
  theta =NA, x = NA, s = NA,
  s2x = function(s){
    s <- c(s[[1]], s[[2]])
    ret <- matrix(c(s[1] - 1, s[2] - 1.5, s[1]*s[2] - 3, 1))
    return(ret)
  },
  predict = function(s){
    x = self$s2x(s)
    return (theta %*% x)
  },
  grad = function(s){
    return(self$s2x(s))
  }
))

