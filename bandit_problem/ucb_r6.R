BanditUCB <- R6Class('BanditUCB',public = list(
  initialize = function(m){
    self$m = m
    self$mean = 0
    self$n = 0 
    invisible(self)
  },
  m = 0, mean = 0, n = 0,
  pull = function(){ rnorm(1) + self$m},
  update = function(x){
    self$n = self$n + 1
    self$mean = (1-1/self$n) * self$mean + 1 / self$n * x
  }
  
))

ucb <- function(mean, n , nj) {
  if (nj == 0) return(Inf)
  return(mean + sqrt(2*log(n)/nj))
}
  
run_experiment_ucb <- function(m1, m2, m3, n){
  bandits = list(BanditUCB$new(m1), BanditUCB$new(m2), BanditUCB$new(m3))
  
  data = rep(NA, n)
  for (i in seq_len(n)){
    j <- which.max(sapply(bandits, function(x) ucb(x$mean, i + 1, x$n))) # to check
  
    x <- bandits[[j]]$pull()
    bandits[[j]]$update(x)
    data[i] = x
  }  
  
  # for the plot
  
  cum_avg <- cumsum(data) / ( seq_len(n))
  
  return(cum_avg)
}
