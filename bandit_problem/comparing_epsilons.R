
# Bandit class --------------------------------------------------------------------------------


setClass('bandit_eps', slots = c(m = 'numeric', mean = 'numeric', n = 'numeric', pull = 'numeric'), 
         prototype = prototype(m = NA_real_, mean = 0, n = as.integer(0)))
pull <- function(object) 0; setGeneric("pull")
#update <- function(object) 0; setGeneric('update')

setMethod("pull", signature(object = "bandit_eps"), function(object) {
  return(rnorm(1, 0, 1) + object@m)
  
})
setMethod("update", signature(object = "bandit_eps"), function(object, x) {
  n = object@n + 1
  mean = (1-1.0/n)* object@mean + 1/n* x
  
  return(list(n = n, mean = mean))
})
bandit_eps <- function(m){ new('bandit_eps',  m = m)}




# Run experiment ------------------------------------------------------------------------------

run_experiment_eps <- function(m1, m2, m3, eps, n) {
  bandits = list(bandit_eps(m1), bandit_eps(m2), bandit_eps(m3))
  data = rep(NA, n)
  
  
  for (i in seq_len(n)){
    p = runif(1)
      if(p < eps){
        j <- base::sample(1:3, 1, replace=F) 
      } else {
        j <- which.max(sapply(bandits, function(x) x@mean)) # to check
      }
    
    x <- pull(bandits[[j]])
    bandits[[j]]@n <- update(bandits[[j]], x)$n
    print(bandits[[j]]@n)
    bandits[[j]]@mean <- update(bandits[[j]], x)$mean
    data[i] <- x
  }

  lapply(bandits, function(x) print(x@mean))
  cum_avg <- cumsum(data) / ( seq_len(n))
  return(cum_avg)
}
require(ggplot2)
c_1  = run_experiment_eps(1.0, 2.0, 3.0, 0.1, 10000)
c_05 = run_experiment_eps(1.0, 2.0, 3.0, 0.05, 10000)
c_01 = run_experiment_eps(1.0, 2.0, 3.0, 0.01, 10000)
ggplot(data.frame(x=c_1), aes(x = seq_along(x), y = c(0,diff(x)), colour = 'c_1')) + geom_line() +
  geom_line(data = data.frame(x=c_05), aes(x = seq_along(x), y = c(0,diff(x)), colour = 'c_05')) +
  geom_line(data = data.frame(x=c_01), aes(x = seq_along(x), y = c(0,diff(x)), colour = 'c_01'))

ggplot(data.frame(x=c_1), aes(x = seq_along(x), y = x, colour = 'c_1')) + geom_line() +
  geom_line(data = data.frame(x=c_05), aes(x = seq_along(x), y = x, colour = 'c_05')) +
  geom_line(data = data.frame(x=c_01), aes(x = seq_along(x), y = x, colour = 'c_01')) + 
  scale_x_log10() + labs(y = 'sample_mean', x = 'n-th bandit choice')
