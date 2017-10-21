#source('./comparing_epsilons.R')

# Bandit class --------------------------------------------------------------------------------

setClass('bandit', slots = c(m = 'numeric', mean = 'numeric', n = 'numeric', pull = 'numeric'), 
         prototype = prototype(m = NA_real_, mean = 0, n = as.integer(0)))
pull <- function(object) 0; setGeneric("pull")
#update <- function(object) 0; setGeneric('update')

setMethod("pull", signature(object = "bandit"), function(object) {
  return(rnorm(1, 0, 1) + object@m)
  
})
setMethod("update", signature(object = "bandit"), function(object, x) {
  n = object@n + 1
  mean = (1-1.0/n)* object@mean + 1/n* x
  
  return(list(n = n, mean = mean))
})
bandit <- function(m, upper_limit = 10){ new('bandit',  m = m, mean = upper_limit)}
# bandit <- bandit(1, 2)
# x = pull(bandit)
# bandit@n <- update(bandit(1), x)$n
# bandit@mean <- update(bandit(1), x)$mean




# Run experiment ------------------------------------------------------------------------------

run_experiment_oiv <- function(m1, m2, m3, n, upper_limit = 10) {
  bandits = list(bandit(m1, upper_limit), bandit(m2, upper_limit), bandit(m3, upper_limit))
  data = rep(NA, n)
  
  
  for (i in seq_len(n)){
    
    j <- which.max(sapply(bandits, function(x) x@mean)) # to check
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
oiv  = run_experiment_oiv(1.0, 2.0, 3.0, 10000)


ggplot(data.frame(x=c_1), aes(x = seq_along(x), y = x, colour = 'eps')) + geom_line() +
  geom_line(data = data.frame(x=oiv), aes(x = seq_along(x), y = x, colour = 'oiv')) + 
  scale_x_log10() + labs(y = 'sample_mean', x = 'n-th bandit choice')
  