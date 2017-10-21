Bandit <- R6Class('Bandit',
                  public = list(
                    initialize = function(m, upper_limit = 0){ 
                      self$m = m
                      self$mean = upper_limit
                    },
                    m = NA_real_,
                    mean = 0,
                    n = as.integer(0),
                    pull = function(){
                      return(rnorm(1) + self$m)
                    },
                    update = function(x) {
                      self$n = self$n + 1
                      self$mean = (1-1.0/self$n)* self$mean + 1/self$n* x
                      
                    }
                    
                  )
                  
)



# Run experiment ------------------------------------------------------------------------------

run_experiment_oiv <- function(m1, m2, m3, n, upper_limit = 10) {
  bandits = list(Bandit$new(m1, upper_limit), Bandit$new(m2, upper_limit), Bandit$new(m3, upper_limit))
  data = rep(NA, n)
  
  for (i in seq_len(n)){
    
    j <- which.max(sapply(bandits, function(x) x$mean)) # to check
    x <- bandits[[j]]$pull()
    bandits[[j]]$update(x)
    data[i] <- x
  }
  
  lapply(bandits, function(x) print(x$mean))
  cum_avg <- cumsum(data) / ( seq_len(n))
  return(cum_avg)
  
}


c_1  = run_experiment_oiv(1.0, 2.0, 3.0, 10000)
c_05 = run_experiment_oiv(1.0, 2.0, 3.0, 10000)
c_01 = run_experiment_oiv(1.0, 2.0, 3.0, 10000)

ggplot(data.frame(x=c_1), aes(x = seq_along(x), y = x, colour = 'c_1')) + geom_line() +
  geom_line(data = data.frame(x=c_05), aes(x = seq_along(x), y = x, colour = 'c_05')) +
  geom_line(data = data.frame(x=c_01), aes(x = seq_along(x), y = x, colour = 'c_01')) + 
  scale_x_log10() + labs(y = 'sample_mean', x = 'n-th bandit choice')
