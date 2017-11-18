LENGTH  <- 3


Agent = R6Class(
    'Agent',
    public = list(
      initialize = function(eps = 0.1 , alpha = 0.5){
        self$eps = eps
        self$verbose = FALSE
        self$state_history = c()
        self$V = 0
        self$alpha = alpha
      },
      eps = NULL, verbose = NULL , state_history = NULL, V = NULL, sym = NULL, alpha = NULL,
      setV = function(V) {
        
        self$V      <- data.frame(index = unlist(lapply(V, `[[`, 1)),
                                  value = unlist(lapply(V, `[[`, 2)))
        self$V      <- self$V[!duplicated(self$V[, 1]),]
        
      },
      set_symbol = function(sym){
        self$sym <- sym
      },
      set_verbose = function(v){
        self$verbose <- v
      },
      reset_history = function(){
        self$state_history = c()
      },
      take_action = function(env){
        
        r = runif(1)
        best_state = NULL
        if(r < self$eps){
          if(self$verbose) print('Taking a random action')
          
          possible_moves <- list()
          
          for(i in seq_len(LENGTH)) {
            for (j in seq_len(LENGTH)) {
              if(env$is_empty(i, j)) {
                
                possible_moves[[length(possible_moves) + 1]] <- c(i, j)
              }
            }
          }
          next_move <- unlist(possible_moves[[base::sample(seq_along(possible_moves), 1, F)]])
          
        } else {
          # choose the best action based on current values of states
          # loop through all possible moves, get their values
          # keep track of the best value
          pos2value = matrix(0, LENGTH, LENGTH ) # for debugging
          next_move = NULL
          best_value = -1
          for(i in seq_len(LENGTH)) {
            for (j in seq_len(LENGTH)) {
              if(env$is_empty(i, j)) {
                # VALUE CHECKING BEFORE WE MAKE ANY MOVES
                env$board[i, j] <- self$sym
                state           <- env$get_state()
                env$board[i, j] <- 0
                pos2value[i, j] <- self$V[self$V[, 1] == state, 2][1]
                if(pos2value[i, j] > best_value){
                  best_value = pos2value[i, j]
                  best_state = state
                  next_move  = c(i, j)
                }
              }
              
            }
          }
          
        }
          
          
          if(self$verbose){ 
            print('Taking a greedy action')
            to_print <- c()
            for(i in seq_len(LENGTH)){
              to_print <- paste0(to_print, "\n------------------", '\n')
              for(j in seq_len(LENGTH)){
                if(env$is_empty(i, j)  & r > self$eps){
                  to_print_tmp <- sprintf("%.2f|", pos2value[i,j])
                }else{
                  to_print_tmp <- if (env$board[i, j] == env$x) "  x |" else if(env$board[i, j] == env$o) "  o |" else "     |"
                }
                
                to_print <- paste(to_print, to_print_tmp)
              }
            }
           cat(paste0(to_print, '\n')) 
          }  
          # make the move
          env$board[next_move[1], next_move[2]] = self$sym 
          
      },
      update_state_history = function(s){ 
        # cannot put this in take_action, because take_action only happens
        # once every other iteration for each player
        # state history needs to be updated every iteration
        # s = env.get_state() # don't want to do this twice so pass it in
        self$state_history <- c(self$state_history, s)
      },
      update = function(env){
        reward <- env$reward(self$sym)
        target <- reward
        for (prev in rev(self$state_history)){
          value = self$V$value[self$V$index==prev] + self$alpha * (target - self$V$value[self$V$index==prev])
          self$V$value[self$V$index==prev] = value
          target = value
        }
        self$reset_history()
      }
    )
)


