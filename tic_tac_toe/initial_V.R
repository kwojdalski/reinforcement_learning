
initialV_x <- function(env, state_winner_triples){
  # initialize state values as follows
  # if x wins, V(s) = 1
  # if x loses or draw, V(s) = 0
  # otherwise, V(s) = 0.5
  
  V <- plyr::llply(state_winner_triples, function(x){
    if(x[3]==1){ # GAME ENDED 
      if(!is.na(x[2]) && x[2] ==env$x){ # if game won by x player
        v <- 1
      }else{
        v <- 0
      }
    }else{
      v <- 0.5 # if non ended 0.5 reward
    }
    return(c(id = x[1], v= v))
    
  }, .progress = 'text')
  
  return(V)
}
 
# vx <- initialV_x(env_game, state_winner_triples)
# env_game$num_states
        
initialV_o <- function(env, state_winner_triples){
  # this is (almost) the opposite of initial V for player x
  # since everywhere where x wins (1), o loses (0)
  # but a draw is still 0 for o
  V <- rep(env$num_states)
  
  V <- plyr::llply(state_winner_triples, function(x){
    if(x[3]==1){ # GAME ENDED 
      if(!is.na(x[2]) && x[2] == env$o){ # if game won by x player
        v <- 1
      }else{
        v <- 0
      }
    }else{
      v <- 0.5 # if non ended 0.5 reward
    }
    return(c(id = x[1] ,v = v))
    
  }, .progress = 'text')
  V
}
  
  
  