get_state_hash_and_winner = function(env, i = 1, j = 1){
  results = list()
  
  for(v in c(0, env$x,  env$o)){
    env$board[i, j] = v  # if empty board it should already be 0
    if (j == 3){
      # j goes back to 0, increase i, unless i = 2, then we are done
      if (i == 3){
        # the board is full, collect results and return
        #if((env$board %>% {which(.!=0)} %>% length())>3) browser()
        
        state  = env$get_state()
        ended  = env$game_over(force_recalculate=TRUE)
        winner = env$winner
        
        results[[length(results)+1]] <- c(state, winner, ended)
      }else{
        results <- c(results, get_state_hash_and_winner(env, i + 1, 0))
          #results = results get_state_hash_and_winner(env, i + 1, 0)
      }
    }else{
          # increment j, i stays the same
      results <- c(results, get_state_hash_and_winner(env, i, j + 1))
    }
  }
    return(results)
}

# 
# 
# get_state_hash_and_winner = function(env, i = 0, j = 0){
#   results = list()
#   
#   to_ret <- plyr::alply(array(c(0, env$x,  env$o)), 1, .fun = function(v){
#     browser()
#     env$board[i, j] = v  # if empty board it should already be 0
#     if (j == 2){
#       # j goes back to 0, increase i, unless i = 2, then we are done
#       if (i == 2){
#         # the board is full, collect results and return
#         
#         state  = env$get_state()
#         ended  = env$game_over(force_recalculate=TRUE)
#         winner = env$winner
#         results[[length(results)+1]] <- c(state, winner, ended)
#       }else{
#         results[[length(results)+1]] <- get_state_hash_and_winner(env, i + 1, 0)
#         #results = results get_state_hash_and_winner(env, i + 1, 0)
#       }
#     }else{
#       # increment j, i stays the same
#       results[[length(results)+1]] <- get_state_hash_and_winner(env, i, j + 1)
#     }
#     return(results)
#     
#   })
#   return(to_ret)  
# }



