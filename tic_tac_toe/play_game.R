play_game <- function(p1, p2, env, draw = F, verbose = F){
  # loops until the game is over
  current_player = NA
  
  while (!env$game_over()){
    # alternate between players
    # p1 always starts first
    if (identical(current_player, p1)) current_player <- p2 else current_player <- p1
        
        # draw the board before the user who wants to see it makes a move
        if(draw){
          if(draw == 1 & identical(current_player, p1))
            env$draw_board()
        }
          
        if(draw == 2 & identical(current_player, p2)) env$draw_board()
        
        # current player makes a move
    
        current_player$take_action(env)
        
        # update state histories
        state = env$get_state()
        p1$update_state_history(state)
        p2$update_state_history(state)
        
        if (draw) env$draw_board()
        
        # do the value function update
        
  }
  if(verbose){
    outcome <- if(!is.na(env$winner)){
      sprintf('The winner is: %s', ifelse(env$winner==-1, 'Player one', 'Player two'))
    } else{
      sprintf('The draw. Noone won.')  
    }
    
    cat(outcome)
  }
  
  p1$update(env)
  p2$update(env)
    
}
