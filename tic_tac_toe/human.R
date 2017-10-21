Human <- R6Class("Human", public = list(
  initialize = function() {NULL}, 
  sym = NULL,
  set_symbol = function(sym){
    self$sym <- sym
  },
  take_action= function(env){
    while(TRUE){
      move = readline(prompt= "Enter coordinates Row, Col for your next move (e.g. 1,1): ")
      coord = as.integer(stringr::str_split_fixed(move, stringr::fixed(','), 2))
      if(env$is_empty(coord[1], coord[2])){
        env$board[coord[1], coord[2]] = self$sym
        break
      }
    }
  },
  update = function(env){invisible(NULL)},
  update_state_history= function(s){invisible(NULL)}
))
  
                               