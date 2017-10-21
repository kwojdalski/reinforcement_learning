Environment <- R6Class('Environment', public = list(
  initialize = function(board){
    self$board = matrix(data = 0, LENGTH, LENGTH)
    self$x = -1 # player one
    self$o = 1 # player two
    self$winner = NA
    self$ended = F
    self$num_states = 3 ^ (LENGTH * LENGTH)
  }, board = NULL, x = NULL, o = NULL, winner = NULL, ended = NULL, num_states = NULL, k = 0, h= 0, v=0, LENGTH =0,
  is_empty = function(i, j){
    self$board[i, j] == 0
  },
  reward = function(sym){
    if(!self$game_over()){
      return(0)
    }
    to_ret <- if(!is.na(self$winner) && self$winner == sym) 1 else 0
    return(to_ret)
  },
  get_state = function(){
    # returns the current state, represented as an int
    # from 0...|S|-1, where S = set of all possible states
    # |S| = 3^(BOARD SIZE), since each cell can have 3 possible values - empty, x, o
    # some states are not possible, e.g. all cells are x, but we ignore that detail
    # this is like finding the integer represented by a base-3 number
    k = 0
    h = 0
    for (i in seq_len(LENGTH)){
      for (j in seq_len(LENGTH)){
        if (self$board[i, j] == 0 ){
          v = 0
        }else if(self$board[i, j] == self$x){
          v = 1
        }else if(self$board[i, j] == self$o){
          v = 2
        }
        h <- h + 3^k * v
        k <- k + 1
      }
    }
    return(h)
  },
  game_over = function(force_recalculate = F) {
    
    if(!force_recalculate & self$ended){
      return(self$ended)
    }
    # check rows TO DO
    for(i in seq_len(LENGTH)){
      for(player in c(self$x, self$o)){
        if(sum(self$board[i,]) == player * LENGTH){
          self$winner <- player
          self$ended  <- TRUE
          return(TRUE)
        }
      }
    }
    # check columns
    for(j in seq_len(LENGTH)){
      for(player in c(self$x, self$o)){
        if(sum(self$board[,i]) == player * LENGTH){
          self$winner = player
          self$ended <- TRUE
          return(TRUE)
        }
      }
    }
    
    # check diagonals
    for(player in c(self$x, self$o)){
      
      # top-left -> bottom-right diagonal
      if (psych::tr(self$board) == player * LENGTH){
        self$winner<- player
        self$ended <- TRUE
        return(TRUE)
      }
      if (psych::tr(t(self$board)) == (player * LENGTH)){
        self$winner <- player
        self$ended <- TRUE
        return(TRUE)
      }
    }
    
    
    
    if(all(self$board != 0)){
      self$winner <-  NA
      self$ended  <-  TRUE
      return(TRUE)
    }
    # GAME IS NOT OVER
    self$winner = NA
    return(FALSE)
    
  },
  is_draw = function(){
    return(self$ended & is.null(self$winner))
  },
  draw_board = function(){
    to_print <- c()
    for(i in seq_len(LENGTH)){
      to_print <- paste0(to_print, "\n-------------", '\n')
      for(j in seq_len(LENGTH)){
        
        to_print_tmp <- if (self$board[i, j] == self$x) "x |" else if(self$board[i, j] == self$o) "o |" else "  |"
        #to_print_tmp <- 'x |'
        to_print <- paste(to_print, to_print_tmp)
      }
      
      
    }
    cat(paste0(to_print, '\n-------------\n\n'))
    
  }
  
))






