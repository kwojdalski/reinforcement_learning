# https://deeplearningcourses.com/c/artificial-intelligence-reinforcement-learning-in-python
# https://www.udemy.com/artificial-intelligence-reinforcement-learning-in-python

parse_position <- function(start){
  when(
    is.character(start) & length(start) == 1 ~ as.numeric(str_split_fixed(start, fixed(','),2)),
    is.numeric(start) ~ start,
    is.data.frame(start) && all.equal(dim(start), c(1,2)), ~ as.numeric(start),
    FALSE ~ simpleError('Wrong input. Try an array of numbers or characters (length of two)!')
  )
}


Grid <- R6Class("Grid", public = list(
    initialize = function(width, height, start) {
      
      self$width  = width
      self$height = height
      self$i      = parse_position(start)[1]
      self$j      = parse_position(start)[2]
      
    }, width= NULL, height = NULL, i = NULL, j = NULL, rewards = NULL, actions = NULL,
    set_state = function(s, verbose = F){
      
      self$i = parse_position(s)[1]
      self$j = parse_position(s)[2]
    },
    parse_position = function(s){
      parse_position(s)
    },
    set = function(rewards, actions){
      self$rewards = rewards
      self$actions = actions # possible actions
    },
    rewards_update = function(update){
      # update is a data frame of updated values
      plyr::adply(update, 1, function(x){
        reward_index <- self$rewards$row == x$row & x$col == self$rewards$col
        if(any(reward_index)) {
          self$rewards[reward_index, ] <- x
        }else{
          self$rewards %<>% add_row(row = x$row, col = x$col, reward = x$reward)
        }
        
      })
      
      
    },
    current_state = function(){
      
      return(c(self$i, self$j))
    },
    is_terminal = function(s){
      return((!s %in% self$actions))
    },
    move = function(action){
      # avail_actions <- self$actions %>% {.[.$row == self$i & .$col ==self$j, 'avail_actions']} %>% unlist()
      avail_actions_idx <- row_matches(c(self$i, self$j), self$actions[,1:2], same_cols = F)
      avail_actions <- unlist(self$actions[avail_actions_idx, 'avail_actions'])
      if(action %in% avail_actions){
        if(action == 'U'){ 
          self$i <- self$i - 1
        } else if (action == 'D'){
          self$i <- self$i +1
        } else if(action == 'R'){
          self$j <- self$j + 1 
        } else if(action == 'L'){
          self$j <- self$j - 1  
        }
        #check if legal move
        # if not it returns the same values self$i = self$i and self$j = self$j.
      }
      #to_ret <- self$rewards %>% {.$reward[.$row == self$i & .$col == self$j]}
      to_ret_idx <- row_matches(c(self$i, self$j), self$rewards[, c('row', 'col')], same_cols = F)
      to_ret <- self$rewards[to_ret_idx, 'reward']
      to_ret <- if(length(to_ret) == 0) 0 else to_ret # no_
             
      
      return(to_ret)
    },
    undo_move = function(action){
        self$i <- action %>% purrr::when(. == 'U' ~ self$i + 1, 
                                         . == 'D' ~ self$i - 1,
                                                  ~ self$i)
        self$j <- purrr::when(. == 'R' ~ self$j - 1,
                              . == 'L' ~ self$j + 1,
                                       ~ self$j)
        # raise an exception if we arrive somewhere we shouldn't be
        # should never happen
        assertthat::assert_that(self$current_state() %in% self$all_states())
    },
    game_over = function(){
      # returns true if game is over, else false
      # true if we are in a state where no actions are possible
      to_ret <- any(self$i == self$actions$row & self$j == self$actions$col)
      
      return(!to_ret)
    },
    all_states = function(){
      # possibly buggy but simple way to get all states
      # either a position that has possible next actions
      # or a position that yields a reward
      # self$actions
      all_states <- full_join(self$actions[, c(1:2)], self$rewards[,c(1,2)])
      return(all_states)
    },
    keys = function(x){
      
      assert_that("row" %in% colnames(x),
                  "col" %in% colnames(x),
                  msg = 'No keys for the object')
    }
      
  )
)

standard_grid <-function(){
  # define a grid that describes the reward for arriving at each state
  # and possible actions at each state
  # the grid looks like this
  # x means you can't go there
  # s means start position
  # number means reward at that state
  # .  .  .  1
  # .  x  . -1
  # s  .  .  .
  g = Grid$new(3, 4, c(2, 0))
  rewards = data.frame(
    row    = c(0, 1), 
    col    = c(3, 3), 
    reward = c(1,-1)
  )
  actions = data.frame(
    row = c(0, 0, 0, 1, 1, 2, 2, 2, 2),
    col = c(0, 1, 2, 0, 2, 0, 1, 2, 3))
  actions$avail_actions = list(c('D', 'R'), c('L', 'R'), c('L', 'D', 'R'), 
                  c('U', 'D'), c('U', 'D', 'R'), c('U', 'R'), 
                  c('L', 'R'), c('L', 'R', 'U'), c('L', 'U'))
  
  g$set(rewards, actions)
  return(g)
}


negative_grid <- function(step_cost = -0.1){
  # in this game we want to try to minimize the number of moves
  # so we will penalize every move
  g = standard_grid()
  
  g$rewards_update(
    data.frame(row    = c( 0, 0, 0, 1, 1, 2, 2, 2, 2),
               col    = c( 0, 1, 2, 0, 2, 0, 1, 2, 3),
               reward = step_cost)
  )
  return(g)
  
}

#debugonce(negative_grid)
#negative <- negative_grid()

