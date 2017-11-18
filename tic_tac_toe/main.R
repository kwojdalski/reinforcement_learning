options(scipen = 999)
source("./tic_tac_toe/agent.R")
source("./tic_tac_toe/environment.R")
source("./tic_tac_toe/get_state_hash_and_winner.R")
source("./tic_tac_toe/human.R")
source("./tic_tac_toe/initial_V.R")
source("./tic_tac_toe/play_game.R")
           
p1       <- Agent$new()
p2       <- Agent$new()
env_game <- Environment$new()

state_winner_triples <- get_state_hash_and_winner(env_game)

# set up state-value data frames
Vx = initialV_x(env_game, state_winner_triples)
p1$setV(Vx)
Vo = initialV_o(env_game, state_winner_triples)
p2$setV(Vo)

# give each player their symbol
p1$set_symbol(env_game$x)
p2$set_symbol(env_game$o)
p1$set_verbose(F)

play_game(p1, p2, Environment$new(), draw = 1, verbose = F)

plyr::a_ply(seq_len(5000), 1, 
            function(x)  play_game(p1, p2, Environment$new(), draw = 0, verbose = F), 
            .progress= 'text') 

if(against_human){
  env <- Environment$new()
  
  human <- Human$new()
  human$set_symbol(env$o)
  p1$set_verbose(TRUE)
  play_game(p1, human, Environment$new(), draw = T, verbose = T)
  # I made the agent player 1 because I wanted to see if it would
  # select the center as its starting move. If you want the agent
  # to go second you can switch the human and AI.

}



