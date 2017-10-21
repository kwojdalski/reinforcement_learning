options(scipen = 999)

p1 <- Agent$new()
p2 <- Agent$new()

# set up state-value data frames
Vx = initialV_x(env_game, state_winner_triples)
p1$setV(Vx)
Vo = initialV_o(env_game, state_winner_triples)
p2$setV(Vo)

# give each player their symbol
p1$set_symbol(env_game$x)
p2$set_symbol(env_game$o)
p1$set_verbose(T)


play_game(p1, p2, Environment$new(), draw = 1, verbose = T)

plyr::a_ply(seq_len(5000), 1, 
            function(x)  play_game(p1, p2, Environment$new(), draw = 0), 
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



