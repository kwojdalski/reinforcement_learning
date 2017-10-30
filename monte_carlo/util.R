row_matches <- function(x, table, nomatch = FALSE, cols_to_match = c('row', 'col'), same_cols = T){
  if(same_cols){ assert_that(all(cols_to_match %in% colnames(x)),
                             all(cols_to_match %in% colnames(table)), 
                             msg = glue('No proper cols in x / table. 
                                        You requested column names: {paste0(cols_to_match, collapse =", ")}
                                        Column names in x are: {colnames(x)}
                                        Column names in table are: {colnames(table)}'))
    x <- x[, cols_to_match]
    table <- table[, cols_to_match]
  }
                                        
  
  if (is.matrix(table)) 
    table <- as.data.frame(table)
  if (is.null(dim(x))) 
    x <- as.data.frame(matrix(x, nrow = 1))
  cx <- do.call("paste", c(x[, , drop = FALSE], sep = "\r"))
  ct <- do.call("paste", c(table[, , drop = FALSE], sep = "\r"))
  to_ret <- which(cx == ct)
  to_ret %<>% when(length(.) > 0  ~ .,
                   length(.) == 0 ~ nomatch)
  
  return(to_ret)
}



random_action <- function(a, eps = 0.5, possible_actions = ALL_POSSIBLE_ACTIONS){
  # choose given a with probability 0.5
  # choose some other a' != a with probability 0.5/3
  p = runif(1)
  if(p < (1-eps)){
    return(a)
  }else{
    possible_actions <- possible_actions[possible_actions != a]
    return(base::sample(possible_actions, 1))
  }
}

