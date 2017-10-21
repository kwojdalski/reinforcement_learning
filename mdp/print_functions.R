
# Print values --------------------------------------------------------------------------------

print_values <- function(V, g){
  to_print <- ""
  for( i in seq_len(g$width)-1){
    to_print <- glue("{to_print}\n---------------------------------\n\n")
    for (j in seq_len(g$height)-1){
      v = V[[3]][V$row == i & V$col== j]
      
      if(length(v)==0){
        to_print <- glue("{to_print} {sprintf('  %s  |', NA)}")
      } else if(v >= 0){
        to_print <- glue("{to_print} {sprintf(' %.2f |', v)}")
      } else if(length(v) > 0) {
        to_print <- glue("{to_print} {sprintf('%.2f |', v)}")
      }
      
    }
    to_print <- glue('{to_print}')
  }
  print(to_print)
}


# Print policy --------------------------------------------------------------------------------

print_policy <- function(P, g){
  to_print <- ''
  for (i in seq_len(g$width)-1){
    to_print <- glue("{to_print}\n---------------------------\n\n")
    for (j in seq_len(g$height)-1){
      
      a = P[[3]][P$row == i & P$col == j]
      if(length(a)==0){
        to_print <- glue("{to_print} {sprintf(' %s  |', NA)}")
      }else{
        to_print <- glue('{to_print} {sprintf("  %s  |", a)}')  
      }
      
    }
    
  }
  to_print <- glue("{to_print}\n---------------------------\n\n")
  print(to_print)
}

