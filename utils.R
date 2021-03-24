library(stringi)

get_parents <- function(connections, node){
  parents <- to_vec(for (i in 1:nrow(connections)) if (connections[i, node] != 0) i)
  return(parents)
}


get_fwer <- function(invariant_predictors, target_parents) {
  return (any(to_vec(for (predictor in invariant_predictors) !is.element(predictor, target_parents))))
}


get_success <- function(invariant_predictors, target_parents) {
  return (setequal(invariant_predictors, target_parents))
}


get_non_descendents <- function(connections, target) {
  seen_target = FALSE
  non_descendents <- vector()
  for (i in 1:nrow(connections)) {
    if (seen_target) {
      if (all(to_vec(for (j in 1:(i-1))
        (connections[j, i] == 0 | is.element(j, non_descendents))))) {
        non_descendents <- append(non_descendents, toString(i))
      }
    } else {
      if (i == target) {seen_target = TRUE}
      else {non_descendents <- append(non_descendents, toString(i))}
    }
  }
  return(non_descendents)
}


sufficiently_distributed <- function(clusters) {
  counts <- table(clusters)
  for (i in names(counts))
    if (counts[[i]] <= 2)
      return(FALSE)
  
  return(TRUE)
}


format_names <- function(list, glue = ' ') {
  new_names <- list()
  for (name in names(list)) {
    name <- paste(strsplit(name, '_')[[1]], collapse = glue)
    new_names <- append(new_names, stri_trans_totitle(name))
  }
  return (new_names)
}