library(pcalg)

run_ICP <- function(X, Y, E){
  icp <- ICP(X, Y, E, alpha=0.05, showAcceptedSets = FALSE, 
             showCompletion = FALSE, stopIfEmpty = TRUE)
  
  accepted_sets <- icp$acceptedSets
  if (length(accepted_sets) == 0) {return (NULL)}
  
  invariant_predictors <- accepted_sets[[1]]
  for (i in 1:length(accepted_sets)){
    invariant_predictors <- intersect(invariant_predictors, accepted_sets[[i]])
    if (length(invariant_predictors) == 0) {return (NULL)}
  }
  return (invariant_predictors)
}

run_GES <- function(X, Y) {
  data <- cbind(X, Y)
  score <- new("GaussL0penObsScore", data)
  ges.fit <- ges(score)
  ges.edges <- ges.fit$repr$.in.edges
  return (ges.edges[[ncol(data)]])
}


run_Lingam <- function(X, Y) {
  data <- cbind(X, Y)
  connections <- lingam(data)$Bpruned
  y_connections <- connections[nrow(connections),]
  y_parents <- to_vec(for (i in 1:ncol(X)) if (y_connections[i] != 0) i)
  return (y_parents)
}