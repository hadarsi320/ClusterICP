library(InvariantCausalPrediction)
source('data_generation.R')

# set.seed(10)
n_settings = 100
n_repeats = 100
success_list <- 0
fwer_list <- 0
for (i in 1:n_settings){
  cat('Setting', i, '\n')
  
  data <- sample_data(repeats = n_repeats)
  X = data$X
  Y = data$Y
  E = data$E
  target_parents <- data$target_parents

  success <- 0
  fwer <- 0
  for (j in 1:n_repeats){
    icp <- ICP(X[[j]], Y[[j]], E, alpha=0.01,
               showAcceptedSets = FALSE, showCompletion = FALSE, stopIfEmpty = TRUE)
    invariant_predictors = to_vec(for (k in 1:ncol(X[[j]])) if (icp$maximinCoefficients[k] != 0) icp$colnames[k])
    fwer[j] <- any((for (predictor in invariant_predictors) !is.element(predictor, target_parents)))
    success[j] <- setequal(invariant_predictors, target_parents)
  }
  success_list[i] <- mean(success)
  fwer_list[i] <- mean(fwer)
}
# plot(icp)

