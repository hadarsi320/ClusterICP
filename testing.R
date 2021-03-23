library(InvariantCausalPrediction)
source('data_generation.R')


data <- sample_data(repeats = 1)
X = data$X
Y = data$Y
E = data$E
target_parents <- data$target_parents

success <- 0
fwer <- 0

icp <- ICP(X, Y, E, alpha=0.01,
           showAcceptedSets = FALSE, showCompletion = FALSE, stopIfEmpty = TRUE)
print(icp)
invariant_predictors = to_vec(for (k in 1:ncol(X)) if (icp$maximinCoefficients[k] != 0) icp$colnames[k])
fwer <- any((for (predictor in invariant_predictors) !is.element(predictor, target_parents)))
success <- setequal(invariant_predictors, target_parents)