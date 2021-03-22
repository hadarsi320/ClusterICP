library(InvariantCausalPrediction)
source('data_generation.R')

# set.seed(10)
data <- sample_data()
X = data$X
Y = data$Y
E = data$E
t = data$target
beta = data$obs_conn[, t][-t]
R = Y - X %*% beta
R_obs = R[E == 1]
R_int = R[E == 2]
equal_vars = var.test(R_obs, R_int)$p.value > 0.05
equal_means = t.test(R_obs, R_int)$p.value > 0.05

icp <- ICP(data$X, data$Y, data$E, alpha=0.01,
           showAcceptedSets = FALSE, showCompletion = FALSE, stopIfEmpty = TRUE)
print(icp)
invariant_predictors = to_vec(for (i in 1:ncol(X)) if (icp$maximinCoefficients[i] != 0) icp$colnames[i])
print(invariant_predictors)
# plot(icp)

