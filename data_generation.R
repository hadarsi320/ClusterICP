library(purrr)
library(comprehenr)

sample_from_sem <- function(p, n, noise_vars, connections) {
  X <- matrix(rnorm(p*n), nrow=n)
  for (i in 2:p) {
    X[, i] = X[, i] * noise_vars[i]^0.5 + X %*% connections[, i]
  }
  # for (i in 1:(p-1)) {
  #   for (j in (i+1):p) {
  #       X[, j] = X[, j] + X[, i] * connections[i, j]
  #     }
  #   }
  return(X)
}


sample_data <- function(repeats=1) {
  # sampling parameters
  n_obs <- rdunif(1, 5) * 100  # num sample in observational data
  n_int <- rdunif(1, 5) * 100  # num sample in interventional data
  p <- rdunif(1, 5, 40)  # num nodes
  k <- rdunif(1, 1, 4)  # average degree

  lb <- rdunif(1, 20) / 10  # lower bound for coefficients
  ub <- lb + rdunif(1, 20) / 10 # upper bound for coefficients
  
  var_min <- rdunif(1, 20) / 10 # lower bound for noise variance
  var_max <- max(var_min, rdunif(1, 20) / 10)  # upper bound for noise variance
  
  if (runif(1) <= 1/3) {
    a_dif <- 0
  } else {
    a_dif <- rdunif(1, 20) / 10
  }
  a_min <- rdunif(1, 40) / 10  # lower bound for noise scaling intervention
  a_max <- a_min + a_dif  # upper bound for noise scaling intervention
  
  int_coefs = rdunif(2, 20) / 10
  lb_int = min(int_coefs)  # lower bound for interventional coefficients
  ub_int = max(int_coefs)  # upper bound for interventional coefficients
  intervene_connections = runif(1) <= 1/3  # whether to intervene on the coefficients or not
  
  
  if (runif(1) <= 1/6) {
    inv_frac = p  # the inverse fraction of coefficients to intervene on
  } else {
    inv_frac = rdunif(1, 11, 30) / 10
  }
  
  target = rdunif(1, p)
  predictors <- to_vec(for (i in 1:p) if (i != target) i)
  
  # observational sem
  noise_vars <- runif(p, var_min, var_max)
  connections = matrix(0, nrow=p, ncol=p)
  for (i in 1:(p-1)) {
    for (j in (i+1):p) {
      if (runif(1) < k / (p-1)) {
        sign = sample(range(-1, 1), 1)
        connections[i, j] = sign * runif(1, lb, ub)
        }
    }
  }
  
  # interventional sem
  int_nodes <- sort(sample(predictors, p/inv_frac))
  noise_vars_int = noise_vars
  connections_int = connections
  
  noise_vars_int[int_nodes] =
    noise_vars_int[int_nodes] * runif(length(int_nodes), a_min, a_max)
  if (intervene_connections) {
    for (j in int_nodes) {
      for (i in 1:(j-1)) {
        if (runif(1) < k / (p-1)) {
          sign = sample(c(-1, 1), 1)
          connections[i, j] = sign * runif(1, lb, ub)
        } else {
          connections[i, j] = 0
        }
      }
    }
  }
  
  target_parents <- to_vec(for (i in 1:p) if (connections[i, target] != 0) i)
  target_parent_weights <- 
    to_vec(for (i in 1:p) if (connections[i, target] != 0) connections[i, target])
  
  # sampling data
  if (repeats == 1) {
    data_obs <- sample_from_sem(p, n_obs, noise_vars, connections)
    data_int <- sample_from_sem(p, n_int, noise_vars_int, connections_int)
    data <- rbind(data_obs, data_int)
    colnames(data) <- 1:p
    
    Y <- data[, target]
    X <- data[, -target]
    
  } else {
    Y <- list()
    X <- list()
    for (i in 1:repeats) {
      data_obs <- sample_from_sem(p, n_obs, noise_vars, connections)
      data_int <- sample_from_sem(p, n_int, noise_vars_int, connections_int)
      data <- rbind(data_obs, data_int)
      colnames(data) <- 1:p
      
      Y[[i]] <- data[, target]
      X[[i]] <- data[, -target]
    }
  }
  ExpInd <- c(rep(1, n_obs), rep(2, n_int))
  
  return(list('X' = X, 'Y' = Y, 'E' = ExpInd, 'target' = target, 
              'target_parents' = target_parents, 'target_parent_weights' = target_parent_weights,
              'obs_conn' = connections,  'int_conn' = connections_int,
              'obs_noise_vars' = noise_vars, 'int_noise_vars' = noise_vars_int,
              'int_nodes' = int_nodes))
}