library(purrr)
library(comprehenr)

sample_from_sem <- function(p, n, noise_vars, connections) {
  X <- matrix(rnorm(p*n, sd = noise_vars^0.5), nrow=n)
  for (i in 2:p) {
    X[, i] = X[, i] + X %*% connections[, i]
  }
  # for (i in 1:(p-1)) {
  #   for (j in (i+1):p) {
  #       X[, j] = X[, j] + X[, i] * connections[i, j]
  #     }
  #   }
  return(X)
}


sample_data <- function(p_max=40) {
  # sampling parameters
  n_obs <- rdunif(1, 5) * 100
  n_int <- rdunif(1, 5) * 100
  p <- rdunif(1, 5, p_max)
  k <- rdunif(1, 4)
  
  lb <- rdunif(1, 20) / 10
  ub <- lb + rdunif(1, 20) / 10 
  
  var_min <- rdunif(1, 20) / 10
  var_max <- max(var_min, rdunif(1, 20) / 10)
  
  a_min <- rdunif(1, 40) / 10
  if (runif(1) <= 1/3) {
    a_max <- a_min
  } else {
    a_max <- a_min + rdunif(1, 20) / 10
  }
  
  if (runif(1) <= 2/3) {
    lb_int = lb
    ub_int = ub
  } else {
    int_coefs = rdunif(2, 20) / 10
    lb_int = min(int_coefs)
    ub_int = max(int_coefs)
  }
  
  if (runif(1) <= 1/6) {
    inv_frac = p
  } else {
    inv_frac = rdunif(1, 11, 30) / 10
  }
  
  intervene_connections = runif(1) <= 1/3
  target_variable = rdunif(1, p)
  predictor_variables <- to_vec(for (i in 1:p) if (i != target_variable) i)
  
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
  int_nodes <- sort(sample(predictor_variables, p/inv_frac))
  noise_vars_int = noise_vars
  connections_int = connections
  
  noise_vars_int[int_nodes] =
    noise_vars_int[int_nodes] * runif(length(int_nodes), a_min, a_max)
  if (intervene_connections) {
    for (j in int_nodes) {
      for (i in 1:(j-1)) {
        if (runif(1) < k / (p-1)) {
          sign = sample(range(-1, 1), 1)
          connections[i, j] = sign * runif(1, lb, ub)
        } else {
          connections[i, j] = 0
        }
      }
    }
  }
  
  # sampling data
  data_obs <- sample_from_sem(p, n_obs, noise_vars, connections)
  data_int <- sample_from_sem(p, n_int, noise_vars_int, connections_int)
  data <- rbind(data_obs, data_int)
  colnames(data) <- 1:p
  
  Y <- data[, target_variable]
  data <- data[, -target_variable]
  ExpInd <- c(rep(1, n_obs), rep(2, n_int))
  target_parents <- to_vec(for (i in 1:p) if (connections[i, target_variable] != 0) i)
  return(list('X' = data, 'Y' = Y, 'E' = ExpInd, 'target_parents' = target_parents,
              'obs_conn' = connections, 'int_conn' = connections_int,
              'int_nodes' = int_nodes, 'target' = target_variable))
}