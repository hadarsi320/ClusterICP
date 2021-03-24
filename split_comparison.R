library(InvariantCausalPrediction)
source('causal_discovery.R')
source('data_generation.R')
source('utils.R')

set.seed(10)
save_file <- format(Sys.time(), 'results/split/%Y-%m-%d-%H-%M-%S.RData')
n_settings = 100
n_repeats = 50

init_list <- list('oracle' = 0, 'baseline' = 0, 'clustering' = 0, 'smart_clustering' = 0)
success_list <- init_list
fwer_list <- init_list


for (i in 1:n_settings){
  cat('Setting', i, '\n')
  
  output <- sample_data(repeats = n_repeats)
  stationary_nodes <- output$stationary_nodes
  connections <- output$obs_conn
  datasets <- output$data
  E = output$E
  p = output$p

  success <- init_list
  fwer <- init_list
  for (j in 1:n_repeats){
    target <- sample(stationary_nodes, 1)
    ground_truth <- get_parents(connections, target) # The actual parents of the target
    X <- datasets[[j]][, -target]
    Y <- datasets[[j]][, target]
    non_descendents <- get_non_descendents(connections, target)
    X_nd <- X[, non_descendents]
  
    # oracle
    prediction = run_ICP(X, Y, E)
    success$oracle[j] <- get_success(prediction, ground_truth)
    fwer$oracle[j] <- get_fwer(prediction, ground_truth)
    
    # baseline
    split_predictor <- sample(1:ncol(X), 1)
    cond_split <- X[, split_predictor] > median(X[, split_predictor])
    prediction = run_ICP(X, Y, cond_split)
    success$baseline[j] <- get_success(prediction, ground_truth)
    fwer$baseline[j] <- get_fwer(prediction, ground_truth)
    
    # clustering
    clusters <- kmeans(X, 2)$cluster
    prediction = run_ICP(X, Y, clusters)
    success$clustering[j] <- get_success(prediction, ground_truth)
    fwer$clustering[j] <- get_fwer(prediction, ground_truth)
    
    # smart clustering
    clusters <- kmeans(X_nd, 2)$cluster
    prediction = run_ICP(X_nd, Y, clusters)
    success$smart_clustering[j] <- get_success(prediction, ground_truth)
    fwer$smart_clustering[j] <- get_fwer(prediction, ground_truth)
  }
  
  for (j in 1:length(success)){
    success_list[[j]][i] <- mean(success[[j]])
    fwer_list[[j]][i] <- mean(fwer[[j]])
  }
  
  save(success_list, fwer_list, file = save_file)
}

