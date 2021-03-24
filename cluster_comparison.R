library(InvariantCausalPrediction)
source('causal_discovery.R')
source('data_generation.R')
source('utils.R')

set.seed(5)
save_file <- format(Sys.time(), 'results/num_clusters/%Y-%m-%d-%H-%M-%S.RData')
methods = c('clustering', 'smart_clustering')
cluster_range = seq.int(2, 10, by = 1)
n_settings = 100
n_repeats = 10

init_list <- to_list(for (method in methods) for (i in cluster_range) 0)
names(init_list) <- to_list(for (method in methods) for (i in cluster_range)
  paste(method, i, sep = '_'))
k <- length(init_list)

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
  for (j in 1:n_repeats)
  {
    target <- sample(stationary_nodes, 1)
    ground_truth <- get_parents(connections, target) # The actual parents of the target
    X <- datasets[[j]][, -target]
    Y <- datasets[[j]][, target]
    non_descendents <- get_non_descendents(connections, target)
    X_nd <- X[, non_descendents]
    
    for (num_clusters in cluster_range) 
    {
      name <- paste('clustering', num_clusters, sep='_')
      clusters <- kmeans(X, num_clusters)$cluster
      if (sufficiently_distributed(clusters))
      {
        prediction = run_ICP(X, Y, clusters)
        success[[name]] <- 
          append(success[[name]], get_success(prediction, ground_truth))
        fwer[[name]] <-
          append(fwer[[name]], get_fwer(prediction, ground_truth))
      }
      
      name <- paste('smart_clustering', num_clusters, sep='_')
      clusters <- kmeans(X_nd, num_clusters)$cluster
      if (sufficiently_distributed(clusters))
      {
        prediction = run_ICP(X_nd, Y, clusters)
        success[[name]] <-
          append(success[[name]], get_success(prediction, ground_truth))
        fwer[[name]] <-
          append(fwer[[name]], get_fwer(prediction, ground_truth))
      }
    }
  }
  
  for (j in 1:k){
    success_list[[j]][i] <- mean(success[[j]])
    fwer_list[[j]][i] <- mean(fwer[[j]])
  }
  
  save(success_list, fwer_list, file = save_file)
}

