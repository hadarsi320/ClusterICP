library(InvariantCausalPrediction)
source('causal_discovery.R')
source('data_generation.R')
source('utils.R')

set.seed(42)
save_file <- format(Sys.time(), 'results/sota/%Y-%m-%d-%H-%M-%S.RData')
methods = c('GES',  'Lingam', 'clustering', 'smart_clustering')
cluster_range = seq.int(2, 10, by = 1)
n_settings = 100
n_repeats = 50


init_list <- to_list(for (method in methods) 0)
names(init_list) <- to_list(for (method in methods) method)
k <- length(init_list)

success_list <- init_list
fwer_list <- init_list

for (i in 1:n_settings){
  cat('Setting', i, '\n')
  
  output <- sample_data(repeats = n_repeats)
  interventional_nodes <- output$interventional_nodes
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
    
    prediction <- run_GES(X, Y)
    success$GES <-
      append(success$GES, get_success(prediction, ground_truth))
    fwer$GES <- 
      append(fwer$GES, get_fwer(prediction, ground_truth))
    
    prediction <- run_Lingam(X, Y)
    success$Lingam <-
      append(success$Lingam, get_success(prediction, ground_truth))
    fwer$Lingam <- 
      append(fwer$Lingam, get_fwer(prediction, ground_truth))
    
    clusters <- kmeans(X, 2)$cluster
    prediction = run_ICP(X, Y, clusters)
    success$clustering <-
      append(success$clustering, get_success(prediction, ground_truth))
    fwer$clustering <- 
      append(fwer$clustering, get_fwer(prediction, ground_truth))
    
    clusters <- kmeans(X_nd, 2)$cluster
    prediction = run_ICP(X_nd, Y, clusters)
    success$smart_clustering <- 
      append(success$smart_clustering, get_success(prediction, ground_truth))
    fwer$smart_clustering <- 
      append(fwer$smart_clustering, get_fwer(prediction, ground_truth))
  }
  
  for (j in 1:k){
    success_list[[j]][i] <- mean(success[[j]])
    fwer_list[[j]][i] <- mean(fwer[[j]])
  }
  
  save(success_list, fwer_list, file = save_file)
}

