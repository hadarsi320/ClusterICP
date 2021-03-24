library(InvariantCausalPrediction)
library(zeallot)
source('data_generation.R')
source('utils.R')

par(cex = 2)

plot_list <- function(list, y_label, x_label, fwer=FALSE) {
  X <- vector()
  Y <- vector()
  colors <- vector()
  
  for (i in 1:length(list)) {
    X <- append(X, rep(i, 5))
    Y <- append(Y, quantile(list[[i]]))
    colors <- append(colors, c('black', 'black', 'red', 'black', 'black'))
    # for (j in 1:length(list[[i]])) {
    #   X <- append(X, i)
    #   Y <- append(Y, list[[i]][j])
    # }
  }
  plot(X, Y, col=colors, pch = 19, ylim = c(0, 1), xaxt = 'n', 
       ylab = y_label, xlab = x_label)
  axis(1, 1:length(list), format_names(list, '\n'))
  if (fwer) {
    abline(h=0.05, col='red', lty=3)
  }
}


plot_cluster_comparison <- function(data_list, label) {
  results <- list('clustering' = list(), 'smart_clustering' = list())
  
  for (name in names(data_list)){
    split_str <-  strsplit(name, '_')[[1]]
    num_clusters <- tail(split_str, n=1)
    method <- paste(head(split_str, -1), collapse = '_')
    results[[method]][num_clusters] <- mean(data_list[[name]])
  }
  plot(names(results$clustering), results$clustering, type = 'b',
       ylim = c(0, 1), ylab = label, xlab = '# Clusters', col = 'blue',
       lwd = 2, pch = 0)
  lines(names(results$smart_clustering), results$smart_clustering,
        col = 'green', type = 'b', lwd = 3, pch = 1)
  if (label == 'FWER')
    abline(h=0.05, col='red', lty=3)
  legend(2, 1, format_names(results), lwd=c(2,3), col=c("blue","green"),
         pch=c(0,1))
}



  # splitting methods
# load('results/split/2021-03-23-18-26-46.RData')
# fwer_list <- fwer_list[c('baseline', 'clustering', 'smart_clustering', 'oracle')]
# success_list <- success_list[c('baseline', 'clustering', 'smart_clustering', 'oracle')]
# plot_list(success_list, 'Success Probability', '')
# plot_list(fwer_list, 'FWER', '', fwer = TRUE)

  # num clusters
# load('results/num_clusters/2021-03-24-00-12-00.RData')
# plot_cluster_comparison(success_list, 'Success Probability')
# plot_cluster_comparison(fwer_list, 'FWER')

  # sota
load('results/sota/2021-03-24-16-05-25.RData')
plot_list(success_list, 'Success Probability', '')
plot_list(fwer_list, 'FWER', '', fwer = TRUE)

