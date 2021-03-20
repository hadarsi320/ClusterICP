library(InvariantCausalPrediction)
source('data_generation.R')

set.seed(10)
data <- sample_data()
icp <- ICP(data$X, data$Y, data$E, alpha=0.05,
           showAcceptedSets = TRUE, showCompletion = FALSE)
print(icp)
# plot(icp) 
