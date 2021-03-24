library(InvariantCausalPrediction)
library(foreign)

par(cex = 2)
dataset <- read.csv('data/ai4i2020.csv', header = TRUE)[, 4:9]
names(dataset) <- c('Air temp', 'Process temp', 'Rot speed', 'Torque', 'Tool wear', 'Machine failure')
X <- data.matrix(dataset[, 1:5])
Y <- factor(dataset[, 6])

for (i in 2:10)
{
  E <- kmeans(X, i)$cluster
  icp = ICP(X, Y, E, showCompletion = FALSE, showAcceptedSets = FALSE)
  if (!icp$modelReject)
  {
    cat(i, 'clusters')
    print(icp)
    plot(icp)
  }
}
