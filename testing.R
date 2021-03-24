library(pcalg)
source('data_generation.R')

# data(gmG)
# score <- new("GaussL0penObsScore", gmG8$x)
# ges.fit <- ges(score)
# if (require(Rgraphviz)) {
#   par(mfrow=c(1,2))
#   plot(ges.fit$essgraph, main = "Estimated CPDAG")
#   plot(gmG8$g, main = "True DAG")
# } 

data(gmInt)
score <- new("GaussL0penIntScore", gmInt$x, gmInt$targets, gmInt$target.index)
# gies.fit <- gies(score)
# par(mfrow=c(1,2))
# plot(gies.fit$essgraph, main = "Estimated ess. graph")
# plot(gmInt$g, main = "True DAG")

output <- sample_data()
interventional_nodes <- output$interventional_nodes
data <- output$data
E <- output$E
target <- 5

interventions <- list(integer(0), interventional_nodes)
score <- new("GaussL0penIntScore", data, interventions, E)
# gies.fit <- gies(score, targets = list(integer(0), intervention_nodes))
# ges.edges <- ges.fit$repr$.in.edges
# return (ges.edges[[target]])