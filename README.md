# ClusterICP

The code of ClusterICP, a final project for the course Causal Inference 097400.

Authors: Eldar Abraham & Hadar Sinai.

Project files:

* causal_discovery.R - Functions which use existing causal discovery methods in order to predict the predictors which have a causal relationship with some target variable.

* cluster_comparison.R - Script which runs ClusterICP on synthetic data on a range of clusters.

* data_generation.R - Functions which generate synthetic data for our experiments (inspired by section 7.1 from original ICP paper).

* plotting.R - Script which creates the plots we present in the paper.

* real_data.R - Script which runs cluster ICP on the Predictive Maintenance dataset.

* sota_comparison.R - Script which runs GES, Lingam and ClusterICP on synthetic data.

* split_comparison.R - Script which runs ICP with multiple methods of splitting observational data into environments (one of which being clustering).

* utils.R - Several utility functions.
