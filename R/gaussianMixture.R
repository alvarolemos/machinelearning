library(foreach)


fitGaussianMixture <- function(X, y, numClustersPerClass) {
  labels <- sort(unique(y))
  mixtureModel <- list()
  for (label in labels) {
    mask <- which(y == label)
    Xlabel <- X[mask, ]
    K <- numClustersPerClass[[label]]
    labelClusters <- findLabelClusters(Xlabel, K)
    bayesianModel <- fitBayesian(Xlabel, labelClusters)
    mixtureModel[[label]] <- bayesianModel
  }
  return(mixtureModel)
}


findLabelClusters <- function(X, K) {
  kmeansModel <- fitKmeans(X, K, membershipAsVector = TRUE)
  labelClusters <- kmeansModel$membership
  labelClusters
}


predictMixture <- function(model, X) {
  probs <- predictMixtureProbs(model, X)
  preds <- apply(probs, 1, which.max)
  as.vector(preds)
}


predictMixtureProbs <- function(model, X) {
  labels <- 1:length(model)
  mixtureProbs <- c()
  for (label in labels) {
    bayesianModel <- model[[label]]
    bayesianModel <- discardZeroCovMatrixClusters(bayesianModel)
    segmentProbs <- predictProbs(bayesianModel, X)
    mixtureProbs <- cbind(mixtureProbs, apply(segmentProbs, 1, sum))
  }
  mixtureProbs
}


discardZeroCovMatrixClusters <- function(bayesianModel) {
  # TODO - Treat it properly on K-Means algorithm
  clustersToKeep <- list()
  for (i in 1:length(bayesianModel)) {
    if (det(bayesianModel[[i]]$covMatrix) != 0) {
      clustersToKeep[[length(clustersToKeep) + 1]] <- bayesianModel[[i]]
    }
  }
  clustersToKeep
}