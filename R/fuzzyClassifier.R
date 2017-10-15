fitFuzzyClassifier <- function(X, y, nClusters) {
  cMeansModel <- cmeans(X, nClusters)
  clustersStdDevs <- calcStdDevs(X, cMeansModel$membership)
  clustersLabels <- assignClustersLabels(y, cMeansModel$membership)
  
  list(
    clustersCenters = cMeansModel$centers,
    clustersStdDevs = clustersStdDevs,
    clustersLabels = clustersLabels
  )
}


calcStdDevs <- function(X, membershipMatrix) {
  nSamples <- nrow(X)
  nFeatures <- ncol(X)
  nClusters <- ncol(membershipMatrix)
  stdDevs <- c()
  
  for (cluster in 1:nClusters) {
    currentClusterMembership <- rep(membershipMatrix[, cluster], nFeatures)
    currentClusterMembership <- matrix(currentClusterMembership, nrow=nSamples, ncol=nFeatures)
    weightedSamples <- X * currentClusterMembership
    stdDevs <- rbind(stdDevs, apply(weightedSamples, 2, sd))
  }
  
  stdDevs
}


assignClustersLabels <- function(y, membershipMatrix) {
  labels <- sort(unique(y))
  membershipSumPerClass <- c()
  for (label in labels) {
    mask <- which(y == label)
    membershipSumPerClass <- cbind(membershipSumPerClass, apply(membershipMatrix[mask, ], 2, sum))
  }
  apply(membershipSumPerClass, 1, which.max)
}


predictFuzzyClassifier <- function(model, X) {
  clustersCenters <- model$clustersCenters
  clustersStdDevs <- model$clustersStdDevs  
  
  nCluster <- 4
  classActivations <- list()
  activations <- c()
  
  # Calc activations
  for (cluster in 1:nCluster) {
    membershipGrades <- c()
    for (feature in 1:ncol(X)) {
      mu <- clustersCenters[cluster, feature]
      sigma <- clustersStdDevs[cluster, feature]
      membershipGrades <- cbind(membershipGrades, calcPdf(X[, feature], mu, sigma))
    }
    currentClusterActivations <- apply(membershipGrades, 1, sum)
    activations <- cbind(activations, currentClusterActivations)
  }
  
  labelsProbabilisticSum <- c()
  labels <- sort(unique(model$clustersLabels))
  for (label in labels) {
    mask <- model$clustersLabels == label
    currentLabelActivations <- activations[, mask]
    currentLabelProbabilisticSum <- apply(currentLabelActivations, 1, sum) # TODO - trocar por snorma
    labelsProbabilisticSum <- cbind(labelsProbabilisticSum, currentLabelProbabilisticSum)
  }
  
  apply(labelsProbabilisticSum, 1, which.max)
}


calcPdf <- function(x, mean, stdDev) {
  (1 / (sqrt(2 * pi * stdDev * stdDev))) * exp(-0.5 * ((x - mean) / (stdDev)) ^ 2)
}
