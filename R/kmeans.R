library(foreach)
library(doParallel)


fitKmeans <- function(X, K, membershipAsVector = FALSE) {
  # Finds cluster centers on a given dataset.
  #
  # Args:
  #   X: Samples matrix, where each row is a sample and the columns are features
  #   K: Number of clusters to segment the dataset
  #   membershipAsVector: If true, the sample's clusters are returned as a
  #                       vector of classes. Otherwise, a membership matrix
  #                       on a dummy format is returned
  #
  # Returns:
  #   A list with the membership matrix (or vector, membershipAsVector is set
  #   as true) and a matrix with each cluster's centroid
  registerDoParallel()

  centroids <- generateRandomCenters(X, K)
  U <- updateMembershipMatrix(X, centroids)

  while (TRUE) {
    centroids <- calcCentroids(X, U)
    newU <- updateMembershipMatrix(X, centroids)
    if (all(newU == U)) {
      if (all(apply(U, 2, sum) >= 2)) {
        break
      } else {
        centroids <- generateRandomCenters(X, K)
        newU <- updateMembershipMatrix(X, centroids)
      }
    }
    U <- newU
  }

  if (membershipAsVector) U <- unDummify(U)

  list(
    membership = U,
    centroids = centroids
  )
}


generateRandomCenters <- function(X, K) {
  # Randomly selects K samples to be the initial clusters' centroids.
  #
  # Args:
  #   X: Samples matrix, where each row is a sample and the columns are features
  #   K: Number of clusters to segment the dataset
  #
  # Returns:
  #   The clusters' centroids matrix
  return(X[sample(nrow(X), K), ])
}


updateMembershipMatrix <- function(X, centroids) {
  # Updates the membership matrix, based on the centroids.
  #
  # Args:
  #   X: Samples matrix
  #   centroids: Centroids matrix
  #
  # Returns:
  #   The updated membership matrix
  K <- nrow(centroids)
  distanceMatrix <- foreach (i = 1:K, .combine=cbind) %dopar% {
    centroid <- centroids[i, ]
    calcEuclidianDistances(X, centroid)
  }
  samplesClusterIndex <- apply(distanceMatrix, 1, which.min)
  dummify(samplesClusterIndex, K)
}


calcEuclidianDistances <- function(X, centroid) {
  # Calculates the samples' Euclidian distance from a given cluster centroid.
  #
  # Args:
  #   X: Sample matrix
  #   centroid: A cluster centriod
  #
  # Returns:
  #   A vector with the Euclidian distance between the samples and the 
  #   given centroid
  distances <- t(apply(X, 1, function(row) row - centroid))
  squaredDistances <- distances ^ 2
  euclidianDistances <- sqrt(apply(squaredDistances, 1, sum))
}


calcCentroids <- function(X, U) {
  # Calculates the clusters' centroids based on the membership matrix.
  #
  # Args:
  #  X: Samples matrix
  #  U: Membership matrix
  #
  # Return:
  #   The clusters' centroids matrix
  K = ncol(U)
  nFeatures <- ncol(X)
  MIN_NUM_SAMPLES_PER_CLUSTER <- 3

  centroids <- c()
  for (j in 1:K) {
    mask <- which(U[, j] == 1)
    currentClusterSamples <- X[mask, ]
    if (length(currentClusterSamples) < MIN_NUM_SAMPLES_PER_CLUSTER) {
      return(generateRandomCenters(X, K))
    } else {
      centroid <- apply(as.matrix(currentClusterSamples), 2, mean)
    }
    centroids <- rbind(centroids, centroid)
  }
  centroids
}


dummify <- function(samplesClusterIndex, K) {
  # Creates a dummy representation of a vector of cluster labels.
  #
  # Args:
  #   samplesClusterIndex: A vector with a cluster label for each dataset sample
  #   K: the number of clusters
  #
  # Returns:
  #   A dummy representation of samplesClusterIndex
  nSamples <- length(samplesClusterIndex)
  foreach (i = 1:nSamples, .combine=rbind) %dopar% {
    dummyRow <- matrix(0, nrow = 1, ncol = K)
    dummyRow[samplesClusterIndex[i]] <- 1
    dummyRow
  }
}


unDummify <- function(U) {
  # Transform a dummy matrix into a vector of labels
  #
  # Args:
  #   U: A membership matrix
  #
  # Returns:
  #   A vector with cluster labels
  as.array(apply(U, 1, which.max))
}