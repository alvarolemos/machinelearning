fitKnn <- function(X, y, K) {
  # Fit a K-Nearest Neighbors model.
  #
  # Args:
  #   X: Training samples
  #   y: Training labels
  #   K: Number of nearest neighbors
  #
  # Returns:
  #   A KNN model params, which basically is the samples, their labels
  #   and the number of nearest neighbors.
  list(
    trainingSamples = X,
    trainingLabels = y,
    K = K
  )
}


predictKnn <- function(model, X) {
  # Make predictions with a K-Nearest Neighbors model.
  #
  # Args:
  #   model: A KNN model
  #   X: Samples that will be used to make predictions
  #
  # Returns:
  #   Predicted labels for the given samples.
  predictions <- c()

  for (row in 1:nrow(X)) {
    x <- X[row, ]
    nearestNeighborsIndexes <- findKNearestNeighbors(model, x)
    prediction <- predictOneSample(model, nearestNeighborsIndexes)
    predictions <- c(predictions, prediction)
  }

  predictions
}


findKNearestNeighbors <- function(model, x) {
  # Find the K nearest neighbors to the given sample.
  #
  # Args:
  #   model: A KNN model
  #   x: Sample whose the nearest neighbors we'll be found
  #
  # Returns:
  #   Nearest neighbors of the given sample.
  distMatrix <- as.matrix(dist(rbind(x, model$trainingSamples), method = 'euclidian'))
  neighbors <- distMatrix[1, 2:nrow(model$trainingSamples)]
  nearestNeighborsIndexes <- order(neighbors)[1:model$K]
  nearestNeighborsIndexes
}


predictOneSample <- function(model, nearestNeighborsIndexes) {
  # Predict label for a sample, given its nearest neighbors
  #
  # Args:
  #   model: A KNN model
  #   nearestNeighborsIndexes: K nearest neighbors indexe
  #
  # Returns:
  #   Predited label for the given sample.
  nearestNeighborsLabels <- model$trainingLabels[nearestNeighborsIndexes]
  labelCounts <- c()
  labels <- sort(unique(model$trainingLabels))
  for (label in labels) {
    labelCounts[label] <- sum(nearestNeighborsLabels == label)
  }
  which.max(labelCounts)
}
