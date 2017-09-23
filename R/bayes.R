library(foreach)


fitBayesian <- function(X, y) {
  # Fits a Naive Bayes model.
  #
  # Args:
  #   X: Samples matrix, where each row is a sample and the columns are features
  #   y: Label vector, with one label for each sample
  #
  # Returns:
  #   A list with the following parameters for each label of the training set:
  #     - Vector with means for each feature
  #     - Covariance matrix
  #     - Prior probability
  model <- list()
  labels <- sort(unique(y))
  # foreach (label = unique(y)) %do% {
  for (label in labels) {
    mask <- which(y == label)
    Xlabel <- as.matrix(X[mask, ])

    means <- apply(Xlabel, 2, mean)
    covMatrix <- cov(Xlabel)
    priorProb <- length(y[mask]) / length(y)

    model[[label]] <- list(
      means = means,
      covMatrix = covMatrix,
      priorProb = priorProb
    )
  }
  model
}


predictBayesian <- function(model, X) {
  # Classifies samples based on Bayes' Formula.
  #
  # Args:
  #   model: A bayesian model, trained by the fitBayesian function
  #   X: samples to be classified
  #
  # Returns:
  #   A vector with predicted classes
  probs <- predictProbs(model, X)
  preds <- apply(probs, 1, which.max)
  as.vector(preds)
}


predictProbs <- function(model, X) {
  # Calculates posterior probabilities using Bayes' Formula.
  #
  # Args:
  #   model: A bayesian model, trained by the fitBayesian function
  #   X: samples to have its posterior probabilities calculated
  #
  # Returns:
  #   A matrix with posterior probabilities as columns and samples as rows
  probs <- c()
  for (labelParams in model) {
    labelProbs <- apply(as.matrix(X), 1, calcPosteriorProb, labelParams)
    probs <- cbind(probs, labelProbs)
  }
  probs
}


calcPosteriorProb <- function(x, model) {
  # Calculates posterior probabilities using Bayes' Formula, for one sample.
  #
  # Args:
  #   x: Sample
  #   m: Distribution's mean vector
  #   K: Distribution's covariance matrix
  #   priorProb: Distribution's prior probability
  #
  # Returns:
  #   Probabilities for each one of the dataset's labels
  likelihood <- pdfnvar(x, model$means, model$covMatrix)
  likelihood * model$priorProb
}


pdfnvar <- function(x, m, K) {
  # Evaluates the multi-variate normal distribution value of a sample.
  #
  # Args:
  #   x: Sample
  #   m: Distribution's mean vector
  #   K: Distribution's covariance matrix
  n <- length(x)
  (1 / (sqrt((2 * pi) ^ n * (det(K))))) * exp(-0.5 * (t(x - m) %*% (solve(K)) %*% (x - m)))
}