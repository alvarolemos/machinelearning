calcAccuracy <- function(yTrue, yPred) {
  length(yTrue[yPred == yTrue]) / length(yTrue)
}


calcBinaryLogLoss <- function(yTrue, yPred, eps = 1e-15) {
  pred <- pmin(pmax(yPred, eps), 1-eps)
  (sum(yTrue * log(yPred) + (1 - yTrue) * log(1 - yPred))) / length(yTrue)
}