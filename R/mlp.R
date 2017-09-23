feedForward <- function(X, weights) {
  netActivations <- list()
  layerOutputs <- list()
  layerInput <- X

  netDepth <- length(weights)
  for (i in 1:netDepth) {
    netActivations[[i]] <- passForward(layerInput, weights[[i]])
    layerOutputs[[i]] <- activate(netActivations[[i]])
    layerInput <- layerOutputs[[i]]
  }

  list(
    netActivations = netActivations,
    layerOutputs = layerOutputs
  )
}


passForward <- function(X, W) {
  Xbiased <- addBias(X)
  net <- Xbiased %*% t(W)
  return(net)
}


addBias <- function(X) {
  return(cbind(1, X))
}


removeBias <- function(W) {
  matrix(W[, 2:ncol(W)], nrow=nrow(W), ncol=ncol(W)-1)
}


initializeRandomWeights <- function(inputSize, outputSize) {
  randomValues <- runif(inputSize * outputSize)
  return(matrix(randomValues, nrow=outputSize, ncol=inputSize))
}


sigmoid = function(x) {
  return(1 / (1 + exp(-x)))
}


sigmoidDerivative = function(x) {
  return(sigmoid(x) * (1 - sigmoid(x)))
}


computeCost <- function(targets, z) {
  return(0.5 * sum(targets - z) ^ 2)
}


activate <- function(net) {
  return(sigmoid(net))
}