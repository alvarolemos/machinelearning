AVAILABLE_TRAINING_PROTOCOLS <- c('sgd', 'batch')


fitMlp <- function(X, targets, layout, learningRate, maxError=0.001, maxNumEpochs=1000, protocol='sgd') {
  if (!(protocol %in% AVAILABLE_TRAINING_PROTOCOLS)) {
    stop(paste('Invalid training protocol:', protocol))
  }

  state <- list()
  netDepth <- length(layout)
  state$weights <- initializeRandomWeights(X, layout)
  state$J <- c()

  for (epoch in 1:maxNumEpochs) {

    feedForwardResults <- feedForward(X, state$weights)
    state$netActivations <- feedForwardResults$netActivations
    state$layerOutputs <- feedForwardResults$layerOutputs

    predictions <- state$layerOutputs[[netDepth]]
    state$J <- c(state$J, computeCost(targets, predictions))
    state$errors <- targets - predictions

    deltaWeights <- initializeWeightsCorrection(state$weights)
    nSamples <- nrow(X)
    for (sample in 1:nSamples) {
      state$currentSample <- sample
      state$sensitivities <- list()

      for (layer in netDepth:1) {
        state$currentLayer <- layer
        state$sensitivities[[layer]] <- calcSensitivity(state, layout)
        dW <- calcWeightsCorrection(state, X, learningRate)
        
        if (protocol == 'sgd') {
          state$weights[[layer]] <- state$weights[[layer]] + dW
        } else if (protocol == 'batch') {
          deltaWeights[[layer]] <- deltaWeights[[layer]] + dW
        }
      }
    }

    if (protocol == 'batch') {
      for (layer in netDepth:1) {
        state$weights[[layer]] <- state$weights[[layer]] + deltaWeights[[layer]]
      }
    }

    if (state$J[epoch] <= maxError)
      break
  }

  list(
    weights = state$weights,
    costHistory = state$J  
  )
}


initializeRandomWeights <- function(X, layout) {
  weights <- list()
  prevLayerSize <- ncol(X)
  for (layer in 1:length(layout)) {
    weights[[layer]] <- initializeRandomLayerWeights(prevLayerSize + 1, layout[layer])
    prevLayerSize <- layout[layer]
  }
  weights
}


initializeRandomLayerWeights <- function(inputSize, outputSize) {
  randomValues <- runif(inputSize * outputSize)
  return(matrix(randomValues, nrow=outputSize, ncol=inputSize))
}


initializeWeightsCorrection <- function(weights) {
  deltaWeights <- list()
  for (i in 1:length(weights)) {
    deltaWeights[[i]] <- matrix(0, nrow = nrow(weights[[i]]), ncol = ncol(weights[[i]]))
  }
  deltaWeights
}


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


calcSensitivity <- function(state, layout) {
  layer <- state$currentLayer
  sample <- state$currentSample
  dOutputdNetActivation <- sigmoidDerivative(state$netActivations[[layer]][sample, ])
  if (layer == length(layout)) {
    error <- state$errors[sample]
    sensitivity <- dOutputdNetActivation * error
  } else {
    weightedNextLayerSensitivitiesSum <- t(removeBias(state$weights[[layer + 1]])) %*% state$sensitivities[[layer + 1]]
    sensitivity <- dOutputdNetActivation * weightedNextLayerSensitivitiesSum
  }
  matrix(sensitivity, nrow=layout[layer], ncol=1)
}


calcWeightsCorrection <- function(state, X, learningRate) {
  layer <- state$currentLayer
  sample <- state$currentSample
  
  if (layer == 1) {
    prevLayerOutput <- X
  } else {
    prevLayerOutput <- state$layerOutputs[[layer - 1]]
  }
  prevLayerOutput <- addBias(prevLayerOutput)

  learningRate * (state$sensitivities[[layer]] %*% t(prevLayerOutput[sample, ]))
}
