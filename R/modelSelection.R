trainTestSplit <- function(X, y, trainSetFraction) {
  nSamples <- nrow(X)
  nTrain <- round(nSamples * trainSetFraction)

  randomIndexes <- sample(nSamples)
  trainIndexes <- randomIndexes[1:nTrain]
  testIndexes <- randomIndexes[nTrain:nSamples]

  XTrain <- X[trainIndexes, ]
  XTest <- X[testIndexes, ]
  yTrain <- y[trainIndexes]
  yTest <- y[testIndexes]

  list(
    XTrain = XTrain,
    XTest = XTest,
    yTrain = yTrain,
    yTest = yTest
  )
}


trainValTestSplit <- function(X, y, trainSetFraction, valSetFraction) {
  nSamples <- nrow(X)
  nTrain <- round(nSamples * trainSetFraction)
  nVal <- round(nSamples * valSetFraction)

  randomIndexes <- sample(nSamples)
  trainIndexes <- randomIndexes[1:nTrain]
  valIndexes <- randomIndexes[nTrain:nVal]
  testIndexes <- randomIndexes[nVal:nSamples]

  XTrain <- X[trainIndexes, ]
  XVal <- X[valIndexes, ]
  XTest <- X[testIndexes, ]
  yTrain <- y[trainIndexes]
  yVal <- y[valIndexes]
  yTest <- y[testIndexes]

  list(
    XTrain = XTrain,
    XVal = XVal,
    XTest = XTest,
    yTrain = yTrain,
    yVal = yVal,
    yTest = yTest
  )
}