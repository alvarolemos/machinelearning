fitNumericEncoder <- function(y) {
  labels <- unique(y)
  encoder <- list()
  for (i in 1:length(labels)) {
    encoder[[labels[i]]] <- i
  }
  encoder
}


numericEncode <- function(encoder, y) {
  if (!is.matrix(y))
    y <- matrix(y, nrow=length(y), ncol=1)
  yEncoded <- apply(y, 1, function(label) encoder[[label]])
  matrix(yEncoded, nrow=nrow(y), ncol=1)
}


numericDecode <- function(encoder, y) {
  decoder <- getNumericDecoder(encoder)
  if (!is.matrix(y))
    y <- matrix(y, nrow=length(y), ncol=1)
  yDecoded <- apply(y, 1, function(numericLabel) decoder[[numericLabel]])
  matrix(yDecoded, nrow=nrow(y), ncol=1)
}


getNumericDecoder <- function(encoder) {
  decoder <- list()
  for (i in 1:length(encoder)) {
    decoder[[i]] <- names(encoder)[i]
  }
  decoder
}


oneHotEncode <- function(y) {
  if (!is.matrix(y))
    y <- matrix(y, nrow=length(y), ncol=1)
  dummyMatrix <- matrix(0, nrow=nrow(y), ncol=length(unique(y)))
  for (i in 1:nrow(y)) {
    dummyMatrix[i, y[i]] <- 1
  }
  dummyMatrix
}


oneHotDecode <- function(yDummy) {
  y <- apply(yDummy, 1, which.max)
  matrix(y, nrow=nrow(yDummy), ncol=1)
}


zNormalize <- function(x) {
  (x - mean(x)) / sd(x)
}