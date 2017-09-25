sigmoid <- function(x) {
  return(1 / (1 + exp(-x)))
}


sigmoidDerivative <- function(x) {
  return(sigmoid(x) * (1 - sigmoid(x)))
}


tanh <- function(x) {
  (exp(x) - exp(-x)) / (exp(x) + exp(-x))
}


tanhDerivative <- function(x) {
  1 - tanh(x) ^ 2
}


softmax <- function(X) {
  if (!is.matrix(X))
    X <- matrix(X, nrow=1, ncol=length(X))
  expX <- exp(X)
  expX / apply(expX, 1, sum)
}


softmaxDerivative <- function(X) {
  softmaxX <- softmax(X)
  softmaxX * (1 - softmaxX)
}


linear <- function(x) {
  x
}


linearDerivative <- function(x) {
  matrix(1, nrow=length(x), ncol=1)
}