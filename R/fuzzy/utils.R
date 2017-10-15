plotGaussians <- function(model, X, y, step = 0.01) {
  means <- model$rulesCenters
  stdDevs <- model$rulesStdDevs
  labels <- model$rulesConsequents

  x1Range <- seq(min(X[, 1])-0.5, max(X[, 1])+0.5, step)
  x2Range <- seq(min(X[, 2])-0.5, max(X[, 2])+0.5, step)
  x1Lim = c(min(x1Range), max(x1Range))
  x2Lim = c(min(x2Range), max(x2Range))
  
  for (cluster in 1:nrow(means)) {
    gaussian <- calcGaussianMembership(x1Range, means[cluster, 1], stdDevs[cluster, 1]) * 0.1
    plot(x1Range, gaussian + min(x2Range), xlab='', ylab='', xlim=x1Lim, ylim=x2Lim, type='l', col=labels[cluster]+1)
    par(new = TRUE)
  }
  
  for (cluster in 1:nrow(means)) {
    gaussian <- calcGaussianMembership(x2Range, means[cluster, 1], stdDevs[cluster, 1]) * 0.1
    plot(gaussian + min(x1Range), x2Range, xlab='', ylab='', xlim=x1Lim, ylim=x2Lim, type='l', col=labels[cluster]+1)
    par(new = TRUE)
  }
  
  plot(X, col=y+1, xlab='x1', ylab='x2', xlim=x1Lim, ylim=x2Lim)
  par(new = TRUE)
  plot(means, col=labels+1, xlab='', ylab='', xlim=x1Lim, ylim=x2Lim, pch=21, bg=labels+1)
}


plotDecisionSurface <- function(model, X, y, step = 0.01) {
  x1Range <- seq(min(X[, 1]), max(X[, 1]), step)
  x2Range <- seq(min(X[, 2]), max(X[, 2]), step)
  x1Lim = c(min(x1Range), max(x1Range))
  x2Lim = c(min(x2Range), max(x2Range))

  surface <- outer(x1Range, x2Range, FUN = function(x1, x2, model) { predictFuzzyClassifier(model, cbind(x1, x2)) }, model)
  contour(x1Range, x2Range, surface, nlevels = 5, drawlabels=FALSE, xlim = x1Lim, ylim = x2Lim)
  par(new = TRUE)
  plot(X, xlim = x1Lim, ylim = x2Lim, col=y+1)
}