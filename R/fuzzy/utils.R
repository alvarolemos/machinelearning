plotGaussians <- function(X, y, means, stdDevs, labels, step = 0.01) {
  x1Range <- seq(min(X[, 1])-0.5, max(X[, 1])+0.5, step)
  x2Range <- seq(min(X[, 2])-0.5, max(X[, 2])+0.5, step)
  x1Lim = c(min(x1Range), max(x1Range))
  x2Lim = c(min(x2Range), max(x2Range))
  
  for (cluster in 1:nrow(means)) {
    gaussian <- calcPdf(x1Range, means[cluster, 1], stdDevs[cluster, 1]) * 0.1
    plot(x1Range, gaussian + min(x2Range), xlab='', ylab='', xlim=x1Lim, ylim=x2Lim, type='l', col=labels[cluster])
    par(new = TRUE)
  }
  
  for (cluster in 1:nrow(means)) {
    gaussian <- calcPdf(x2Range, means[cluster, 1], stdDevs[cluster, 1]) * 0.1
    plot(gaussian + min(x1Range), x2Range, xlab='', ylab='', xlim=x1Lim, ylim=x2Lim, type='l', col=labels[cluster])
    par(new = TRUE)
  }
  
  plot(X, col=y+1, xlab='x1', ylab='x2', xlim=x1Lim, ylim=x2Lim)
  par(new = TRUE)
  plot(means, col=labels, xlab='', ylab='', xlim=x1Lim, ylim=x2Lim, pch=21, bg=labels)
}