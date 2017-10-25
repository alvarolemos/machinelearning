fitPca <- function(X) {
  # Applies the Principal Component Analysis on the given dataset.
  #
  # Args:
  #   X: Samples on which the PCA will be applied.
  #
  # Returns:
  #   A PCA model, that is a list with the eigen values and vectors
  #   of the Principal Components.
  XTranslated <- translateToDatasetCenter(X)
  covMatrix <- cov(XTranslated)
  eigenCovMatrix <- eigen(covMatrix)
  list(
    eigenValues = eigenCovMatrix$values,
    eigenVectors = eigenCovMatrix$vectors
  )
}


translateToDatasetCenter <- function(X) {
  # Translates a dataset to its center.
  #
  # Args:
  #   X: dataset
  #
  # Returns:
  #   Translated to the center dataset.
  XMean <- colMeans(X)
  XTranslated <- X - matrix(XMean, nrow=nrow(X), ncol=ncol(X))
  XTranslated
}


projectPrincipalComponents <- function(model, X, nComponents=NULL, minExpVar=NULL) {
  # Project given dataset on the model's Principal Components.
  #
  # Args:
  #   model: PCA model, with training dataset's eigen vectors and values
  #   X: Dataset on which the Principal Components projection will be model
  #   nComponets: Number of components to use in the projection
  #   minExpVar: Minimum explained variance to consider. Automatically chooses
  #              the number of Principal Components to obtain that.
  #
  # Returns:
  #   The projected dataset.
  XTranslated <- translateToDatasetCenter(X)
  XProjected <- XTranslated %*% model$eigenVectors

  if (!is.null(minExpVar)) {
    for (nComps in 1:ncol(X)) {
      cumExpVar <- calcCumulativeProportionVarianceExplained(model, X, nComps)
      if (cumExpVar >= minExpVar) {
        nComponents = nComps
        break
      }
    }
  } else if (is.null(nComponents)) {
    nComponents = ncol(X)
  }
  
  XProjected[, 1:nComponents]
}


calcProportionVarianceExplained <- function(model, X, component) {
  # Calculates the Proportion Variance Explained of a specific component.
  #
  # Args:
  #   model: PCA model
  #   X: Dataset on which the variance explained will be calculated
  #   component: Number of the principal component considered
  #
  # Returns:
  #   Proportion Variance Explained.
  calcVarianceExplained(model, X, component) / calcTotalVariance(X)
}


calcCumulativeProportionVarianceExplained <- function(model, X, nComponents) {
  # Calculates the Cumulative Proportion Variance Explained of a range of Principal Components.
  #
  # Args:
  #   model: PCA model
  #   X: Dataset on which the variance explained will be calculated
  #   nComponent: Range of Principal Components considered
  #
  # Returns:
  #   Cumulative Proportion Variance Explained.
  cumVarExp <- 0
  for (component in 1:nComponents) {
    cumVarExp <- cumVarExp + calcVarianceExplained(model, X, component)
  }
  cumVarExp / calcTotalVariance(X)
}


calcVarianceExplained <- function(model, X, component) {
  # Calculates the Variance Explained of a Principal Component.
  #
  # Args:
  #   model: PCA model
  #   X: Dataset on which the variance explained will be calculated
  #   component: Considered component
  #
  # Returns:
  #   Cumulative Proportion Variance Explained.
  XTranslated <- translateToDatasetCenter(X)
  sum((XTranslated %*% model$eigenVectors[, component]) ^ 2) / nrow(X)
}


calcTotalVariance <- function(X) {
  # Calculates the Total Variance of a dataset.
  #
  # Args:
  #   X: Dataset
  #
  # Returns:
  #   Total Variance.
  XTranslated <- translateToDatasetCenter(X)
  sum(apply(XTranslated, 2, function(x) x^2)) / nrow(X)
}