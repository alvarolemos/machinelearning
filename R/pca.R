fitPca <- function(X) {
  # Applies the Principal Component Analysis on the given dataset.
  #
  # Args:
  #   X: Samples on which the PCA will be applied.
  #
  # Returns:
  #   A PCA model, that is a list with the eigen values and vectors
  #   of the principal components.
}


projectPrincipalComponents <- function(model, X, nComponents, minExpVar) {
  # Project given dataset on the model's principal components.
  #
  # Args:
  #   model: PCA model, with training dataset's eigen vectors and values
  #   X: Dataset on which the principal components projection will be model
  #   nComponets: Number of components to use in the projection
  #   minExpVar: Minimum explained variance to consider. Automatically chooses
  #              the number of principal components to obtain that.
  #
  # Returns:
  #   The projected dataset.
}


calcExplainedVariance <- function(model, X, nComponents) {
  # Calculates the explained variance of the projected space.
  #
  # Args:
  #   model: PCA model
  #   X: Dataset on which the explained variance will be calculated
  #   nComponents: Number of principal components to consider
  #
  # Returns:
  #   Explained variance.
}