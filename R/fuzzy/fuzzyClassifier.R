library(e1071)


fitFuzzyClassifier <- function(X, y, nRules) {
  cMeansModel <- cmeans(X, nRules)
  rulesStdDevs <- calcStdDevs(X, cMeansModel$membership)
  rulesConsequents <- assignRuleConsequent(y, cMeansModel$membership)

  list(
    rulesCenters = cMeansModel$centers,
    rulesStdDevs = rulesStdDevs,
    rulesConsequents = rulesConsequents
  )
}


calcStdDevs <- function(X, membershipMatrix) {
  nSamples <- nrow(X)
  nFeatures <- ncol(X)
  nRules <- ncol(membershipMatrix)
  stdDevs <- c()

  for (rule in 1:nRules) {
    currentRuleMembership <- rep(membershipMatrix[, rule], nFeatures)
    currentRuleMembership <- matrix(currentRuleMembership, nrow=nSamples, ncol=nFeatures)
    weightedSamples <- X * currentRuleMembership
    stdDevs <- rbind(stdDevs, apply(weightedSamples, 2, sd))
  }

  stdDevs
}


assignRuleConsequent <- function(y, membershipMatrix) {
  labels <- sort(unique(y))
  membershipSumPerClass <- c()
  for (label in labels) {
    mask <- which(y == label)
    membershipSumPerClass <- cbind(membershipSumPerClass, apply(membershipMatrix[mask, ], 2, sum))
  }
  groupsConsequents <- apply(membershipSumPerClass, 1, which.max)
  binarizeClasses(groupsConsequents)
}


binarizeClasses <- function(classes) {
  classes - 1
}


predictFuzzyClassifier <- function(model, X) {
  rulesCenters <- model$rulesCenters
  rulesStdDevs <- model$rulesStdDevs  

  activations <- calcActivations(model, X)

  consequentsProbabilisticSum <- c()
  consequents <- sort(unique(model$rulesConsequents))
  for (consequent in consequents) {
    mask <- model$rulesConsequents == consequent
    currentConsequentActivations <- activations[, mask]
    if (!is.matrix(currentConsequentActivations))
      currentConsequentActivations <- matrix(currentConsequentActivations, nrow=length(currentConsequentActivations), ncol=1)
    currentConsequentProbabilisticSum <- apply(currentConsequentActivations, 1, calcProbabilisticSum)
    consequentsProbabilisticSum <- cbind(consequentsProbabilisticSum, currentConsequentProbabilisticSum)
  }

  predictions <- apply(consequentsProbabilisticSum, 1, which.max)
  binarizeClasses(predictions)
}


calcActivations <- function(model, X) {
  rulesCenters <- model$rulesCenters
  rulesStdDevs <- model$rulesStdDevs
  activations <- c()

  for (rule in 1:nrow(rulesCenters)) {
    membershipGrades <- c()
    for (feature in 1:ncol(X)) {
      mu <- rulesCenters[rule, feature]
      sigma <- rulesStdDevs[rule, feature]
      membershipGrades <- cbind(membershipGrades, calcGaussianMembership(X[, feature], mu, sigma))
    }
    currentRuleActivations <- apply(membershipGrades, 1, prod)
    activations <- cbind(activations, currentRuleActivations)
  }

  activations
}


calcGaussianMembership <- function(x, mean, stdDev) {
  (1 / (sqrt(2 * pi * stdDev * stdDev))) * exp(-0.5 * ((x - mean) / (stdDev)) ^ 2)
}


calcProbabilisticSum <- function(x) {
  return(sum(x) - prod(x))
}
