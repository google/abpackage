# Copyright 2014-2018 Google Inc. All rights reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' SinglePrePost returns the percentage change and the difference between
#' the treatment and the control in an AB test for Gaussian data.
#' When available, pre-period data can also be
#' used to improve the statistical power of the test.
#'
#' @param data A data.frame with 4 columns for each sample. The column condition
#'   indicating whether the sample is from the control or treatment group.
#'   A column pre indicating the observed value in the pre-period. A column
#'   post indicating the observed value in the experiment period. If the
#'   column pre is missing, the test is based only on the experiment period.
#'   A column metric indicating the name of the metric.
#' @param weights An optional data.frame with two variables and a single row.
#'   The variables \code{control} and \code{treatment} indicate the weight of
#'   the two groups. The weight must be proportional to the traffic proportion
#'   of observations in the condition group.
#'   If NULL, equal weights are are used in the fitting process.
#' @param n.nodes The number of nodes used to approximate each univariate
#'   distribution.
#' @param ci.level Coverage of the confidence intervals.
#' @return
#'   A data.frame with 2 rows and 7 columns:
#'   lower, center, upper: the lower limit, the median and the upper
#'     limit of the credible interval.
#'   mean: the posterior mean.
#'   var: the posterior variance.
#'   p.value: the p-value.
#'   type: a string equal to percent.change or difference.
#' @keywords internal
SinglePrePost <- function(data,
                          weights = NULL,
                          n.nodes = 50,
                          ci.level = 0.95) {
  if (is.null(weights)) {
    weights <- data.frame(control = 1, treatment = 1)
  }
  metric <- data$metric[1]
  # If at least one of the post data is missing, return an empty data.frame.
  if (any(is.na(data$post))) {
    warning(sprintf("Missing values for the metric %s in the post period.",
                    metric))
    return(EmptyOutput())
  }
  # Use pre-period iff column pre exists, non pre-period data is missing,
  # standard deviation of pre-period is non zero, and
  # the design matrix in the second stage is not ill-conditioned.
  use.pre.period <-
    has_name(data, "pre") && IsPrePeriodValid(data, weights, metric)
  # Center and scale the pre.period to avoid ill conditioned design
  # matrices in the second stage.
  if (use.pre.period) {
    data %<>% CenterAndScalePrePeriod(weights)
  }
  trmt.data <- dplyr::filter(data, condition == "treatment")
  ctrl.data <- dplyr::filter(data, condition == "control")
  # If there are less than 5 observations per condition,
  # return an empty data.frame.
  kMinObs <- 5
  if (nrow(trmt.data) < kMinObs || nrow(ctrl.data) < kMinObs) {
    warning(sprintf("Too few observations for the metric %s.", metric))
    return(EmptyOutput())
  }
  # Compute the posterior for the post-period means.
  mus <- Posterior(trmt.data,
                   ctrl.data,
                   weights,
                   n.nodes,
                   use.pre.period)
  # Percent change is not meaningful for negative data. Do not
  # compute ci and p-value for percent change when data are negative.
  if (min(data$post) > 0) {
    percent.change <- (100 * mus$trmt / mus$ctrl) - 100
    pc.df <- ComputeStats(percent.change, ci.level, "percent.change")
  } else {
    warning(sprintf("Non-positive values for the metric %s in the post period.",
                    metric))
    pc.df <- EmptyOutput() %>%
      dplyr::filter(type == "percent.change")
  }
  # Compute ci and p-value for difference between treatment mean and control
  # mean.
  d.df <- ComputeStats(mus$trmt - mus$ctrl, ci.level, "difference")
  return(dplyr::bind_rows(pc.df, d.df))
}

#' Check whether to use the pre.period or not.
#'
#' @param data A data frame with columns pre and condition.
#' @param metric The name of the metric.
#' @return
#'    A boolean indicating whether to use the pre-period or not.
#' @keywords internal
IsPrePeriodValid <- function(data, weights, metric) {
  if (any(is.na(data$pre))) {
    warning(sprintf(paste0("Missing values for the metric %s in the pre ",
                           "period. The pre period will not be used."),
                    metric))
    return(FALSE)
  }
  if (sd(data$pre) < 1E-10) {
    warning(sprintf(paste0("Standard deviation in the pre period is ",
                           "too small for the metric %s. ",
                           "The pre period will not be used."),
                    metric))
    return(FALSE)
  }
  if (IsIllConditioned(data, weights)) {
      warning(sprintf("Ill conditioned pre-period for the metric %s.", metric))
      return(FALSE)
    }
  return(TRUE)
}

#' Computes the posterior distribution of the post-period means.
#'
#' @param trmt.data Data.frame for the treatment group.
#' @param ctrl.data Data.frame for the control group.
#' @param weights Data.frame with two variables and a single row.
#'   The variables \code{control} and \code{treatment} indicate the weight of
#'   the two groups.
#' @param n.nodes The number of nodes used to approximate each univariate
#'   distribution.
#' @param pre.period Boolean indicating whether to use the pre-period or not.
#' @return
#'    A matrix with two columns, representing the posterior of the control
#'    and the treatment group.
#' @keywords internal
Posterior <- function(trmt.data,
                      ctrl.data,
                      weights,
                      n.nodes,
                      pre.period) {
  # Create a grid of equally spaced nodes to approximate univariate
  # distributions.
  unif.nodes <- (2 * seq(1, n.nodes) - 1) / (2 * n.nodes)
  if (pre.period) {
    return(PrePostPosterior(trmt.data, ctrl.data, weights, unif.nodes))
  } else {
    return(PostOnlyPosterior(trmt.data$post, ctrl.data$post, unif.nodes))
  }
}

#' Computes the posterior distribution of the post-period means, when using
#' only post-period data.
#'
#' @param trmt.data Vector of post-period data from the treatment group.
#' @param ctrl.data Vector of post-period data from the control group.
#' @param unif.nodes Vector of probabilities.
#' @return
#'    A matrix with two columns, representing the posterior of the control
#'    and the treatment group.
#' @keywords internal
PostOnlyPosterior <- function(trmt.data, ctrl.data, unif.nodes) {
  n.trmt <- length(trmt.data)
  n.ctrl <- length(ctrl.data)
  mu.trmt.nodes <- NonStdStudentTQuantiles(unif.nodes,
                                           n.trmt - 1,
                                           sd(trmt.data)/ sqrt(n.trmt),
                                           mean(trmt.data))
  mu.ctrl.nodes <- NonStdStudentTQuantiles(unif.nodes,
                                           n.ctrl - 1,
                                           sd(ctrl.data)/ sqrt(n.ctrl),
                                           mean(ctrl.data))
  return(expand.grid(ctrl = mu.ctrl.nodes, trmt = mu.trmt.nodes))
}


#' Computes the posterior distribution of the post-period means, when
#' correcting for the pre-period.
#'
#' @param trmt.data Data.frame for the treatment group.
#' @param ctrl.data Data.frame for the control group.
#' @param weights Data.frame with two variables and a single row.
#'   The variables \code{control} and \code{treatment} indicate the weight of
#'   the two groups.
#' @param unif.nodes Vector of probabilities.
#' @return
#'    A matrix with two columns, representing the posterior of the control
#'    and the treatment group.
#' @keywords internal
PrePostPosterior <- function(trmt.data, ctrl.data, weights, unif.nodes) {
  # First stage: estimate pre-period mean.
  n.pre <- length(trmt.data$pre) + length(ctrl.data$pre)
  mu.pre <- NonStdStudentTQuantiles(unif.nodes,
                                    n.pre - 1,
                                    WeightedSE(trmt.data$pre,
                                               ctrl.data$pre,
                                               weights$treatment,
                                               weights$control),
                                    WeightedAverage(trmt.data$pre,
                                                    ctrl.data$pre,
                                                    weights$treatment,
                                                    weights$control))
  # Second stage: estimate experiment period means.
  mus <- dplyr::bind_rows(lapply(mu.pre,
                                 SecondStageEstimation,
                                 trmt.post.data = trmt.data$post,
                                 ctrl.post.data = ctrl.data$post,
                                 trmt.pre.data = trmt.data$pre,
                                 ctrl.pre.data = ctrl.data$pre,
                                 unif.nodes = unif.nodes))
  return(mus)
}

#' Computes the quantiles of the non-standardized Student's t-distribution.
#'
#' @param unif.nodes Vector of probabilities.
#' @param df Degrees of freedom.
#' @param sigma Scale parameter.
#' @param mu Location paramter.
#' @return
#'    A vector of quantiles.
#' @keywords internal
NonStdStudentTQuantiles <- function(unif.nodes, df, sigma, mu) {
  return(qt(unif.nodes, df = df) * sigma + mu)
}

#' Computes the parameters of a univariate linear regression.
#'
#' @param y Dependent variable.
#' @param x Explanatory variable.
#' @return
#'   A data.frame with:
#'     intercept: The estimate of the intercept coefficient.
#'     df: The degress of freedom of the standard deviation of the residuals.
#'     sigma: The standard deviation of the residuals.
#'     XtX.inverse.intercept: The top left elemeng of the inverse
#'       of the matrix X'X.
#' @keywords internal
UnivariateLinearRegression <- function(y, x) {
  # Transform inputs to double.
  x <- as.double(x)
  y <- as.double(y)
  # See pp. 222 - 224 of http://www.stat.cmu.edu/~hseltman/309/Book/chapter9.pdf
  n <- length(x)
  df <- n - 2
  slope <- cov(x, y) / var(x)
  intercept <- mean(y - slope * mean(x))
  y.hat <- intercept + slope * x
  sigma <- sqrt(sum((y - y.hat) ^ 2) / df)
  XtX.inverse.intercept <- sum(x * x) / (n * (n - 1) * var(x))

  return(data.frame(intercept = intercept,
                    df = df,
                    sigma = sigma,
                    XtX.inverse.intercept = XtX.inverse.intercept))
}

#' Computes a deterministic approximation of the second stage of pre-post.
#'
#' @param post.data Vector of data in the experiment period.
#' @param pre.data vector of data in the pre-period.
#' @param mu.0 Point estimate of the mean during the pre-period.
#' @param unif.nodes Vector of equally spaced nodes between zero and one.
#' @return A discrete approximation of the posterior distribution of the mean.
#' @keywords internal
ConditionalRegression <- function(post.data,
                                  pre.data,
                                  mu.0,
                                  unif.nodes) {
  fit <- UnivariateLinearRegression(post.data, pre.data - mu.0)
  nodes <-
    NonStdStudentTQuantiles(unif.nodes,
                            fit$df,
                            fit$sigma * sqrt(fit$XtX.inverse.intercept),
                            fit$intercept)
  return(nodes)
}

#' Computes estimates of the experiment period means given the pre-period mean.
#'
#' @param mu.0 The mean in the pre-period.
#' @param trmt.post.data A vector of data under treatment in the experiment
#'   period.
#' @param ctrl.post.data A vector of data under control in the experiment
#'   period.
#' @param trmt.pre.data A vector of data under treatment in the pre-period.
#' @param ctrl.pre.data A vector of data under control in the pre-period.
#' @param unif.nodes A vector of equally spaced nodes between zero and one.
#' @return
#'   A matrix with posterior estimates of the experiment period means.
#' @keywords internal
SecondStageEstimation <- function(mu.0,
                                  trmt.post.data,
                                  ctrl.post.data,
                                  trmt.pre.data,
                                  ctrl.pre.data,
                                  unif.nodes) {

  mu.trmt.nodes <- ConditionalRegression(trmt.post.data,
                                         trmt.pre.data,
                                         mu.0,
                                         unif.nodes)
  mu.ctrl.nodes <- ConditionalRegression(ctrl.post.data,
                                         ctrl.pre.data,
                                         mu.0,
                                         unif.nodes)
  return(expand.grid(ctrl = mu.ctrl.nodes, trmt = mu.trmt.nodes))
}

#' Center and scale the pre-period data according to weights.
#'
#' @param data A data.frame with 2 columns for each sample. The column condition
#'   indicating whether the sample is from the control or treatment group.
#'   A column pre indicating the observed value in the pre-period.
#' @param weights An data.frame with two variables and a single row.
#'   The variables \code{control} and \code{treatment} indicate the weight of
#'   the two groups.
#' @keywords internal
CenterAndScalePrePeriod <- function(data, weights) {
  ctrl.pre.data <- data %>%
    dplyr::filter(condition == "control") %>%
    dplyr::pull(pre)
  trmt.pre.data <- data %>%
    dplyr::filter(condition == "treatment") %>%
    dplyr::pull(pre)

  pre.period.mean <- WeightedAverage(ctrl.pre.data,
                                     trmt.pre.data,
                                     weights$control,
                                     weights$treatment)
  pre.period.sd <- WeightedSD(ctrl.pre.data,
                              trmt.pre.data,
                              weights$control,
                              weights$treatment)
  return(data %>% dplyr::mutate(pre = (pre - pre.period.mean) / pre.period.sd))

}

#' Checks whether the pre-period data would result in a ill conditioned
#' design matrix in the second stage.
#'
#' @param data A data.frame with 2 columns for each sample. The column condition
#'   indicating whether the sample is from the control or treatment group.
#'   A column pre indicating the observed value in the pre-period.
#' @param weights An data.frame with two variables and a single row.
#'   The variables \code{control} and \code{treatment} indicate the weight of
#'   the two groups.
#' @param threshold The condition number threshold to determine if the design
#'   matrix is ill conditioned.
#' @keywords internal
IsIllConditioned <- function(data, weights, threshold = 1E-10) {

  ctrl.pre.data <- data %>%
    dplyr::filter(condition == "control") %>%
    dplyr::pull(pre)
  trmt.pre.data <- data %>%
    dplyr::filter(condition == "treatment") %>%
    dplyr::pull(pre)

  pre.period.mean <- WeightedAverage(ctrl.pre.data,
                                     trmt.pre.data,
                                     weights$control,
                                     weights$treatment)
  pre.period.sd <- WeightedSD(ctrl.pre.data,
                              trmt.pre.data,
                              weights$control,
                              weights$treatment)

  centered.ctrl.pre.data <- (ctrl.pre.data - pre.period.mean) / pre.period.sd
  centered.trmt.pre.data <- (trmt.pre.data - pre.period.mean) / pre.period.sd

  return(IsIllConditionedMatrix(centered.ctrl.pre.data, threshold) ||
    IsIllConditionedMatrix(centered.trmt.pre.data, threshold))
}

#' Checks whether the design matrix for the second stage of PrePost is ill
#' conditioned.
#'
#' @param x.centered Centered vector of pre-period data for either control or
#'  treatment group.
#' @param threshold The condition number threshold to determine if the design
#'   matrix is ill conditioned.
#' @keywords internal
IsIllConditionedMatrix <- function(x.centered, threshold) {
  # Construct design matrix
  n <- length(x.centered)
  X <- cbind(rep(1, n), x.centered)
  Z <- t(X) %*% X
  # Compute the trace and the determinant
  trace <- sum(diag(Z))
  deter <- det(Z)
  # Compute eigenvalues
  lambda.min <- trace / 2 - sqrt(trace ^ 2 / 4 - deter)
  lambda.max <- trace / 2 + sqrt(trace ^ 2 / 4 - deter)

  return(lambda.min / lambda.max < threshold)
}

#' Computes empirical p-value from vector of data
#'
#' @param data A vector.
#' @return The p-value.
#' @keywords internal
ComputePValue <- function(data) {
  prob.positive.change <- mean(data > 0)
  return(2 * min(prob.positive.change, 1 - prob.positive.change))
}

#' Compute stats from a vector of data
#'
#' @param data A vector.
#' @param ci.level The confidence level for the credible intervals.
#' @param which A string indicating the type of confidence interval.
#'   Options are 'percent.change' and 'difference'.
#' @return
#'   A data.frame with lower, median and upper values for the credible interval,
#'   mean, variance, p.value and type.
#' @keywords internal
ComputeStats <- function(data, ci.level, which) {
  alpha <- 1 - ci.level
  return(data.frame(lower = quantile(data, alpha / 2),
                    median = median(data),
                    upper = quantile(data, 1 - alpha / 2),
                    mean = mean(data),
                    var = var(data),
                    p.value = ComputePValue(data),
                    type = which,
                    stringsAsFactors = FALSE))
}

#' WeightedAverage returns the weighted average of two vectors
#'
#' @param x.1 A numerical vector.
#' @param x.2 A numerical vector.
#' @param weight.1 A positive weight.
#' @param weight.2 A positive weight.
#' @return
#'   The weighted average of x.1 and x.2.
#' @keywords internal
WeightedAverage <- function(x.1, x.2, weight.1 = 1, weight.2 = 1) {
  assert_that(weight.1 > 0, weight.2 > 0)
  n.1 <- length(x.1)
  n.2 <- length(x.2)
  return((weight.1 * sum(x.1) + weight.2 * sum(x.2)) /
           (weight.1 * n.1 + weight.2 * n.2))
}

#' WeightedSD returns the standard deviation of weighted average two vectors
#'
#' @param x.1 A numerical vector.
#' @param x.2 A numerical vector.
#' @param weight.1 A positive weight.
#' @param weight.2 A positive weight.
#' @return
#'   The standard deviation of weighted average of x.1 and x.2.
#' @keywords internal
WeightedSD <- function(x.1, x.2, weight.1 = 1, weight.2 = 1) {
  assert_that(weight.1 > 0, weight.2 > 0)
  n.1 <- length(x.1)
  n.2 <- length(x.2)
  x.bar <- WeightedAverage(x.1, x.2, weight.1, weight.2)
  weighted.sd <- sqrt((weight.1 * sum((x.1 - x.bar) ^ 2) +
      weight.2 * sum((x.2 - x.bar) ^ 2)) / (n.1 + n.2 - 1))
  return(weighted.sd)
}

#' WeightedSE returns the standard error of weighted average two vectors
#'
#' @param x.1 A numerical vector.
#' @param x.2 A numerical vector.
#' @param weight.1 A positive weight.
#' @param weight.2 A positive weight.
#' @return
#'   The standard error of weighted average of x.1 and x.2.
#' @keywords internal
WeightedSE <- function(x.1, x.2, weight.1 = 1, weight.2 = 1) {
  assert_that(weight.1 > 0, weight.2 > 0)
  weighted.sd <- WeightedSD(x.1, x.2, weight.1, weight.2)
  return(weighted.sd / sqrt(length(x.1) * weight.1 + length(x.2) * weight.2))
}

#' Returns output where all values are NAs.
#'
#' @return
#'   A data.frame with 2 rows and 7 columns:
#'   lower, center, upper: the lower limit, the median and the upper
#'     limit of the credible interval.
#'   mean: the posterior mean.
#'   var: the posterior variance.
#'   p.value: the p-value.
#'   type: a string equal to percent.change or difference.
#' @keywords internal
EmptyOutput <- function() {
  kNRows <- 2
  empty.output <- data.frame(lower = rep(NA, kNRows),
                             center = rep(NA, kNRows),
                             upper = rep(NA, kNRows),
                             mean = rep(NA, kNRows),
                             var = rep(NA, kNRows),
                             p.value = rep(NA, kNRows),
                             type = c("percent.change", "difference"),
                             stringsAsFactors = FALSE)
  return(empty.output)
}
