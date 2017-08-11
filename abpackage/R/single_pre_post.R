# Copyright 2014-2017 Google Inc. All rights reserved.
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
#' @param data A data.frame with 3 columns for each sample. The column condition
#'   indicating whether the sample is from the control or treatment group.
#'   A column pre indicating the observed value in the pre-period. A column
#'   post indicating the observed value in the experiment period. If the
#'   column pre is missing, the test is based only on the experiment period.
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
SinglePrePost <- function(data,
                          weights = NULL,
                          n.nodes = 50,
                          ci.level = 0.95) {
  if (is.null(weights)) {
    weights <- data.frame(control = 1, treatment = 1)
  }
  kNRows <- 2
  empty.output <- data.frame(lower = rep(NA, kNRows),
                             center = rep(NA, kNRows),
                             upper = rep(NA, kNRows),
                             mean = rep(NA, kNRows),
                             var = rep(NA, kNRows),
                             p.value = rep(NA, kNRows),
                             type = c("percent.change", "difference"),
                             stringsAsFactors = FALSE)

  # If at least one of the post data is missing, return an empty data.frame.
  if (any(is.na(data$post))) {
    return(empty.output)
  }

  # Create a grid of equally spaced nodes to approximate univariate
  # distributions.
  unif.nodes <- (2 * 1:n.nodes - 1) / (2 * n.nodes)

  # Use pre-period iff column pre exists, non pre-period data is missing, and
  # standard deviation of pre-period is non zero.
  use.pre.period <- has_name(data, "pre") &&
    all(!is.na(data$pre)) && (sd(data$pre) > 1E-10)
  # Center and scale the pre.period to avoid ill conditioned design
  # matrices in the second stage.
  if (use.pre.period) {
    data %<>% dplyr::mutate(pre = (pre - mean(pre)) / sd(pre))
  }
  trmt.data <- dplyr::filter(data, condition == "treatment")
  ctrl.data <- dplyr::filter(data, condition == "control")

  if (!use.pre.period) {

    n.trmt.post <- length(trmt.data$post)
    n.ctrl.post <- length(ctrl.data$post)
    mu.trmt.nodes <-
        NonStdStudentTQuantiles(unif.nodes,
                                n.trmt.post - 1,
                                sd(trmt.data$post)/ sqrt(n.trmt.post),
                                mean(trmt.data$post))
    mu.ctrl.nodes <-
        NonStdStudentTQuantiles(unif.nodes,
                                n.ctrl.post - 1,
                                sd(ctrl.data$post)/ sqrt(n.ctrl.post),
                                mean(ctrl.data$post))
    mus <- expand.grid(ctrl = mu.ctrl.nodes, trmt = mu.trmt.nodes)
  } else {
    # If the design matrix of the regression is ill-conditioned, return an
    # empty data.frame.
    if (IsIllConditioned(ctrl.data$pre) || IsIllConditioned(trmt.data$pre)) {
      return(empty.output)
    }
    # First stage: estimate pre-period mean.
    mu.pre <- NonStdStudentTQuantiles(unif.nodes,
                                      n.pre <- length(data$pre) - 1,
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

  }

  # Percent change is not meaningful for negative data. Do not
  # compute ci and p-value for percent change when data are negative.
  if (min(data$post) > 0) {
    percent.change <- (100 * mus$trmt / mus$ctrl) - 100
    pc.df <- ComputeStats(percent.change, ci.level, "percent.change")
  } else {
    pc.df <- data.frame(lower = NA,
                        center = NA,
                        upper = NA,
                        mean = NA,
                        var = NA,
                        p.value = NA,
                        type = "percent.change",
                        stringsAsFactors = FALSE)
  }
  # Compute ci and p-value for difference between treatment mean and control
  # mean.
  d.df <- ComputeStats(mus$trmt - mus$ctrl, ci.level, "difference")
  return(dplyr::bind_rows(pc.df, d.df))
}

#' Computes the quantiles of the non-standardized Student's t-distribution.
#'
#' @param unif.nodes Vector of probabilities.
#' @param df Degrees of freedom.
#' @param sigma Scale parameter.
#' @param mu Location paramter.
#' @return
#'    A vector of quantiles.
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

#' Checks whether the design matrix for the second stage of PrePost is ill
#' conditioned.
#'
#' @param x Vector of pre-period data for either control or treatment group.
#' @param threshold The condition number threshold to determine if the design
#'   matrix is ill conditioned.
IsIllConditioned <- function(x, threshold = 1E-10) {
  # Construct design matrix
  n <- length(x)
  X <- cbind(rep(1, n), c(x - mean(x)))
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
#'   A data.frame with lower, center and upper values for the credible interval,
#'   mean, variance, p.value and type.
ComputeStats <- function(data, ci.level, which) {
  alpha <- 1 - ci.level
  return(data.frame(lower = quantile(data, alpha / 2),
                    center = median(data),
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
WeightedAverage <- function(x.1, x.2, weight.1 = 1, weight.2 = 1) {
  assert_that(weight.1 > 0, weight.2 > 0)
  n.1 <- length(x.1)
  n.2 <- length(x.2)
  return((weight.1 * sum(x.1) + weight.2 * sum(x.2)) /
           (weight.1 * n.1 + weight.2 * n.2))
}

#' WeightedSE returns the standard error of weighted average two vectors
#'
#' @param x.1 A numerical vector.
#' @param x.2 A numerical vector.
#' @param weight.1 A positive weight.
#' @param weight.2 A positive weight.
#' @return
#'   The standard error of weighted average of x.1 and x.2.
WeightedSE <- function(x.1, x.2, weight.1 = 1, weight.2 = 1) {
  assert_that(weight.1 > 0, weight.2 > 0)
  n.1 <- length(x.1)
  n.2 <- length(x.2)
  x.bar <- WeightedAverage(x.1, x.2, weight.1, weight.2)
  weighted.sd <- sqrt((weight.1 * sum((x.1 - x.bar) ^ 2) +
      weight.2 * sum((x.2 - x.bar) ^ 2)) / (n.1 + n.2 - 1))
  return(weighted.sd / sqrt(n.1 * weight.1 + n.2 * weight.2))
}
