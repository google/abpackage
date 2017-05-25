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
                          n.nodes = 50,
                          ci.level = 0.95) {
  kNRows <- 2
  empty.output <- data.frame(lower = rep(NA, kNRows),
                             center = rep(NA, kNRows),
                             upper = rep(NA, kNRows),
                             mean = rep(NA, kNRows),
                             var = rep(NA, kNRows),
                             p.value = rep(NA, kNRows),
                             type = c("percent.change", "difference"))

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
    n.pre <- length(data$pre)
    mu.pre <- NonStdStudentTQuantiles(unif.nodes,
                                      n.pre - 1,
                                      sd(data$pre) / sqrt(n.pre),
                                      mean(data$pre))

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
    pc.df <- ComputeCI(percent.change, ci.level)
    pc.df$mean <- mean(percent.change)
    pc.df$var <- var(percent.change)
    pc.df$p.value <- ComputePValue(percent.change)
  } else {
    pc.df <- data.frame(lower = NA,
                        center = NA,
                        upper = NA,
                        mean = NA,
                        var = NA,
                        p.value = NA)
  }
  pc.df$type <- "percent.change"
  # Compute ci and p-value for difference between treatment mean and control
  # mean.
  difference <- mus$trmt - mus$ctrl
  d.df <- ComputeCI(difference, ci.level)
  d.df$mean <- mean(difference)
  d.df$var <- var(difference)
  d.df$p.value <- ComputePValue(difference)
  d.df$type <- "difference"

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

#' Compute credible interval from a vector of data
#'
#' @param data A vector.
#' @param ci.level The confidence level for the credible intervals.
#' @return
#'   A data.frame with lower, center and upper values for the credible interval.
ComputeCI <- function(data, ci.level) {
  alpha <- 1 - ci.level
  return(data.frame(lower = quantile(data, alpha / 2),
                    center = quantile(data, 0.5),
                    upper = quantile(data, 1 - alpha / 2)))
}
