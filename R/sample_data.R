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

#' Generates synthetic data from Gaussian distributions
#'
#' @param n.metrics Number of metrics.
#' @param n.observations Number of observations.
#' @param mu.pre Vector or scalar representing the mean during the pre-period
#'   for each metric.
#' @param sigma.pre Vector or scalar representing the standard deviation during
#'   the pre-period for each metric.
#' @param rho.ctrl Vector or scalar representing the correlation between the
#'   pre-period and the post-period for the control group for each metric.
#' @param rho.trmt Vector or scalar representing the correlation between the
#'   pre-period and the post-period for the treatment group for each metric.
#'   If missing, set equal to rho.ctrl.
#' @param mu.ctrl Vector or scalar representing the mean during the post-period
#'   for the control group for each metric. If missing, set equal to mu.pre.
#' @param sigma.ctrl Vector or scalar representing the standard deviation during
#'   the post-period for the control group for each metric. If missing, set
#'   equal to sigma.pre.
#' @param mu.trmt Vector or scalar representing the mean during the post-period
#'   for the treatment group for each metric. If missing, set equal to mu.pre.
#' @param sigma.trmt Vector or scalar representing the standard deviation
#'   during the post-period for the treatment group for each metric. If missing,
#'   set equal to sigma.pre.
#' @param spread If TRUE, the output has a horizontal structure
#'   with a \code{pre.post} column and column for each metric.
#'   If FALSE, the output has a vertical structure with a
#'   \code{metric} column, a \code{pre} column and a \code{post} column.
#' @export
#' @return A \code{data.frame} which can be used as input for the \code{PrePost}
#'   and the \code{PreCheck} functions.
#' @examples
#'   data <- SampleData(n.metrics = 10)
#'   ans <- PrePost(data)
#'
#'   n.metrics <- 10
#'   mu.pre <- 100
#'   mu.ctrl <- 100
#'   mu.trmt <- c(110, rep(100, n.metrics - 1))
#'   data <- SampleData(n.metrics = n.metrics,
#'                      mu.pre = mu.pre,
#'                      mu.ctrl = mu.ctrl,
#'                      mu.trmt = mu.trmt)
#'   ans <- PrePost(data)
#'
#'   data <- SampleData(n.metrics = n.metrics,
#'                      mu.pre = mu.pre,
#'                      mu.ctrl = mu.ctrl,
#'                      mu.trmt = mu.trmt,
#'                      spread = TRUE)
#'   std.data <- ReshapeData(data)
#'   ans <- PrePost(std.data)
SampleData <- function(n.metrics = 100,
                       n.observations = 20,
                       mu.pre = 100,
                       sigma.pre = 1,
                       rho.ctrl = 0.8,
                       rho.trmt = NULL,
                       mu.ctrl = NULL,
                       sigma.ctrl = NULL,
                       mu.trmt = NULL,
                       sigma.trmt = NULL,
                       spread = FALSE) {

  assert_that(n.metrics >= 1)
  if (n.observations < 10) {
    warning("Estimates can be unstable for n.observations < 10.")
  }
  assert_that(all(rho.ctrl >= 0), all(rho.ctrl <= 1))
  assert_that(all(sigma.pre > 0))
  assert_that(all(sigma.ctrl > 0))
  # Assign default values for NULL input parameters.
  if (is.null(rho.trmt)) {
    rho.trmt <- rho.ctrl
  }
  if (is.null(mu.ctrl)) {
    mu.ctrl <- mu.pre
  }
  if (is.null(mu.trmt)) {
    mu.trmt <- mu.pre
  }
  if (is.null(sigma.ctrl)) {
    sigma.ctrl <- sigma.pre
  }
  if (is.null(sigma.trmt)) {
    sigma.trmt <- sigma.pre
  }
  assert_that(all(rho.trmt >= 0), all(rho.trmt <= 1))
  assert_that(all(sigma.trmt > 0))

  # Create a data.frame with the following columns: condition, observation,
  # metric, pre and post.
  output <- data.frame(mu.pre = Vectorize(mu.pre, n.metrics),
                       sigma.pre = Vectorize(sigma.pre, n.metrics),
                       mu.trmt = Vectorize(mu.trmt, n.metrics),
                       sigma.trmt = Vectorize(sigma.trmt, n.metrics),
                       mu.ctrl = Vectorize(mu.ctrl, n.metrics),
                       sigma.ctrl = Vectorize(sigma.ctrl, n.metrics),
                       mu.ctrl = Vectorize(mu.ctrl, n.metrics),
                       sigma.ctrl = Vectorize(sigma.ctrl, n.metrics),
                       rho.ctrl = Vectorize(rho.ctrl, n.metrics),
                       rho.trmt = Vectorize(rho.trmt, n.metrics),
                       metric = MetricNames(n.metrics),
                       stringsAsFactors = FALSE) %>%
      dplyr::group_by(metric) %>%
      do(SampleMetricData(., n.observations)) %>%
      dplyr::ungroup() %>%
      as.data.frame()

  if (spread) {
      # Reshape the data.frame to have the following columns: condition,
      # observation, pre.post and a column for each metric.
      output %<>% tidyr::gather(pre.post, value, pre:post) %>%
          tidyr::spread(metric, value)
  } else {
    output %<>% dplyr::select(-observation)
  }

  return(output)
}

SampleMetricData <- function(data, n) {
  # Generate data for a given metric.

  # Sample data for the treatment group.
  trmt <- SampleGroupData(data$mu.pre,
                          data$sigma.pre,
                          data$mu.trmt,
                          data$sigma.trmt,
                          data$rho.trmt,
                          n) %>%
      dplyr::mutate(condition = rep("treatment", n),
                    observation = seq(1, n))
  # Sample data for the control group.
  ctrl <- SampleGroupData(data$mu.pre,
                          data$sigma.pre,
                          data$mu.ctrl,
                          data$sigma.ctrl,
                          data$rho.ctrl,
                          n) %>%
      dplyr::mutate(condition = rep("control", n),
                    observation = seq(1, n))
  # Stack the data from the two groups.
  return(dplyr::bind_rows(trmt, ctrl))
}

SampleGroupData <- function(mu.pre, sigma.pre, mu.post, sigma.post, rho, n) {
  # Generate data for pre-period and experiment period from bivariate
  # normal distribution.
  pre <- rnorm(n, mu.pre, sigma.pre)
  post <- rnorm(n,
                mu.post + sigma.post / sigma.pre * rho * (pre - mu.pre),
                sqrt((1 - rho ^ 2)) * sigma.post)
  return(data.frame(pre = pre, post = post, stringsAsFactors = FALSE))
}

Vectorize <- function(x, n) {
  # If x is a scalar, returns a vector of length n where all elements
  # are equal to x. If x is a vector, checks that the length of the vector is
  # n and returns the vector.

  if (length(x) == 1) {
    return(rep(x, n))
  } else if (length(x) == n) {
    return(x)
  } else {
    stop(sprintf("Input must be a scalar or have length %s", n))
  }
}

MetricNames <- function(n) {
  # Create a vector of metric names. The metric numbers have leading zero(s) to
  # guarantee ordering in printing and plotting.
  assert_that(n >= 1)
  metrics <- paste("metric", formatC(seq_len(n),
                                     width = floor(log10(n)) + 1,
                                     flag = "0"))
  return(metrics)
}
