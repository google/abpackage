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

RenameColNames <- function(data,
                           current.names,
                           new.names) {
  # Changes the name of the variables from current.names to new.names.

  if (length(current.names) != length(new.names)) {
    stop("current.names and std.names must have the same length")
  }
  for (i in 1:length(current.names)) {
    assert_that(has_name(data, current.names[i]))
    # Replace current name with new name.
    names(data)[names(data) == current.names[i]] <- new.names[i]
  }
  return(data)
}

HasVariableWithSomeLevels <- function(data, variable, levels) {
  return(all(unique(data[[variable]]) %in% levels))
}

HasVariableWithAllLevels <- function(data, variable, levels) {
  return(setequal(levels, unique(data[[variable]])))
}

on_failure(HasVariableWithSomeLevels) <- function(call, env) {
  df <- eval(call$data, env)
  variable <- eval(call$variable, env)
  unique.levels <- unique(df[[variable]])
  levels <- eval(call$levels, env)
  paste0("variable ", variable,
         " (levels: ",
         paste(unique.levels, collapse = ", "),
         ") should have some of these levels: ",
         paste(levels, collapse = ", "))
}

on_failure(HasVariableWithAllLevels) <- function(call, env) {
  df <- eval(call$data, env)
  variable <- eval(call$variable, env)
  unique.levels <- unique(df[[variable]])
  levels <- eval(call$levels, env)
  paste0("variable ", variable,
         " (levels: ",
         paste(unique.levels, collapse = ", "),
         ") should have all of these levels: ",
         paste(levels, collapse = ", "))
}

StandardizeLevelNames <- function(data,
                                  variable.name,
                                  current.levels,
                                  std.levels,
                                  sub.set = FALSE) {
  # Changes the levels of the variable variable.name from current.levels
  # to std.levels.

  if (sub.set) {
    assert_that(HasVariableWithSomeLevels(data, variable.name, current.levels))
  } else {
    assert_that(HasVariableWithAllLevels(data, variable.name, current.levels))
  }
  # Check that the number of current levels is equal to the number of
  # default levels.
  if (length(current.levels) != length(std.levels)) {
    stop(sprintf("Variable %s must have %d levels",
                 paste(variable.name, "levels", sep = "."),
                 length(std.levels)))
  }
  # Replace current levels with default levels.
  data[, variable.name] <- factor(data[, variable.name],
                                  levels = current.levels)
  levels(data[, variable.name]) <- std.levels
  # Remove factors.
  data[, variable.name] <- as.character(data[, variable.name])
  return(data)
}

CINames <- function(ci.level) {
  # Maps lower, center and upper to quantiles for a given coverage level.
  assert_that(ci.level > 0, ci.level < 1)
  alpha <- 1 - ci.level
  num.names <- paste0(100 * c(alpha / 2, 0.5, 1 - alpha / 2), '%')
  return(data.frame(num = num.names,
                    string = c("lower", "center", "upper"),
                    stringsAsFactors = FALSE))
}

on_failure(has_name) <- function(call, env) {
  paste0(deparse(call$x), " does not have variable ", eval(call$which, env))
}
