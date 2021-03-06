#' rcvirtual: foundations for statistical models with R reference classes
#'
#' This package establishes the foundations of several reference classes that
#' can be employed to set up, fit and explore a statistical model
#'
#' The following virtual reference classes are included in this package:
#' 1) the daemon is the RC that has overall control over the model, and is the
#' entity that users should interact with to get things done;
#'
#' 2) the factory RC creates appropriate strategy and parameter objects and
#' passes them to the daemon;
#'
#' 3) parameter RCs contain all the information about model parameters;
#'
#' 4) strategy RCs define how the model is fitted
#'
#' 5) plotter RCs specify plotting functions
#'
#' @docType package
#' @name rcvirtual
NULL
#> NULL
