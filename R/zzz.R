
#' @method print slack.block.element    
#' @export 
print.slack.block.element <- function(x, ...) {
  if (is.list(x)) attr(x, "class") <- NULL
  print.default(x, ...)
}

#' @method print slack.composition.object    
#' @export 
print.slack.composition.object <- function(x, ...) {
  if (is.list(x)) attr(x, "class") <- NULL
  print.default(x, ...)
}

#' @method print slack.block.object    
#' @export
print.slack.block.object <- function(x, ...) {
  if (is.list(x)) attr(x, "class") <- NULL
  print.default(x, ...)
}

#' @method print slack.view.object    
#' @export
print.slack.view.object <- function(x, ...) {
  if (is.list(x)) attr(x, "class") <- NULL
  print.default(x, ...)
}