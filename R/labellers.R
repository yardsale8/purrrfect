#' Labeller helper functions
#'
#' Returns a labeller function with the provided arguements, making it easier
#' to provide a custom labeller.
#'
#' @param labels Data frame of labels. Usually contains only one element, but faceting over multiple factors entails multiple label variables.
#'
#' @param Whether to display the labels of multiple factors on separate lines.
#'
#' @param String separating variables and values.
#'
#' @param Maximum number of characters before wrapping the strip.
#'
#' @return a labeller function
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' mtcars$cyl2 <- factor(mtcars$cyl, labels = c("alpha", "beta", "gamma"))
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#'
#' # Displaying both the values and the variables with default `sep`.
#' p + facet_grid(. ~ cyl, labeller = label_both)
#'
#' # Displaying both the values and the variables with default `sep`.
#' p + facet_grid(. ~ cyl, labeller = label_both_with(sep = "="))
label_value_with <- function( multi_line = TRUE) {
  \(x) ggplot2::label_value(x, multi_line = multi_line)
}

#' @rdname label_value_with
#' @export
label_both_with <- function( multi_line = TRUE, sep = ": ") {
  \(x) ggplot2::label_both(x, multi_line = multi_line, sep = sep)
}

#' @rdname label_value_with
#' @export
label_context_with <- function( multi_line = TRUE, sep = ": ") {
  \(x) ggplot2::label_context(x, multi_line = multi_line, sep = sep)
}

#' @rdname label_value_with
#' @export
label_parsed_with <- function( multi_line = TRUE) {
  \(x) ggplot2::label_parsed(x, multi_line = multi_line)
}

#' @rdname label_value_with
#' @export
label_wrap_gen_with <- function(width = 25, multi_line = TRUE) {
  \(x) ggplot2label_both(x, width = width, multi_line = multi_line)
}
