#' descriptives
#'
#' provides latex code for lazy_objects.
#'
#' Currently supported: lazy_descriptives (table)
#'
#' @param lazy_model an object produced by lazy_hci.
#' @export
lazy_latex <- function(lazy_object) {
  kableExtra::kbl(lazy_object, format = "latex", booktabs = TRUE, caption = attr(lazy_object,"lazyhci.caption"))
}
