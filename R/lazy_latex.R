#' descriptives
#'
#' provides latex code for lazy_objects.
#'
#' Currently supported: lazy_descriptives (table)
#'
#' @param lazy_model an object produced by lazy_hci.
#' @export
lazy_latex <- function(lazy_object) {
  if(is.null(attr(lazy_object, "lazyhci.latexable")))
    stop("I don't know how to handle this object.")

  if(attr(lazy_object, "lazyhci.latexable") == "table")
    kableExtra::kbl(lazy_object, format = "latex", booktabs = TRUE, caption = attr(lazy_object,"lazyhci.caption"))

  if(attr(lazy_object, "lazyhci.latexable") == "text") {
    cat(paste0("\\subsection{", attr(lazy_object, "lazyhci.dv"), "}"))



  }
}

build_latex_omnibus.internal <- function(df1, df2, F, p, es) {
  return(paste0("\\anova{", df1, "}{", df2, "}{", F, "}{", p, "}{", es, "}"))
}
