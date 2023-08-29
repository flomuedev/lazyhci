#' descriptives
#'
#' calculate descriptives for all subsets
#'
#'
#' @param lazy_model the data in tidy format
#' @param dv the name of the column containing the dependent variable
#' @param ivs (optional) a vector of strings containing names of the columns identifying the independent variables to plot
#'
#' @export
lazy_descriptives <- function(lazy_model, dv, ivs = NULL) {

  assert_lazy_model.internal(lazy_model)
  pkg.env$assert_colnames_quietly(lazy_model$source$data, dv, only_colnames=FALSE)
  checkmate::assert_character(ivs, null.ok = TRUE)

  if(is.null(ivs)) {
    IVs <- lazy_model$ivs
    IVs.pretty <- c(lazy_model$source$within.vars, lazy_model$source$between.vars)
  } else {
    IVs.pretty <- ivs
    IVs <- janitor::make_clean_names(ivs)
  }

  DV.pretty <- dv
  dv <- janitor::make_clean_names(dv)

  groupnames.sets <- list_all_subsets(IVs)
  groupnames.sets <- groupnames.sets[lapply(groupnames.sets,length)>0]

  result <- list()

  for(set in groupnames.sets) {
    vars <- as.list(set)
    name = paste(vars, collapse ="_x_")

    tmp <- lazy_model$data %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(unlist(vars)))) %>%
      dplyr::summarize(n = dplyr::n(),
                mean = mean(!!as.name(dv)),
                sd=sd(!!as.name(dv)),
                se=se(!!as.name(dv)),
                min=min(!!as.name(dv)),
                max=max(!!as.name(dv)),
                median=median(!!as.name(dv)),
                Q1=quantile(!!as.name(dv))[2],
                Q3=quantile(!!as.name(dv))[4],
      )

    if(length(vars) == 1)
      IVstr <- vars[1]
    else
      IVstr <- paste0(paste(vars[1:length(vars)-1], collapse = ", "), " and ", vars[length(vars)])

    attr(tmp, 'lazyhci.latexable') <- "table"
    attr(tmp, 'lazyhci.caption') <- paste0("The ", dv, " grouped by ", IVstr, ".")

    result[[name]] <- tmp
  }

  class(result) <- "lazyhci_descriptives"

  return(result)
}

list_all_subsets <- function(x) {
  return (unlist(lapply(1:length(x),    # Get all combinations
                combinat::combn,
                x = x,
                simplify = FALSE),
         recursive = FALSE))
}

#' @export
print.lazyhci_descriptives <- function(x, ...){
  cli::cli_h1("Descriptives")

  for(name in names(x)) {
    cli::cli_h2(name)

    print(x[[name]])
    cat("\n")
  }
}

#lazy_latex <- function(lazy_descriptive) {
#    kableExtra::kbl(lazy_descriptive, format = "latex", booktabs = TRUE, caption = attr(lazy_descriptive,"lazyhci.caption"))
#}
