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
    cat(paste0("\\subsection{", attr(lazy_object, "lazyhci.dv"), "}\n\n"))

  not_significant_rows <- numeric(0)

  for (row in 1:nrow(lazy_object$model$anova_table)) {
    if(lazy_object$model$anova_table[row, "Pr(>F)"] < .05) {
      # significant
      cat(fill_template.internal(get_template("omnibus_significant"), lazy_object = lazy_object, row = row))

      ## print post-hoc

      post_hoc.list <- summary(demo1.ana$post_hoc[[rownames(demo1.ana$model$anova_table)[row]]][[2]])

      for(post_hoc_row in nrow(post_hoc.list)) {
        if(post_hoc.list[post_hoc_row, "p.value"] < 0.05) {
          cat("Post-hoc tests confirmed significantly ($p", format_p.internal(post_hoc.list[post_hoc_row, "p.value"]) ,"$) higher {dv} for {left} compared to {right}." )
        }
      }

    } else {
      not_significant_rows <- c(not_significant_rows, row)
    }
  }

  ##handle not significant effects.



  }
}

template.variables <- c("omnibus_test", "effect_type", "ivs", "dv", "es")

fill_template.internal <- function(template, lazy_object, row) {
  table <- lazy_object$model$anova_table

  ##ominbus test
  template <- stringr::str_replace_all(template,
                           pattern = stringr::fixed("{omnibus_test}"),
                           replacement = build_latex_omnibus.internal(table[row, "num Df"], table[row, "den Df"], table[row, "F"], table[row, "Pr(>F)"]))

  ## eff_type
  ivs <- stringr::str_split_1(rownames(demo1.ana$model$anova_table)[row], ":")

  eff_type <- "main"
  ivs_pre <- "the"

  if(length(ivs) > 1) {
    eff_type <- "interaction"
    ivs_pre <- "between"
  }

  template <- stringr::str_replace_all(template,
                                       pattern = stringr::fixed("{effect_type}"),
                                       replacement = eff_type)

  template <- stringr::str_replace_all(template,
                                       pattern = stringr::fixed("{ivs}"),
                                       replacement = paste0(ivs_pre, " ", knitr::combine_words(ivs)))

  template <- stringr::str_replace_all(template,
                                       pattern = stringr::fixed("{dv}"),
                                       replacement = attr(lazy_object, "lazyhci.dv"))
  ##es


  # .02 as small, one  of .13 as medium, and one of .26 as large. It seems appropriate to apply the same guidelines to η2 G as well.
  ges <- table[row, "ges"]

  ges_str <- "large"

  if(ges < 0.26)
    ges_str <- "medium"
  else if(ges < 0.13)
    ges_str <- "small"

  ges_str <- paste0(ges_str, " (\\ges{", ges, "})")

  template <- stringr::str_replace_all(template,
                                       pattern = stringr::fixed("{es}"),
                                       replacement = ges_str)

  return(template)
}

build_latex_omnibus.internal <- function(df1, df2, F, p) {
  return(paste0("\\anova{", df1, "}{", df2, "}{", F, "}{", p, "}"))
}

get_template <- function(template_type) {
  file <- system.file(file.path('latex', paste0(template_type, '.tex_template')), package = "lazyhci")
  lines <- readLines(file)
  return(sample(lines, 1))
}

format_p.internal <- function(p){
  if(p<0.001)
    return("<.001")
  else if(p<0.01)
    return("<.01")
  else if(p<0.05)
    return("<.05")
  else
    return(">.05")
}
