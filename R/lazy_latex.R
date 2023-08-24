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
    print(kableExtra::kbl(lazy_object, format = "latex", booktabs = TRUE, caption = attr(lazy_object,"lazyhci.caption")))

  if(attr(lazy_object, "lazyhci.latexable") == "text") {
    cat(paste0("\\subsection{", attr(lazy_object, "lazyhci.dv"), "}\n\n"))

  not_significant_rows <- numeric(0)

  for (row in 1:nrow(lazy_object$model$anova_table)) {
    if(lazy_object$model$anova_table[row, "Pr(>F)"] < .05) {
      # significant
      cat(fill_template.internal(get_template("omnibus_significant"), lazy_object = lazy_object, row = row))

      ## print post-hoc

      post_hoc.list <- summary(lazy_object$post_hoc[[rownames(lazy_object$model$anova_table)[row]]][[2]])

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
  ivs <- stringr::str_split_1(rownames(lazy_object$model$anova_table)[row], ":")

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


lazy_latex2 <- function(lazy_object, round.digits = 2, p.val = 0.05) {
  if(is.null(attr(lazy_object, "lazyhci.latexable")))
    stop("I don't know how to handle this object.")

  if(attr(lazy_object, "lazyhci.latexable") == "table")
    print(kableExtra::kbl(lazy_object, format = "latex", booktabs = TRUE, caption = attr(lazy_object,"lazyhci.caption"), digits = round.digits))

  if(attr(lazy_object, "lazyhci.latexable") == "aov") {

    terms.sig <- rownames(lazy_object$model$anova_table %>% dplyr::filter(`Pr(>F)` < p.val))
    terms.nsig <- rownames(lazy_object$model$anova_table %>% dplyr::filter(`Pr(>F)` >= p.val))

    print_omnibus_analysis(lazy_object, "aov", round.digits, terms.sig, terms.nsig, aov_get_test_data, aov_get_post_hoc_string)
  }
}

aov_get_post_hoc_string <- function(lazy_object, term) {
  q_001 <- summary(lazy_object$post_hoc[[term]][[2]]) %>% dplyr::filter(p.value < 0.001)
  q_01 <- summary(lazy_object$post_hoc[[term]][[2]]) %>% dplyr::filter(p.value >= 0.001 & p.value < 0.01)
  q_05 <- summary(lazy_object$post_hoc[[term]][[2]]) %>% dplyr::filter(p.value >= 0.01 & p.value < 0.05)

  res <- c()


  if(nrow(q_001) > 0) {
    for(i in 1:nrow(q_001)) {       # for-loop over rows
      post_hoc <-q_001[i, ]
      str <- paste0(post_hoc[[1]])
      res <- c(res, str)
    }

    res <- c(res, "$(p<.001)$")
  }

  if(nrow(q_01) > 0) {
    for(i in 1:nrow(q_01)) {       # for-loop over rows
      post_hoc <-q_01[i, ]
      str <- paste0(post_hoc[[1]])
      res <- c(res, str)
    }

    res <- c(res, "$(p<.01)$")
  }

  if(nrow(q_05) > 0) {
    for(i in 1:nrow(q_05)) {       # for-loop over rows
      post_hoc <-q_05[i, ]
      str <- paste0(post_hoc[[1]])
      res <- c(res, str)
    }

    res <- c(res, "$(p<.05)$")
  }



  return(stringr::str_flatten_comma(res, "and"))
}

aov_get_test_data <- function(lazy_object, term, round.digits = 2, categorize.p = TRUE) {
  row <- lazy_object$model$anova_table %>%
    dplyr::filter(row.names(lazy_object$model$anova_table) %in% c(term)) %>%
    dplyr::mutate(p = dplyr::if_else(categorize.p,
                                     format_p.internal(`Pr(>F)`),
                                     as.character(round(`Pr(>F)`, digits = round.digits)))) %>%
    dplyr::mutate(omnibus_test = paste0("\\anova{",
                                        round(`num Df`, digits = round.digits), "}{",
                                        round(`den Df`, digits = round.digits), "}{",
                                        round(F, digits = round.digits), "}{",
                                        p,
                                        "}")) %>%
    dplyr::mutate(effect_type = dplyr::if_else(
      length(unlist(stringr::str_split(term, pattern = ":"))) == 1,
      "main",
      "interaction")) %>%
    dplyr::mutate(ivs = stringr::str_flatten_comma(unlist(stringr::str_split(term, pattern = ":")), last = " and ")) %>%
    dplyr::mutate(dv = attr(lazy_object, "lazyhci.dv")) %>%
    dplyr::mutate(es_str = dplyr::if_else(
      ges > 0.26,
      "large",
      dplyr::if_else(ges > 0.13,
                     "medium",
                     "small")
    )) %>%
    dplyr::mutate(
      es = paste0(es_str, " (\\ges{", round(ges, digits = round.digits), "})")
    )

  return(row)
}

print_omnibus_analysis <- function(lazy_object, type, round.digits, terms.sig, terms.nsig, fun_get_test_data, fun_get_post_hoc_string) {
  dv <- attr(lazy_object, "lazyhci.dv")


  cat(paste0("\\subsection{", dv, "}\n\n"))

  cur <- 1

  for(term.sig in terms.sig) {
    if(cur == length(terms.sig))
      starter = get_template("starter_last")
    else
      starter = english::ordinal(cur)

    string <- fill_template2(get_template("omnibus_significant"), fun_get_test_data(lazy_object, term.sig))

    print(stringr::str_to_sentence(paste0(starter, ", ", string)))
    cur <- cur+1


    #post-hoc
    print(fun_get_post_hoc_string(lazy_object,term.sig))
  }

  terms.nsig.main <- terms.nsig %>% stringr::str_subset(":", negate = TRUE)
  terms.nsig.int  <- terms.nsig %>% stringr::str_subset(":")

  nsig.main <- stringr::str_flatten_comma(terms.nsig %>% stringr::str_subset(":", negate = TRUE), last = "and")

  if(length(terms.nsig.main) > 0 && length(terms.nsig.int) > 0 ) {
    cat(get_template("omnibus_not_significant_both"), data.frame(ivs=nsig.main))
  }

  if(length(terms.nsig.main) > 0) {
    cat(get_template("omnibus_not_significant_main"), data.frame(ivs=nsig.main))
  }

  if(length(terms.nsig.int) > 0) {
    cat(get_template("omnibus_not_significant_interaction"))
  }

  #print(fill_template2(get_template("omnibus_not_significant.")), data.frame(ivs=nsig.main))

}

fill_template2 <- function(template, data) {
  return(stringr::str_glue_data(.x = data, template))
}
