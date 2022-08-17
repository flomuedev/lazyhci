#' analyze
#'
#' This function conducts (non-) parametric analyzes of (multi-) factorial experiments
#' @param data the data
#' @param participant the column name of the participant identifier
#' @param DV the column name of the dependent variable
#' @param within.vars (optional) a list of within-subject variables
#' @param between.vars (optional) a list of between-subject variables
#' @param analysis_type one of "parametric" or "non-parametric". Defaults to "parametric"
#' @param posthoc.adj the adjustment method for post-hoc tests. Defaults to "bonferroni"
#' @param anova.type the type of anova test to perform. Defaults to 3
#' @param transformation (optional) a transformation that should be applied to the data before the test.
#' @param es the type of effect size to report, one of "ges", "pes" or "none".
#' @param ignore_design_not_complete if true, allows to test incomplete designs.
#'
#' @export
analyze <- function(data, participant, DV, within.vars = NULL, between.vars = NULL, analysis_type=c("parametric", "non-parametric"), posthoc.adj="bonferroni", anova.type = 3, transformation = NULL, es = c("ges", "pes", "none"), ignore_design_not_complete = FALSE) {

  if(!hasArg(analysis_type)) {
    analysis_type <- "parametric"
    message("No analysis_type specified, defaulting to parametric analyzis.")
  }

  data <- fix_col_names(data)
  participant <- fix_strings(participant)
  DV <- fix_strings(DV)
  within.vars <- fix_strings(within.vars)
  between.vars <- fix_strings(between.vars)

  if(!check_complete_design(data, within.vars = within.vars, between.vars = between.vars, participantCol = participant, DV = DV)) {
    warning("This is not a complete design. Please check the output of check_complete_design to find out what is broken.")

    if(!ignore_design_not_complete) {
      stop("Depending on your design, some tests might still yield results. You can ignore this error by setting the 'ignore_design_not_complete' flag.")
    }
  }


  switch(rlang::arg_match(analysis_type),
         "parametric"={

           model <- aov_afex.internal(data = data, DV = DV, participant = participant, within.vars = within.vars, between.vars = between.vars, posthoc.adj = posthoc.adj, anova.type = anova.type, transformation = transformation, es = es)

           ##check assumptions

           if(model$normality_test[1] < 0.05) {
             warning("The test for normality of the residuals is significant. This might indicate that your data is not normaly distributed. Please check the histogram for a more detailed view (plot(x$normality_test)). You can add a transformation (e.g., log) to your data and check if this helps or switch to a non-parametric analyzis.")
           } else {
             print(paste0("Test for normality of the residuals: OK (p = ", model$normality_test, ")" ))
           }

           if(!is.null(within.vars)) {
             if(length(model$sphericity_test[model$sphericity_test<0.05 ]) > 0) {
               message("The tests for sphericity is significant for at least one variable. This is not a big issue, we'll correct the results later on.")
             } else {
               print(paste0("Test for sphericity: OK for all variables"))
             }

           }


           if(!is.null(between.vars)) {
             if(model$homogeneity_test[1] < 0.05) {
               warning("The assumption of homogeneity of variances is not met. Consider using a non-parametric test.")
             } else {
               print(paste0("Test for the homogeneity of the variances: OK (p = ", model$homogeneity_test, ")" ))
             }
           }



         },
         "non-parametric"={
           stop("Non-parametric analyzes are not implemented yet :(")

           if(!is.null(within.vars) & !is.null(between.vars)) {
             stop("non-parametric within/between not supported yet.")
           } else if(!is.null(between.vars)) {
             if(length(t) == 1) {
               print("do kruskal wallis not yet implemented.")


             } else if(length(t) > 1) {
               stop("non-parametric multi factorial between-subjects analysis not supported yet.")
             }
           }

         },
         {
           stop("I don't know that type.")
         }
  )

  #model$nice_table$text <- paste0("F(", model$nice_table$df1, ",", model$nice_table$df2, ") = ", model$nice_table$F, ", p = ", model$nice_table$p.value)
  model$nice_table$latex <- paste0("\ano{", model$nice_table$df1, "}{", model$nice_table$df2, "}{", model$nice_table$F, "}{", model$nice_table$p.value)

  if(es == "ges") {
    model$nice_table$latex <- paste0(model$nice_table$latex, "}{", model$nice_table$ges, "}{ges")
  }

  if(es == "pes") {
    model$nice_table$latex <- paste0(model$nice_table$latex, "}{", model$nice_table$pes, "}{pes")
  }

  model$nice_table$latex <- paste0(model$nice_table$latex, "}")

  ##post hoc

  ##post hoc
  model$nice_table.filtered <- model$nice_table %>% filter(as.numeric(p.value) < 0.05)
  if(nrow(model$nice_table.filtered) > 0) {
    post_hoc_tests <- split(model$nice_table.filtered, 1:nrow(model$nice_table.filtered))
    names(post_hoc_tests) <- model$nice_table.filtered$Effect
    post_hoc_tests <- lapply(post_hoc_tests, run_post_hoc.internal, model = model$model)

    model <- append(model, list(post_hoc=post_hoc_tests))
  }

  return(model)

}

aov_art.internal <- function(data, DV, participant, within.vars = NULL, between.vars = NULL, posthoc.adj="bonferroni", anova.type, transformation, es = c("ges", "pes", "none")) {
  require(ARTool)
}

#' @noRd
aov_afex.internal <- function(data, DV, participant, within.vars = NULL, between.vars = NULL, posthoc.adj="bonferroni", anova.type, transformation, es = c("ges", "pes", "none")) {
  require(afex)
  require(performance)
  require(qqplotr)

  es <- rlang::arg_match(es)
  print(es)
  result <- list()

  afex_options(
    correction_aov = "GG",
    emmeans_model = "multivariate"
  )


  data <- fix_col_names(data)
  DV <- fix_strings(DV)
  participant <- fix_strings(participant)
  within.vars <- fix_strings(within.vars)
  between.vars <- fix_strings(between.vars)

  aov_ez_string <- "afex::aov_ez(id = participant, dv = DV, data = data, type = anova.type, include_aov = TRUE, es = es"

  if(!is.null(within.vars))
    aov_ez_string <- paste0(aov_ez_string, ", within = within.vars")

  if(!is.null(between.vars))
    aov_ez_string <- paste0(aov_ez_string, ", between = between.vars")

  if(!is.null(transformation))
    aov_ez_string <- paste0(aov_ez_string, ", transformation = transformation")

  aov_ez_string <- paste0(aov_ez_string, ")")

  print(aov_ez_string)

  aov.model <- eval(parse(text=aov_ez_string))

  result <- append(result, list(model=aov.model))

  ##assumption checks
  result <- append(result, list(normality_test=check_normality(aov.model)))

  if(!is.null(within.vars)) {
    result <- append(result, list(sphericity_test=check_sphericity(aov.model)))
  }

  if(!is.null(between.vars)) {
    result <- append(result, list(homogeneity_test=check_homogeneity(aov.model)))
  }

  nice.table <- nice(aov.model, es = es)

  nice.table$df1 <- str_split(nice.table$df, ", ", simplify = TRUE)[,1]
  nice.table$df2 <- str_split(nice.table$df, ", ", simplify = TRUE)[,2]

  result <- append(result, list(nice_table=nice.table))

  return(result)
}

#' @noRd
run_post_hoc.internal <- function(row, model) {
  require(emmeans)

  emm <- eval(parse(text=paste0("emmeans::emmeans(model, ~",row$Effect,")")))

  return(summary(pairs(emm)))
}
