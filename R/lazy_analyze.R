#' lazy_analyze2
#'
#' This function conducts (non-) parametric analyzes of (multi-) factorial experiments
#' @param lazy_model the data model
#' @param dv the column name of the dependent variable
#' @param analysis_type one of "aov", "art", "friedman", "lme", "glme".
#' @param posthoc.adj the adjustment method for post-hoc tests. Defaults to "bonferroni"
#' @param anova.type the type of anova test to perform. Defaults to 3
#' @param transformation (optional) a transformation that should be applied to the data before the test.
#'
#' @export
lazy_analyze2<- function(lazy_model, dv, analysis_type=c("aov", "art", "lme", "glme", "friedman"), posthoc.adj = "bonf", anova.type = 3, transformation = NULL, family = c("poisson", "binominal"), nAGQ=NULL) {

  analysis_type <- match.arg(analysis_type)

  data.clean <- lazy_model$data
  participant.clean <- lazy_model$participant
  DV.clean <- janitor::make_clean_names(dv)
  within.vars.clean <- lazy_model$within.vars
  between.vars.clean <- lazy_model$between.vars

  result <- list()
  attr(result, 'lazyhci.anatype') <- analysis_type

  result[["descriptives"]] <- lazy_descriptives(lazy_model, dv)

  if(analysis_type == "aov") {
    model <- fit_model_afex.internal(data = data.clean, participant = participant.clean, DV = DV.clean, within.vars = within.vars.clean, between.vars = between.vars.clean, anova.type = anova.type, transformation = transformation)
    result[["model"]] <- model
    result[["post_hoc"]] <- do.post_hoc.internal(model,
                                                 rownames(model$anova_table %>% dplyr::filter(`Pr(>F)` < 0.05)),
                                                 posthoc.adj, fct_contrasts = post_hoc.internal, fct_ip = interaction_plot.aov.internal)

    result[["normality_test"]] <- performance::check_normality(model)

    if(!is.null(within.vars.clean)) {
      result[["sphericity_test"]] <- performance::check_sphericity(model)
    }

    if(!is.null(between.vars.clean)) {
      result[["homogeneity_test"]] <- performance::check_homogeneity(model)
    }

  }

  if(analysis_type == "art") {
    model <- aov_art_fit.internal(data = data.clean, DV = DV.clean, participant = participant.clean, within.vars = within.vars.clean, between.vars = between.vars.clean)
    model$anova <-  anova(model$m.aov)
    model$anova$eta.sq <- with(model$anova, `Sum Sq`/(`Sum Sq` + `Sum Sq.res`))
    result[["model"]] <- model
    result[["post_hoc"]] <- do.post_hoc.internal(model$m.lme, model$anova$Term, posthoc.adj, fct_contrasts = post_hoc.art.internal, fct_ip = interaction_plot.art.internal)
  }

  if(analysis_type == "lme" || analysis_type == "glme") {
    model <- lmer.fit.inernal(data = data.clean, DV = DV.clean, participant = participant.clean, within.vars = within.vars.clean, between.vars = between.vars.clean, analysis_type = analysis_type, glme.family = family, nAGQ = nAGQ)
    result[["anova"]] <-  car::Anova(model, type=anova.type)
    result[["model"]] <- model
    result[["post_hoc"]] <- do.post_hoc.internal(model, rownames(result[["anova"]][!(row.names(result[["anova"]]) %in% c("(Intercept)")), ]), posthoc.adj, fct_contrasts = post_hoc.internal, fct_ip = interaction_plot.aov.internal)
  }

  if(analysis_type == "friedman") {

    if(!((length(within.vars.clean) == 1) && (length(between.vars.clean) == 0))) {
      stop("friedman analysis only applies to one within and zero between vars .")
    }

    #friedman.test(x ~ w | t, data = wb)

    friedman <- friedman.test(as.formula(paste0(DV.clean, " ~ ", within.vars.clean[[1]], " | ", participant.clean)), data = data.clean)
    post_hoc_friedman <- PMCMRplus::frdAllPairsConoverTest(y = data.clean[[DV.clean]], groups = data.clean[[within.vars.clean[[1]]]],
                                                           blocks = data.clean[[participant.clean]], p.adjust.method = posthoc.adj)

    result[["friedman"]] <- friedman
    result[["post_hoc"]] <- post_hoc_friedman
  }

  class(result) <- "lazyhci_analysis"
  attr(result, 'lazyhci.latexable') <- "text"
  attr(result, 'lazyhci.dv') <- dv

  return(result)
}

make.factors.internal <- function(data.clean, vars) {
  for(var in vars) {
    if(!is.factor(data.clean[[var]])) {
      warning(paste0("auto-converting column ", var, " to a factor."))
      data.clean[[var]] <- as.factor(data.clean[[var]])
    }
  }

  return(data.clean)
}

do.post_hoc.internal <- function(model, terms, posthoc.adj = "bonf", fct_contrasts, fct_ip) {

  result <- list()

  for(term in terms) {

    print(unlist(stringr::str_split(term, ":")))

    tmp <- fct_contrasts(model, unlist(stringr::str_split(term, ":")), posthoc.adj)
    interaction.plot <- fct_ip(tmp[[1]], unlist(stringr::str_split(term, ":")))

    tmp[["interaction_plot"]] <- interaction.plot

    result[[term]] <- tmp
  }

  class(result) <- "lazyhci_post_hoc_list"

  return(result)
}

post_hoc.art.internal <- function(model, factors, posthoc.adj = "bonf") {
  artlm <- ARTool::artlm.con(model, paste(factors, collapse = ":"))
  result <- post_hoc.internal(artlm, factors, posthoc.adj, collapse = "")

  return(result)
}

post_hoc.internal <- function(model, factors, posthoc.adj = "bonf", collapse=":") {

  form_string_emm <- paste0("pairwise ~ ", paste(factors, collapse = collapse))

  result <- emmeans::emmeans(model, list(as.formula(form_string_emm)), adjust = "bonf")

  return(result)
}

lmer.fit.inernal <- function(data, participant, DV, within.vars = NULL, between.vars = NULL, analysis_type, anova.type = 3, glme.family = NULL, nAGQ = NULL) {

  if(analysis_type == "glme" && !hasArg(analysis_type))
    stop("glme requires the family argument.")

  formula.string <- paste(
    DV,
    " ~ ",
    "(",
    paste(c(within.vars, between.vars), collapse ="*"),
    ")",
    " + ",
    "(1|",
    participant,
    ")",
    sep=""
  )

  print(paste0("fitting model for ", formula.string))

  if(analysis_type == "lme")
    model <- lme4::lmer(as.formula(formula.string), data=data)

  print(nAGQ)

  if(analysis_type == "glme") {
    if(is.null(nAGQ))
      model <- lme4::glmer(as.formula(formula.string), data=data, family = glme.family)
    else
      model <- lme4::glmer(as.formula(formula.string), data=data, family = glme.family, nAGQ = nAGQ)
  }

  print("finished fitting model")


  return(model)
}

fit_model_afex.internal <- function(data, participant, DV, within.vars = NULL, between.vars = NULL, transformation = NULL, anova.type = 3, es = "ges") {

  afex_options(
    correction_aov = "GG",
    emmeans_model = "multivariate"
  )

  aov_ez_string <- "afex::aov_ez(id = participant, dv = DV, data = data, type = anova.type, include_aov = TRUE, es = es"

  if(!is.null(within.vars))
    aov_ez_string <- paste0(aov_ez_string, ", within = within.vars")

  if(!is.null(between.vars))
    aov_ez_string <- paste0(aov_ez_string, ", between = between.vars")

  if(!is.null(transformation))
    aov_ez_string <- paste0(aov_ez_string, ", transformation = transformation")

  aov_ez_string <- paste0(aov_ez_string, ")")

  #message(paste0("Fitting AOV Model using afex for ", aov_ez_string))

  aov.model <- eval(parse(text=aov_ez_string))

  return(aov.model)
}

aov_art_fit.internal <- function(data, DV, participant, within.vars = c(), between.vars = c()) {

  data.internal <- as.data.frame(data)

  formula.aov <- paste(
    DV,
    " ~ ",
    "(",
    paste(c(within.vars, between.vars), collapse ="*"),
    ")",
    " + ",
    "Error(",
    participant,
    "/",
    "(",
    paste(within.vars, collapse ="*"),
    "))",
    sep=""
  )

  formula.lme <- paste(
    DV,
    " ~ ",
    "(",
    paste(c(within.vars, between.vars), collapse ="*"),
    ")",
    " + ",
    "(1|",
    participant,
    ")",
    sep=""
  )

  print(formula.aov)

  m.aov <- ARTool::art(as.formula(formula.aov), data=data)
  m.lme <- ARTool::art(as.formula(formula.lme), data=data)


  return(list("m.aov" = m.aov, "m.lme" = m.lme))

}

interaction_plot.aov.internal <- function(emmeans_model, factors) {

  if(length(factors) > 3)
    return(NULL)

  if(length(factors) == 1) {
    formula2 <- as.formula(paste("~", factors[[1]], sep=""))
  }
  if(length(factors) == 2) {
    formula2 <- as.formula(paste(factors[[1]], "~", factors[[2]], sep=""))
  } else if(length(factors) == 3) {
    formula2 <- as.formula(paste(factors[[1]], "~", factors[[2]], " | ", factors[[3]], sep=""))
  }

  return(emmeans::emmip(emmeans_model, formula2))
}

interaction_plot.art.internal <- function(emmeans_model, factors) {

  if(length(factors) > 3)
    return(NULL)

  colname <- paste(factors, collapse = "")

  emmeans_model <- as.data.frame(emmeans_model) %>%
    separate(!!as.name(colname), factors, sep = ",")

  aes_ggplot_arguments <- list(
    x=factors[[1]],
    y="emmean"
  )

  tvar <- factors[[1]]

  if(length(factors) > 1)
    tvar <- factors[[2]]


  plot <- ggplot(data=emmeans_model, aes_string(x=factors[[1]], y="emmean", colour = tvar)) +
    geom_point() +
    geom_linerange(
      aes_string(ymin = "lower.CL", ymax = "upper.CL")
    ) +
    geom_line(
      aes_string(
        group = tvar
      )
    )

  if(length(factors) == 3) {
    plot <- plot + facet_grid(reformulate(factors[[3]], "."))
  }

  return(plot)
}



#' @export
print.lazyhci_analysis <- function(x, ...){

  cli::cli_h1("Assumption Checks")

  if("normality_test" %in% names(x)) {
    cli::cli_par()
    val <- format(round(x[["normality_test"]], 4), nsmall = 4)
    if(x[["normality_test"]] < 0.05) {
      cli::cli_alert_danger("Check for the normality of residuals...{.strong FAILED} (p = {val})")
      cli::cli_alert_info("Statistical tests for the normality can be unreliable. Please check the histogram by calling {.code plot(x$normality_test)}")
      cli::cli_alert_info("You can try to use a transformation when fitting the model or switch to a non-parametric analysis type.")
    } else
      cli::cli_alert_success("Check for the normality of residuals...OK (p = {val})")
    cli::cli_end()
  }

  if("sphericity_test" %in% names(x)) {
    cli::cli_par()
    if(any(x[["sphericity_test"]] < 0.05)) {
      cli::cli_alert_warning("Check for sphericity...{.strong FAILED}")
      cli::cli_alert_info("The check for sphericity was significant for at least one of you IVs.")
      cli::cli_alert_info("We will correct for these using Greenhouse Geissers method. There is nothing you need to do.")
    } else
      cli::cli_alert_success("Check for sphericity...OK")
    cli::cli_end()
  }

  if("homogeniety_test" %in% names(x)) {
    val <- format(round(x[["homogeniety_test"]], 2), nsmall = 2)
    if(x[["homogeniety_test"]] < 0.05)
      cli::cli_alert_warning("Check for homogeneity of variances...FAILED ({val})")
    else
      cli::cli_alert_success("Check for homogeneity of variances...OK ({val})")
  }

  cli::cli_h1("Analysis")

  print(x$model)

  cli::cli_h1("Post-Hoc Tests")

  print(x$post_hoc)

}

#' @export
print.lazyhci_post_hoc_list <- function(x, ...){
  for(term in names(x))   {
    #cat("Post hoc tests for", term, "\n")
    dat <- x[[term]]

    cli::cli_h3(term)
    cli::cli_par()
    print(dat$pairwise)
    cli::cli_end()
    #print(dat[grep("pairwise", names(dat))])
  }

}
