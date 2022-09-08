#' lazy_analyze
#'
#' This function conducts (non-) parametric analyzes of (multi-) factorial experiments
#' @param data the data
#' @param participant the column name of the participant identifier
#' @param DV the column name of the dependent variable
#' @param within.vars (optional) a list of within-subject variables
#' @param between.vars (optional) a list of between-subject variables
#' @param analysis_type one of "aov", "art", "friedman", "lme", "glme".
#' @param posthoc.adj the adjustment method for post-hoc tests. Defaults to "bonferroni"
#' @param anova.type the type of anova test to perform. Defaults to 3
#' @param transformation (optional) a transformation that should be applied to the data before the test.
#'
#' @export
lazy_analyze<- function(data, participant, DV, within.vars = NULL, between.vars = NULL, analysis_type=c("aov", "art", "lme", "glme", "friedman"), posthoc.adj = "bonf", anova.type = 3, transformation = NULL, family = c("poisson", "binominal")) {
  require(janitor)
  require(emmeans)
  require(ARTool)

  if(!hasArg(analysis_type))
    stop("Please provide an analysis type.")

  data.clean <- janitor::clean_names(data)
  participant.clean <- janitor::make_clean_names(participant)
  DV.clean <- janitor::make_clean_names(DV)
  within.vars.clean <- janitor::make_clean_names(within.vars)
  between.vars.clean <- janitor::make_clean_names(between.vars)

  data.clean <- make.factors.internal(data.clean, c(within.vars.clean, between.vars.clean, participant.clean))

  result <- list()
  result[["descriptives"]] <- descriptives(data.clean, DV.clean, c(within.vars.clean, between.vars.clean))

  if(analysis_type == "aov") {
    model <- fit_model_afex.internal(data = data.clean, participant = participant.clean, DV = DV.clean, within.vars = within.vars.clean, between.vars = between.vars.clean, anova.type = anova.type, transformation = transformation)
    result[["model"]] <- model
    result[["post_hoc"]] <- do.post_hoc.internal(model, model$Anova$terms[!model$Anova$terms %in% c('(Intercept)')], posthoc.adj, fct_contrasts = post_hoc.internal, fct_ip = interaction_plot.aov.internal)

    result[["normality_test"]] <- check_normality(result[["model"]])

    if(!is.null(within.vars)) {
      result[["sphericity_test"]] <- check_sphericity(model)
    }

    if(!is.null(between.vars)) {
      result[["homogeneity_test"]] <- check_homogeneity(model)
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
    model <- lmer.fit.inernal(data = data.clean, DV = DV.clean, participant = participant.clean, within.vars = within.vars.clean, between.vars = between.vars.clean, analysis_type = analysis_type, glme.family = family)
    result[["anova"]] <-  car::Anova(model, type=anova.type)
    result[["model"]] <- model
    result[["post_hoc"]] <- do.post_hoc.internal(model, rownames(result[["anova"]][!(row.names(result[["anova"]]) %in% c("(Intercept)")), ]), posthoc.adj, fct_contrasts = post_hoc.internal, fct_ip = interaction_plot.aov.internal)
  }

  print(length(within.vars.clean) == 1)
  print(length(between.vars.clean) == 0)



  if(analysis_type == "friedman") {
    require(PMCMRplus)

    if(!((length(within.vars.clean) == 1) && (length(between.vars.clean) == 0))) {
      stop("friedman analysis only appllies to one within and zero between vars .")
    }

    #friedman.test(x ~ w | t, data = wb)

    friedman <- friedman.test(as.formula(paste0(DV.clean, " ~ ", within.vars.clean[[1]], " | ", participant.clean)), data = data.clean)
    post_hoc_friedman <- PMCMRplus::frdAllPairsConoverTest(y = data.clean[[DV.clean]], groups = data.clean[[within.vars.clean[[1]]]],
                                         blocks = data.clean[[participant.clean]], p.adjust.method = posthoc.adj)

    result[["friedman"]] <- friedman
    result[["post_hoc"]] <- post_hoc_friedman
  }


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
  require(tidyverse)

  result <- list()

  for(term in terms) {

    print(unlist(str_split(term, ":")))

    tmp <- fct_contrasts(model, unlist(str_split(term, ":")), posthoc.adj)
    interaction.plot <- fct_ip(tmp[[1]], unlist(str_split(term, ":")))

    tmp[["interaction_plot"]] <- interaction.plot

    result[[term]] <- tmp
  }

  return(result)
}

post_hoc.art.internal <- function(model, factors, posthoc.adj = "bonf") {
  require(ARTool)

  artlm <- artlm.con(model, paste(factors, collapse = ":"))
  result <- post_hoc.internal(artlm, factors, posthoc.adj, collapse = "")

  return(result)
}

post_hoc.internal <- function(model, factors, posthoc.adj = "bonf", collapse=":") {
  require(emmeans)

  form_string_emm <- paste0("pairwise ~ ", paste(factors, collapse = collapse))

  result <- emmeans(model, list(as.formula(form_string_emm)), adjust = "bonf")

  return(result)
}

lmer.fit.inernal <- function(data, participant, DV, within.vars = NULL, between.vars = NULL, analysis_type, anova.type = 3, glme.family = NULL) {
  require(lme4)
  require(car)
  require(emmeans)

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

  if(analysis_type == "lme")
    model <- lme4::lmer(as.formula(formula.string), data=data)

  if(analysis_type == "glme")
    model <- lme4::glmer(as.formula(formula.string), data=data, family = glme.family)

  return(model)
}

fit_model_afex.internal <- function(data, participant, DV, within.vars = NULL, between.vars = NULL, transformation = NULL, anova.type = 3, es = "ges") {
  require(afex)
  require(performance)
  require(qqplotr)

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

  message(paste0("Fitting AOV Model using afex for ", aov_ez_string))

  aov.model <- eval(parse(text=aov_ez_string))

  return(aov.model)
}

aov_art_fit.internal <- function(data, DV, participant, within.vars = c(), between.vars = c()) {
  require(ARTool)
  require(effectsize)

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

  m.aov <- art(as.formula(formula.aov), data=data)
  m.lme <- art(as.formula(formula.lme), data=data)


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

  return(emmip(emmeans_model, formula2))
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

