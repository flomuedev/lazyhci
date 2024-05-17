#' lazy_analyze
#'
#' This function conducts (non-) parametric analyzes of (multi-) factorial experiments
#' @param lazy_model the data model
#' @param dv the column name of the dependent variable
#' @param analysis_type one of "aov", "art", "friedman", "lme", "glme".
#' @param posthoc.adj the adjustment method for post-hoc tests. Defaults to "bonf"
#' @param anova.type the type of anova test to perform. Defaults to 3
#' @param transformation (optional) a transformation that should be applied to the data before the test.
#' @param family the family to use for lme or glme.One of "poisson" or "binomial"
#' @param nAGQ the nAGQ argument for glmer
#' @param lme.formula (optional) a formula for glme or lme calls to override the calculated formula.
#' @param emm.type the emm type argument. Defaults to "response"
#' @param na.rm remove na
#' @param remove.incomplete remove incomplete cases before the analysis. Only applies to "art"
#'
#' @importFrom stats as.formula anova friedman.test median quantile reformulate sd setNames filter
#' @importFrom methods hasArg is
#' @importFrom utils capture.output tail
#' @importFrom rlang .data
#'
#' @export
lazy_analyze <- function(lazy_model, dv, analysis_type = c("aov", "art", "lme", "glme", "friedman"), posthoc.adj = "bonf", anova.type = 3, transformation = NULL, family = c(NULL, "poisson", "binomial"), nAGQ = NULL, lme.formula = NULL, emm.type = "response", na.rm = FALSE, remove.incomplete = FALSE) {
  analysis_type <- match.arg(analysis_type)
  family <- match.arg(family)

  data.clean <- lazy_model$data
  participant.clean <- lazy_model$participant
  DV.clean <- janitor::make_clean_names(dv)
  within.vars.clean <- lazy_model$within.vars
  between.vars.clean <- lazy_model$between.vars

  result <- list()
  attr(result, "lazyhci.anatype") <- analysis_type

  result[["lazy_model"]] <- lazy_model
  result[["descriptives"]] <- lazy_descriptives(lazy_model, dv, na.rm = na.rm)

  if (analysis_type == "aov") {
    model <- fit_model_afex.internal(data = data.clean, participant = participant.clean, DV = DV.clean, within.vars = within.vars.clean, between.vars = between.vars.clean, anova.type = anova.type, transformation = transformation, na.rm = na.rm)
    result[["model"]] <- model
    result[["post_hoc"]] <- do.post_hoc.internal(model,
      rownames(model$anova_table %>% dplyr::filter(`Pr(>F)` < 0.05)),
      posthoc.adj,
      fct_contrasts = post_hoc.internal, fct_ip = interaction_plot.aov.internal, emm.type = emm.type
    )

    result[["normality_test"]] <- performance::check_normality(model)

    if (!is.null(within.vars.clean)) {
      result[["sphericity_test"]] <- performance::check_sphericity(model)
    }

    if (!is.null(between.vars.clean)) {
      result[["homogeneity_test"]] <- performance::check_homogeneity(model)
    }

    attr(result, "lazyhci.latexable") <- "aov"
  }

  if (analysis_type == "art") {
    if (remove.incomplete) {
      data.clean <- filter_broken_participants(data.clean, c(within.vars.clean, between.vars.clean), participant.clean, DV.clean, na.rm)
    }
    model <- aov_art_fit.internal(data = data.clean, DV = DV.clean, participant = participant.clean, within.vars = within.vars.clean, between.vars = between.vars.clean)
    model$anovaAOV <- anova(model$m.aov)
    model$anovaLME <- anova(model$m.lme)
    model$anovaAOV$eta.sq <- with(model$anovaAOV, `Sum Sq` / (`Sum Sq` + `Sum Sq.res`))
    # model$anovaLME$eta.sq <- with(model$anovaLME, `Sum Sq`/(`Sum Sq` + `Sum Sq.res`))
    class(model) <- "lazyhci_analysis_art"
    attr(result, "lazyhci.latexable") <- "art"
    result[["model"]] <- model
    result[["post_hoc"]] <- do.post_hoc.internal(model$m.lme, model$anovaLME %>% dplyr::filter(`Pr(>F)` < 0.05) %>% dplyr::pull(Term), posthoc.adj, fct_contrasts = post_hoc.art.internal, fct_ip = interaction_plot.art.internal)
  }

  if (analysis_type == "lme" || analysis_type == "glme") {
    model <- lmer.fit.inernal(data = data.clean, DV = DV.clean, participant = participant.clean, within.vars = within.vars.clean, between.vars = between.vars.clean, analysis_type = analysis_type, glme.family = family, nAGQ = nAGQ, lme.formula = lme.formula)
    anova <- car::Anova(model, type = anova.type)
    bindmodel <- list(model = model, anova = anova)
    class(bindmodel) <- "lazyhci_analysis_lme4"
    attr(result, "lazyhci.latexable") <- "lme"
    result[["model"]] <- bindmodel
    result[["post_hoc"]] <- do.post_hoc.internal(model, rownames(
      anova %>% dplyr::filter(!grepl("Intercept", rownames(anova))) %>% dplyr::filter(`Pr(>Chisq)` < 0.05)
    ), posthoc.adj, fct_contrasts = post_hoc.internal, fct_ip = interaction_plot.aov.internal)
  }

  if (analysis_type == "friedman") {
    if (!((length(within.vars.clean) == 1) && (length(between.vars.clean) == 0))) {
      stop("friedman analysis only applies to one within and zero between vars .")
    }

    # friedman.test(x ~ w | t, data = wb)

    data.friedman <- data.clean %>%
      dplyr::group_by_at(c(lazy_model$within.vars, lazy_model$participant), .drop = FALSE) %>%
      dplyr::summarise(
        n = dplyr::n(),
        val = mean(!!as.name(DV.clean))
      )

    colnames(data.friedman)[colnames(data.friedman) == "val"] <- DV.clean

    formula.string <- paste0(DV.clean, " ~ ", within.vars.clean[[1]], " | ", participant.clean)

    friedman <- friedman.test(as.formula(formula.string), data = data.friedman)
    post_hoc_friedman <- PMCMRplus::frdAllPairsConoverTest(
      y = data.friedman[[DV.clean]], groups = data.friedman[[within.vars.clean[[1]]]],
      blocks = data.friedman[[participant.clean]], p.adjust.method = posthoc.adj
    )

    attr(result, "lazyhci.latexable") <- "friedman"
    result[["test_repetitions"]] <- unique(data.friedman$n)[1]
    result[["model"]] <- friedman
    result[["post_hoc"]] <- post_hoc_friedman
  }

  class(result) <- "lazyhci_analysis"
  attr(result, "lazyhci.dv") <- dv

  return(result)
}

filter_broken_participants <- function(x, vars, participant, dv, na.rm = FALSE) {
  data.sum <- x %>%
    dplyr::group_by_at(c(vars, participant), .drop = FALSE) %>%
    dplyr::summarise(
      n = dplyr::n(),
      val = mean(!!as.name(dv), na.rm = na.rm)
    )

  to_remove <- factor(0)

  missing <- data.sum %>%
    dplyr::filter(n == 0)

  na.nan <- data.sum %>%
    dplyr::filter(is.na(val) | is.nan(val))

  if (nrow(missing) > 0) {
    to_remove <- c(to_remove, unique(missing %>% select(!!as.name(participant)) %>% pull()))
  }


  if (nrow(na.nan) > 0) {
    to_remove <- c(to_remove, unique(na.nan %>% select(!!as.name(participant)) %>% pull()))
  }

  if (length(to_remove) > 0) {
    cli::cli_alert_warning("Removed IDs {to_remove} before the analysis because they are missing data. ")
    x <- x %>% dplyr::filter(!(!!as.name(participant) %in% to_remove))
    x[[participant]] <- droplevels(x[[participant]])
  }

  return(x)
}

make.factors.internal <- function(data.clean, vars) {
  for (var in vars) {
    if (!is.factor(data.clean[[var]])) {
      warning(paste0("auto-converting column ", var, " to a factor."))
      data.clean[[var]] <- as.factor(data.clean[[var]])
    }
  }

  return(data.clean)
}

do.post_hoc.internal <- function(model, terms, posthoc.adj = "bonf", fct_contrasts, fct_ip, emm.type = "response") {
  result <- list()

  for (term in terms) {
    # print(unlist(stringr::str_split(term, ":")))

    tmp <- fct_contrasts(model, unlist(stringr::str_split(term, ":")), posthoc.adj)

    interaction.plot <- fct_ip(tmp[[1]], unlist(stringr::str_split(term, ":")), emm.type = emm.type)

    tmp[["interaction_plot"]] <- interaction.plot

    class(tmp) <- "lazyhci_post_hoc_item"

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

post_hoc.internal <- function(model, factors, posthoc.adj = "bonf", collapse = ":") {
  form_string_emm <- paste0("pairwise ~ ", paste(factors, collapse = collapse))

  result <- emmeans::emmeans(model, list(as.formula(form_string_emm)), adjust = posthoc.adj)

  return(result)
}

lmer.fit.inernal <- function(data, participant, DV, within.vars = NULL, between.vars = NULL, analysis_type, anova.type = 3, glme.family = NULL, nAGQ = NULL, lme.formula = NULL) {
  if (analysis_type == "glme" && !hasArg(glme.family)) {
    stop("glme requires the family argument.")
  }

  if (is.null(lme.formula)) {
    formula.string <- paste(
      DV,
      " ~ ",
      "(",
      paste(c(within.vars, between.vars), collapse = "*"),
      ")",
      " + ",
      "(1|",
      participant,
      ")",
      sep = ""
    )

    cli::cli_alert_info("Fitting model using formula {.code {formula.string}}. You can override the formula using the {.code lme.formula} argument.")
  } else {
    formula.string <- lme.formula
  }


  if (analysis_type == "lme") {
    model <- lme4::lmer(as.formula(formula.string), data = data)
  }


  if (analysis_type == "glme") {
    if (is.null(nAGQ)) {
      model <- lme4::glmer(as.formula(formula.string), data = data, family = glme.family)
    } else {
      model <- lme4::glmer(as.formula(formula.string), data = data, family = glme.family, nAGQ = nAGQ)
    }
  }

  return(model)
}


fit_model_afex.internal <- function(data, participant, DV, within.vars = NULL, between.vars = NULL, transformation = NULL, anova.type = 3, es = "ges", na.rm = FALSE) {
  afex::afex_options(
    correction_aov = "GG",
    emmeans_model = "multivariate"
  )

  aov_ez_string <- "afex::aov_ez(id = participant, dv = DV, data = data, type = anova.type, include_aov = TRUE, es = es, na.rm = na.rm"

  if (!is.null(within.vars)) {
    aov_ez_string <- paste0(aov_ez_string, ", within = within.vars")
  }

  if (!is.null(between.vars)) {
    aov_ez_string <- paste0(aov_ez_string, ", between = between.vars")
  }

  if (!is.null(transformation)) {
    aov_ez_string <- paste0(aov_ez_string, ", transformation = transformation")
  }

  aov_ez_string <- paste0(aov_ez_string, ")")

  # message(paste0("Fitting AOV Model using afex for ", aov_ez_string))

  aov.model <- eval(parse(text = aov_ez_string))

  return(aov.model)
}

aov_art_fit.internal <- function(data, DV, participant, within.vars = c(), between.vars = c()) {
  data.internal <- as.data.frame(data)

  formula.aov <- paste(
    DV,
    " ~ ",
    "(",
    paste(c(within.vars, between.vars), collapse = "*"),
    ")",
    " + ",
    "Error(",
    participant,
    "/",
    "(",
    paste(within.vars, collapse = "*"),
    "))",
    sep = ""
  )

  formula.lme <- paste(
    DV,
    " ~ ",
    "(",
    paste(c(within.vars, between.vars), collapse = "*"),
    ")",
    " + ",
    "(1|",
    participant,
    ")",
    sep = ""
  )

  m.aov <- ARTool::art(as.formula(formula.aov), data = data)
  m.lme <- ARTool::art(as.formula(formula.lme), data = data)


  return(list("m.aov" = m.aov, "m.lme" = m.lme))
}

interaction_plot.aov.internal <- function(emmeans_model, factors, emm.type = "response") {
  if (length(factors) > 3) {
    return(NULL)
  }

  if (length(factors) == 1) {
    formula2 <- as.formula(paste("~", factors[[1]], sep = ""))
  }
  if (length(factors) == 2) {
    formula2 <- as.formula(paste(factors[[1]], "~", factors[[2]], sep = ""))
  } else if (length(factors) == 3) {
    formula2 <- as.formula(paste(factors[[1]], "~", factors[[2]], " | ", factors[[3]], sep = ""))
  }

  return(emmeans::emmip(emmeans_model, formula2, type = emm.type))
}

interaction_plot.art.internal <- function(emmeans_model, factors, emm.type = response) {
  if (length(factors) > 3) {
    return(NULL)
  }

  colname <- paste(factors, collapse = "")

  emmeans_model <- as.data.frame(emmeans_model) %>%
    tidyr::separate(!!as.name(colname), factors, sep = ",")

  aes_ggplot_arguments <- list(
    x = factors[[1]],
    y = "emmean"
  )

  tvar <- factors[[1]]

  if (length(factors) > 1) {
    tvar <- factors[[2]]
  }

  emmeans_model <- emmeans_model %>%
    dplyr::rename(
      dplyr::any_of(
        c(
          lower.CL = "asymp.LCL",
          upper.CL = "asymp.UCL"
        )
      )
    )


  plot <- ggplot2::ggplot(data = emmeans_model, ggplot2::aes_string(x = factors[[1]], y = "emmean", colour = tvar)) +
    ggplot2::geom_point() +
    ggplot2::geom_linerange(
      ggplot2::aes_string(ymin = "lower.CL", ymax = "upper.CL")
    ) +
    ggplot2::geom_line(
      ggplot2::aes_string(
        group = tvar
      )
    )

  if (length(factors) == 3) {
    plot <- plot + ggplot2::facet_grid(reformulate(factors[[3]], "."))
  }

  return(plot)
}



#' @export
print.lazyhci_analysis <- function(x, ...) {
  cli::cli_h1("Assumption Checks")

  test <- FALSE

  if ("test_repetitions" %in% names(x)) {
    cli::cli_par()
    if (x[["test_repetitions"]] != 1) {
      cli::cli_alert_warning("Your data contains {x[['test_repetitions']]} repetitions. Consider using a more appropriate test that takes these into account. For now, we used the mean for the friedman test.")
    } else {
      cli::cli_alert_success("Check if the design is valid for the friedman test ...{.strong OK}")
    }
    cli::cli_end()
    test <- TRUE
  }

  if ("normality_test" %in% names(x)) {
    cli::cli_par()
    val <- finalfit::p_tidy(x[["normality_test"]], digits = 3)
    if (x[["normality_test"]] < 0.05) {
      cli::cli_alert_danger("Check for the normality of residuals...{.strong FAILED} (p{val})")
      cli::cli_alert_info("Statistical tests for the normality can be unreliable. Please check the histogram by calling {.code plot(x$normality_test)}")
      cli::cli_alert_info("You can try to use a transformation when fitting the model or switch to a non-parametric analysis type.")
    } else {
      cli::cli_alert_success("Check for the normality of residuals...{.strong OK} (p{val})")
    }
    cli::cli_end()
    test <- TRUE
  }

  if ("sphericity_test" %in% names(x)) {
    cli::cli_par()
    if (any(x[["sphericity_test"]] < 0.05)) {
      cli::cli_alert_danger("Check for sphericity...{.strong FAILED}")

      model.summary <- summary(x$model)

      for (i in seq_len(length(x[["sphericity_test"]]))) {
        name <- names(x[["sphericity_test"]])[i]
        p.value <- x[["sphericity_test"]][i]

        if (p.value < 0.05) {
          adj <- model.summary$pval.adjustments[name, "GG eps"]
          cli::cli_alert_info("Mauchly's test indicated a violation of the assumption of sphericity for {name} (< {round(p.value, digits = 3)}). We will use a GG epsilon of {round(adj, digits = 3)}")
        }
      }

      cli::cli_alert_info("We will correct for these using Greenhouse Geissers method. There is nothing you need to do.")
    } else {
      cli::cli_alert_success("Check for sphericity...{.strong OK}")
    }
    cli::cli_end()
    test <- TRUE
  }

  if ("homogeneity_test" %in% names(x)) {
    cli::cli_par()
    val <- finalfit::p_tidy(x[["homogeneity_test"]], digits = 3)
    if (x[["homogeneity_test"]] < 0.05) {
      cli::cli_alert_danger("Check for homogeneity of variances...{.strong FAILED} (p{val})")
      cli::cli_alert_info("The check for homogeneity was significant for at least one of you IVs.")
      cli::cli_alert_info("ANOVAs are considered robust to light heteroscedasticity. But you should consider using a test that does not require this assumption.")
    } else {
      cli::cli_alert_success("Check for homogeneity of variances...{.strong OK} (p{val})")
    }
    cli::cli_end()
    test <- TRUE
  }

  if ("lazyhci_analysis_art" %in% class(x$model)) {
    sumAov <- summary(x$model$m.aov)
    sumLme <- summary(x$model$m.lme)

    cli::cli_par()
    cli::cli_alert_info("ART (AOV Model): F values of ANOVAs on aligned responses not of interest (should all be ~0):")
    print(round(summary(sumAov$aligned.anova$F), 2))
    cli::cli_end()

    cli::cli_par()
    cli::cli_alert_info("ART (LME Model): F values of ANOVAs on aligned responses not of interest (should all be ~0):")
    print(round(summary(sumLme$aligned.anova$F), 2))
    cli::cli_end()

    test <- TRUE
  }

  if (!test) {
    cli::cli_par()
    cli::cli_alert_info("There are no assumption checks for this test.")
    cli::cli_end()
  }


  cli::cli_h1("Analysis")
  print(x$model)

  cli::cli_h1("Post-Hoc Tests")
  print(x$post_hoc)
}

#' @export
print.lazyhci_post_hoc_list <- function(x, ...) {
  for (term in names(x)) {
    # cat("Post hoc tests for", term, "\n")
    dat <- x[[term]]

    cli::cli_h3(term)
    cli::cli_par()
    print(dat$pairwise)
    if (grepl(":", term, fixed = TRUE)) {
      cli::cli_alert_info("You can check the interaction plot to help interpret these results by calling x$post_hoc$`{term}`$interaction_plot")
    }
    cli::cli_end()
    # print(dat[grep("pairwise", names(dat))])
  }

  if (length(x) == 0) {
    cli::cli_alert_info("No post-hoc tests available as the omnibus test did not yield significant results.")
  }
}

#' @export
print.afex_aov <- function(x, ...) {
  print(anova(x, correction = "GG"))
}

#' @export
print.lazyhci_post_hoc_friedman <- function(x, ...) {
  cli::cli_h1("Post-Hoc Tests")
  print()
}

#' @export
print.lazyhci_post_hoc_item <- function(x, ...) {
  print(x$pairwise)
}

#' @export
print.lazyhci_analysis_art <- function(x, ...) {
  cli::cli_h2("ANOVA (AOV Model)")
  cli::cli_alert_info("Fitted using formula {.code {deparse1(x$m.aov$formula)}}\n\n")
  print(x$anovaAOV)

  cli::cli_h2("ANOVA (LME Model)")
  cli::cli_alert_info("Fitted using formula {.code {deparse1(x$m.lme$formula)}}\n\n")
  print(x$anovaLME)
}

#' @export
print.lazyhci_analysis_lme4 <- function(x, ...) {
  cli::cli_h2("Model Details")
  print(x$model)

  cli::cli_h2("ANOVA")
  print(x$anova)
}
