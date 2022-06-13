#' aov_analysis
#'
#' Perform (RM) anova analysis using ezANOVA including asusmption checks and post-hoc tests
#' @param data the data
#' @param DV the name of the column containing the DV
#' @param participantCol the column containing an identifier for the participant
#' @param within.vars (optional) a vector containing the names of the columns with within-subjects variables
#' @param between.vars (optional) a vector containing the names of the columns with between-subjects variables
#' @param posthoc.adj (optional) post-hoc adjustment method. Defaults to bonferroni
#' @param anova.type (optional) as defined by ezAnova. Defaults to 2.
#' @param normality.test (optional) the normality test function to use. Defaults to shapiro.test
#' @param latex.digits.round (optional) round the latex output to x digits. Defaults to 2.
#' @export
aov_analysis <- function(data, DV, participantCol, within.vars = NULL, between.vars = NULL, posthoc.adj="bonferroni", anova.type = 2, normality.test = shapiro.test, latex.digits.round = 2) {
  options(contrasts = c("contr.sum", "contr.poly"))

  data.aov <- fix_col_names(data)
  DV <- fix_strings(DV)
  participantCol <- fix_strings(participantCol)
  within.vars <- fix_strings(within.vars)
  between.vars <- fix_strings(between.vars)

  ezString <- build_ez_string.internal("data.aov", DV = DV, participantCol = participantCol, within.vars = within.vars, between.vars = between.vars, anova.type = anova.type)

  if(!check_complete_design(data.aov, within.vars = within.vars, between.vars = between.vars, participantCol = participantCol, DV))
    stop("This is not a complete design. Please check the output of check_complete_design")

  mod.ez <- eval(parse(text=ezString))
  residuals <- purrr::map(mod.ez$aov, residuals)
  ezanova_residuals_tbl <- enframe(residuals) %>% unnest(cols=c(value))

  mod.ez$'NormalityTest' <- normality.test(ezanova_residuals_tbl$value)
  mod.ez$'NormalityHist' <- hist(ezanova_residuals_tbl$value)

  tidy <- broom::tidy(mod.ez$aov)
  mod.ez$Posthoc = list()

  if(!is.null(within.vars)) {
    tmp <- mod.ez$ANOVA %>% left_join(mod.ez$`Mauchly's Test for Sphericity`, by="Effect") %>% left_join(mod.ez$`Sphericity Corrections`)
    tmp <- tmp %>%
      rowwise() %>%
      mutate(
        DFnCor  = ifelse(is.na(p.y) | p.y > 0.05, DFn, DFn*GGe),
        DFdCor  = ifelse(is.na(p.y) | p.y > 0.05, DFd, DFd*GGe),
        pCor    = ifelse(is.na(p.y) | p.y > 0.05, `p.x`, `p[GG]`),
        pCorSig = ifelse(is.na(p.y) | p.y > 0.05, `p<.05.x`, `p[GG]<.05`),
        GGeCor  = ifelse(is.na(p.y) | p.y > 0.05, NA, GGe)
        )

    anova.cor <- mod.ez$ANOVA
    anova.cor$DFn <- tmp$DFnCor
    anova.cor$DFd <- tmp$DFdCor
    anova.cor$p <- tmp$pCor
    anova.cor$`p<.05` <- tmp$pCorSig
    anova.cor$GGe <- tmp$GGeCor

    anova.cor <- anova.cor %>%
      rowwise() %>%
      mutate(
        latex = build_aov_latex.internal(DFn, DFd, F, p, ges, GGe)
      ) %>%
      as.data.frame()

    mod.ez$'ANOVA Corrected' <- anova.cor
  }

  mod.ez$ANOVA <- mod.ez$ANOVA %>%
    rowwise() %>%
    mutate(
      latex = build_aov_latex.internal(DFn, DFd, F, p, ges)
    ) %>%
    as.data.frame()


  for(row in 1:nrow(tidy)) {
    cur.term <- tidy[row, 2][[1]]
    cur.p <- tidy[row, 7][[1]]

    if(!is.na(cur.p) && cur.p< 0.05) {
      cur.formula.str <- paste("~ ", cur.term, sep="")

      data.sum <- data.aov %>%
        dplyr::group_by_at(str_split(cur.term, pattern = ":", simplify = TRUE)) %>%
        dplyr::summarise(
          n=n(),
          mean=mean(!!as.name(DV), na.rm = TRUE),
          sd = sd(!!as.name(DV), na.rm = TRUE)
        )

      data.sum <- data.sum %>%
        dplyr::mutate(latex=paste0("\val{", roundp.internal(mean, digits = 2) ,"}{", roundp.internal(sd, digits = 2) ,"}"))

      emm <- emmeans::emmeans(mod.ez$aov, as.formula(cur.formula.str))
      pairs <- pairs(emm,adjust = posthoc.adj)

      emm <- as_tibble(emm) %>%
        dplyr::mutate(latex=paste0("\valCI{", roundp.internal(emmean, digits = 2) ,"}{", roundp.internal(SE, digits = 2) ,"}{", roundp.internal(lower.CL, digits = 2) , "}{", roundp.internal(upper.CL, digits = 2) , "}"))

      mod.ez$Posthoc[[cur.term]] <- list(descriptives = as.data.frame(data.sum), emm = as.data.frame(emm), contrasts = as.data.frame(pairs))    }
  }

  return(mod.ez)
}

build_ez_string.internal <- function(data, DV, participantCol, within.vars = NULL, between.vars = NULL, anova.type = 2) {
  ezString <- paste0('ez::ezANOVA(data=', data, ', dv=', DV,', wid=', participantCol, ', ')

  if(!is.null(within.vars)) {
    ezString <- paste0(ezString,
                       'within=.(', paste(within.vars, collapse = ",") ,'), '
    )
  }
  if(!is.null(between.vars)) {
    ezString <- paste0(ezString,
                       'between=.(', paste(between.vars, collapse = ",") ,'), '
    )
  }

  ezString <- paste0(ezString,
                     'type=', anova.type, ', return_aov = TRUE)')

  print(ezString)

  return(ezString)
}
