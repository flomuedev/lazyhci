#' setwd_to_clipboard
#'
#' Changes the working directory to the one in the clipboard
#' @export
setwd_to_clipboard <- function() {
  setwd(chartr("\\", "/", readClipboard()))
}
#' se
#'
#' This function allows you to calculate the standard error
#' @param x The data
#' @export
se <- function(x, na.rm=FALSE) {
  return(sd(x, na.rm=na.rm)/sqrt(ifelse(na.rm, length(x[!is.na(x)]), length(x))))
}

#' fix_col_names
#'
#' Removes spaces and comma from column names
#' @param x The data
#' @export
fix_col_names <- function(x) {
  require(stringr)
  names(x)<-fix_strings(names(x))
  return(x)
}

#' fix_strings
#'
#' Removes spaces and comma from strings
#' @param x The data
#' @export
fix_strings <- function(x) {
  #require(stringr)
  #return(str_replace_all(x, c(" " = "." , "," = "" )))
  return(make.names(x, unique=TRUE))
}

#' set_col_type
#'
#' Batch-sets column names
#' @param x The data
#' @param cols a vector of col numbers
#' @param coltype one of 'numeric', 'character' 'factor'
#' @export
set_col_type <- function(x, cols, coltype = c("numeric", "character", "factor")) {
  require(rlang)
  rlang::arg_match(coltype)

  for(col in cols) {
    x[col] <- fun.conv(x[col])
  }
  return(x)
}

#' check_complete_design
#'
#' This function checks if the data is complete for your design for one DV
#' @param data the data
#' @param within.vars a vector of the names of your within IV cols
#' @param between.vars a vector of the names of your between IV cols
#' @param participantCol the name of your participant ID col
#' @param DV the name of the dependent variable column
#' @export
check_complete_design <- function(data, within.vars = NULL, between.vars = NULL, participantCol, DV) {
  require(dplyr)

  if(!is.null(within.vars)) {
    data.sum <- data %>%
      dplyr::group_by_at(c(within.vars, participantCol), .drop=FALSE) %>%
      dplyr::summarise(
        n = n(),
        val = mean(!!as.name(DV))
      )

    missing <- data.sum %>%
      filter(n == 0)

    higher <- data.sum %>%
      filter(n > 1)

    na.nan <- data.sum %>%
      filter(is.na(val) | is.nan(val))

    if(nrow(missing) != 0) {
      warning("This is not a complete design, go and yell at the student. The following condition data is missing:")
      warning(paste0(capture.output(as.data.frame(missing)), collapse = "\n"))
      return(FALSE)
    }

    if(nrow(na.nan) != 0) {
      warning("There are NAs or NANs in the table, go and yell at the student. The following conditions contain NAs or NANs:")
      warning(paste0(capture.output(as.data.frame(na.nan)), collapse = "\n"))
      return(FALSE)
    }

    if(nrow(higher) != 0) {
      warning("WARNING: Your data seems to contain multiple repetitions (or you have not provided all IVs). The table should be collapsed before the ANOVA.")
    }
  }

  if(!is.null(between.vars)) {
    between.groupsizes <- data %>%
      group_by_at(between.vars) %>%
      summarize(n=n()) %>%
      select(n) %>%
      pull() %>%
      unique() %>%
      length()

    between.groupsizes.participant <- data %>%
      group_by_at(c(between.vars, participantCol)) %>%
      summarize(n=n()) %>%
      select(n) %>%
      pull() %>%
      unique() %>%
      length()



    if(between.groupsizes != 1) {
      warning("WARNING: Your between-groups are not equal-sized. This might be totally fine, just be sure about what your doing.")
    }

    if(between.groupsizes.participant != 1) {
      warning("WARNING: Your you have (at least) one between groups factor that varies within a participant. This is rather strange.")
    }
  }

  return(TRUE)
}

#' @noRd
data_summary.internal <- function(data, varname, groupnames, fun.sum = mean, fun.error = sd, na.rm = FALSE){
  require(dplyr)

  data.sum <- data %>%
    dplyr::group_by_at(groupnames) %>%
    dplyr::summarise(
      mean = fun.sum(!!as.name(varname), na.rm = na.rm),
      error = fun.error(!!as.name(varname), na.rm = na.rm)
    )

  return(data.sum)
}

#' @noRd
roundp.internal <- function(x, digits = 2) {
  return(sprintf(paste0("%.", digits, "f"), round(x,digits)))
}

#' @noRd
build_aov_latex.internal <- function(DFn, DFd, F, p, ges, gge = NA) {

  F <- roundp.internal(F, digits = 2)
  ges <- roundp.internal(ges, digits = 2)
  sig <- symnum(p, corr=FALSE, na=FALSE,
                cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                symbols = c("<.001", "<.01", "<.05", ">.05"))

  if(is.na(gge)) {
    if(is.numeric(DFn))
      DFn <- roundp.internal(DFn, digits=0)
    if(is.numeric(DFd))
      DFd <- roundp.internal(DFd, digits=0)

    latexString <- paste0("\anova{", DFn, "}{", DFd, "}{",F,"}{",sig,"}{",ges,"}")
  } else {
    gge <- roundp.internal(gge, digits = 2)
    DFn <- roundp.internal(DFn, digits = 2)
    DFd <- roundp.internal(DFd, digits = 2)

    latexString <- paste0("\anovaCor{", DFn, "}{", DFd, "}{",F,"}{",sig,"}{",gge,"}{",ges,"}")
  }


  return(latexString)
}
