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

#' wtf_is
#'
#' Reports details about the supplied object
#' @param x The objectö
#' @export
wtf_is <- function(x) {
  # For when you have no idea what something is.
  # https://stackoverflow.com/questions/8855589
  cat("1. typeof():\n")
  print(typeof(x))
  cat("\n2. class():\n")
  print(class(x))
  cat("\n3. mode():\n")
  print(mode(x))
  cat("\n4. names():\n")
  print(names(x))
  cat("\n5. slotNames():\n")
  print(slotNames(x))
  cat("\n6. attributes():\n")
  print(attributes(x))
  cat("\n7. str():\n")
  print(str(x))
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
  if(is.null(x))
    return(NULL)
  return(make.names(x, unique=TRUE))
}

#' import_google_form
#'
#' Imports data from google forms (sheets). Expects study data to be in a specific format:
#'
#' 0:n pre questions (demographics etc.)
#' 0:n questions per condition for 0:n conditions
#' 0:n post study questions
#'
#' @param url the spredsheet url
#' @param pre_questions the number of pre question columns
#' @param post_questions the number of post question columns
#' @param nr_of_conditions the number of conditions
#' @param participant_column the column number containing the participant id.
#'
#' @param x The data
#' @export
import_google_form <- function(url, pre_questions, post_questions, nr_of_conditions, participant_column) {
  require(tidyverse)
  require(googlesheets4)
  require(janitor)


  data.raw <- googlesheets4::read_sheet(url) %>% janitor::clean_names()

  data.pre <- data.raw %>% select(1:all_of(pre_questions))
  data.post <- data.raw %>% select(all_of(participant_column), (ncol(data.raw) - all_of(post_questions) + 1):ncol(data.raw))
  questions_per_condition <- (ncol(data.raw) - pre_questions - post_questions) / nr_of_conditions

  data.main <- NULL

  for(condition in 1:nr_of_conditions) {
    start <- 1 + (condition - 1) * questions_per_condition + pre_questions
    end <- condition * questions_per_condition + pre_questions

    data.tmp <- data.raw %>% select(all_of(participant_column), start:end)

    if(!is.null(data.main)) {
      names(data.tmp) <- names(data.main)
    }

    data.main <- rbind(data.main, data.tmp)
  }

  return(list("pre" = data.pre, "main" = data.main, "post" = data.post))
}

#' descriptives
#'
#' calculate descriptives for all subsets
#'
#'
#' @param data the data
#' @param DV the dependent variable
#' @param IVs a list of independent variables
#'
#' @export
descriptives <- function(data, DV, IVs) {
  require(tidyverse)
  require(sets)

  groupnames.sets <- as.list(sets::set_power(IVs))
  groupnames.sets <- groupnames.sets[lapply(groupnames.sets,length)>0]

  if("package:sets" %in% search()) detach("package:sets", unload=TRUE) # sets is evil.

  result <- list()

  for(set in groupnames.sets) {
    vars <- as.list(set)
    name = paste(vars, collapse ="_x_")

    tmp <- data %>%
      group_by(across(all_of(unlist(vars)))) %>%
      summarize(n = n(),
                mean = mean(!!as.name(DV)),
                sd=sd(!!as.name(DV)),
                se=se(!!as.name(DV)),
                min=min(!!as.name(DV)),
                max=max(!!as.name(DV)),
                median=median(!!as.name(DV)),
                Q1=quantile(!!as.name(DV))[2],
                Q3=quantile(!!as.name(DV))[4],
      )

    result[[name]] <- tmp
  }

  return(result)
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
