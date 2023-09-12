#' se
#'
#' This function allows you to calculate the standard error
#' @param x The data
#' @param na.rm remove NA (defaults to FALSE)
#' @export
se <- function(x, na.rm=FALSE) {
  return(sd(x, na.rm=na.rm)/sqrt(ifelse(na.rm, length(x[!is.na(x)]), length(x))))
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
#' @param verbose defaults to FALSE
#'
#' @export
lazy_import_google_form <- function(url, pre_questions, post_questions, nr_of_conditions, participant_column, verbose = FALSE) {

  sheet <- googlesheets4::read_sheet(url)
  data.raw <-  sheet %>% janitor::clean_names()

  data.pre <- data.raw %>% dplyr::select(1:all_of(pre_questions))
  data.post <- data.raw %>% dplyr::select(all_of(participant_column), (ncol(data.raw) - all_of(post_questions) + 1):ncol(data.raw))
  questions_per_condition <- (ncol(data.raw) - pre_questions - post_questions) / nr_of_conditions

  data.main <- NULL

  for(condition in 1:nr_of_conditions) {
    start <- 1 + (condition - 1) * questions_per_condition + pre_questions
    end <- condition * questions_per_condition + pre_questions

    data.tmp <- data.raw %>% dplyr::select(all_of(participant_column), start:end)

    if(!is.null(data.main)) {
      names(data.tmp) <- names(data.main)
    }

    data.main <- rbind(data.main, data.tmp)
  }

  return(list("pre" = data.pre, "main" = data.main, "post" = data.post))
}

lazy_check_complete_design2 <- function(x) {
  if(!is.null(x$within.vars)) {
    data.sum <- x$data %>%
      dplyr::group_by_at(c(x$within.vars, x$participant), .drop=FALSE) %>%
      dplyr::summarise(
        n = dplyr::n()
      )

    ns <- unique(data.sum$n)

    if(length(ns) > 1) {
      #more
      missing <- data.sum %>%
        dplyr::filter(n == 0)

      if(nrow(missing) > 0) {
        warning("This is not a complete design, go and yell at the student. The following condition data seems to be missing:")

      } else {
        warning("Your measurements are not balanced, go and yell at the student. The following condition data has fewer measurements compared to the largest condition:")
        warning(paste0(capture.output(data.sum %>% dplyr::filter(n < max(data.sum$n))), collapse = "\n"))

      }

    }

    if(length(ns) == 1) {
      if(ns > 1) {
        ##repetitions
        warning("Your data seems to contain multiple (", ns ,") repetitions (or you have not provided all IVs). We will collapse this before the ANOVA.")
      }
    }
  }

}

lazy_missing <- function(x) {
  if(!is.null(x$within.vars)) {
    data.sum <- x$data %>%
      dplyr::group_by_at(c(x$within.vars, x$participant), .drop=FALSE) %>%
      dplyr::summarise(
        n = dplyr::n()
      )

    ns <- unique(data.sum$n)

    if(length(ns) > 1) {
      #more
      missing <- data.sum %>%
        dplyr::filter(n == 0)
      return(missing)
    }
  }
  return(NULL)
}




#' lazy_check_complete_design
#'
#' This function checks if the data is complete for your design for one DV
#' @param lazy_model the data model
#' @param dv the name of the dependent variable column
#' @export
lazy_check_complete_design <- function(lazy_model, dv) {

  if(!is.null(lazy_model$within.vars)) {
    data.sum <- lazy_model$data %>%
      dplyr::group_by_at(c(lazy_model$within.vars, lazy_model$participant), .drop=FALSE) %>%
      dplyr::summarise(
        n = dplyr::n(),
        val = mean(!!as.name(dv))
      )

    missing <- data.sum %>%
      dplyr::filter(n == 0)

    higher <- data.sum %>%
      dplyr::filter(n > 1)

    na.nan <- data.sum %>%
      dplyr::filter(is.na(val) | is.nan(val))

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

  if(!is.null(lazy_model$between.vars)) {
    between.groupsizes <- lazy_model$data %>%
      dplyr::group_by_at(lazy_model$between.vars) %>%
      dplyr::summarize(n=dplyr::n()) %>%
      dplyr::select(n) %>%
      dplyr::pull() %>%
      unique() %>%
      length()

    between.groupsizes.participant <- lazy_model$data %>%
      dplyr::group_by_at(c(lazy_model$between.vars, lazy_model$participant)) %>%
      dplyr::summarize(n=dplyr::n()) %>%
      dplyr::select(n) %>%
      dplyr::pull() %>%
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

#' lazy_demo_data
#'
#' Load demo data
#' @param filename the filename
#' @export
lazy_demo_data <- function(filename) {
  return(system.file(file.path('testdata', filename), package = "lazyhci"))
}

clean_levels <-function(x) {
  if(!is.factor(x))
    return(x)

  old_lvl <- levels(x)
  names(old_lvl) <- janitor::make_clean_names(old_lvl)

  res <- forcats::fct_recode(x, !!!old_lvl)

  out <- list(dat=res, map = old_lvl)

  return(out)
}
