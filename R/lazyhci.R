#' lazy_model
#'
#' This function produces a lazy mode
#' @param data the data in tidy format
#' @param participant the name of the column containing the participant identifier
#' @param within.vars (optional) a vector of strings containing names of the columns identifying within independent variables
#' @param between.vars (optional) a vector of strings containing names of the columns identifying within independent variables
#' @param make_factor indicates if columns should automatically be converted to factors (default TRUE)
#' @export
lazy_model <- function(data, participant, within.vars = NULL, between.vars = NULL, make_factor=TRUE) {

  checkmate::assert_data_frame(data)
  checkmate::assert_string(participant)
  checkmate::assert_character(within.vars, null.ok = TRUE)
  checkmate::assert_character(between.vars, null.ok = TRUE)
  checkmate::assert_logical(make_factor)
  pkg.env$assert_colnames_quietly(data, c(participant, within.vars, between.vars), only_colnames=FALSE)

  data.clean <- janitor::clean_names(data)
  participant.clean <- janitor::make_clean_names(participant)

  within.vars.clean = NULL
  between.vars.clean = NULL

  if(!is.null(within.vars))
    within.vars.clean <- janitor::make_clean_names(within.vars)
  if(!is.null((between.vars)))
    between.vars.clean <- janitor::make_clean_names(between.vars)

  if(make_factor) {
    res <- make.factors.internal(data.clean, c(within.vars.clean, between.vars.clean, participant.clean))
    if(length(res$changed) > 0) {
      cli::cli_alert_info("auto-converted columns {res$changed} to factor. You can turn off this behaviour using {.code make_factor=FALSE}.")
    }

    data.clean <- res$data
  }

  old <- list()

  for(iv in c(within.vars.clean, between.vars.clean)) {
    res <- clean_levels(unlist(data.clean[iv]))
    data.clean[iv] <- res[["dat"]]
    old[[iv]] <- res[["map"]]
  }


  source <- tibble::lst(data, participant, within.vars, between.vars, ivs = c(within.vars, between.vars), lvl = old)

  res <- tibble::lst(data = data.clean,
                     participant = participant.clean,
                     within.vars = within.vars.clean,
                     between.vars = between.vars.clean,
                     ivs = c(within.vars.clean, between.vars.clean),
                     source)

  class(res) <- "lazyhci_model"

  return(res)
}

get_pretty_lvl <- function(lazy_model, iv, lvl) {
  return(lazy_model$source$lvl[[iv]][[lvl]])
}

get_pretty_name_iv_c <-function(lazy_model, ivs) {
  return(unlist(lapply(ivs, get_pretty_name_iv, lazy_model = lazy_model)))
}

get_pretty_name_iv <- function(lazy_model, iv) {
  index <- which(lazy_model$ivs == iv)
  return(lazy_model$source$ivs[index])
}

get_pretty_name_dv_c <- function(lazy_model, dvs) {
  return(unlist(lapply(dvs, get_pretty_name_dv, lazy_model = lazy_model)))

}

get_pretty_name_dv <- function(lazy_model, dv) {
  index <- grep(dv, colnames(lazy_model$data))
  return(colnames(lazy_model$source$data)[index])
}

#' @export
print.lazyhci_model <- function(x, ...){
  cli::cli_h1("This is a lazy hci model.")

  cli::cli_alert_info(paste("The model contains data of ", x$data %>% dplyr::pull(x$participant) %>% nlevels(), " participants identified as ",
                       paste(
                         x$data %>% dplyr::pull(x$participant) %>% levels()
                         , collapse=", ")
                       ,".", sep=""))

  lazy_check <- tryCatch(lazy_check_complete_design2(x),
                         error=function(e) e,
                         warning=function(w) w)

  if(is(lazy_check, "warning")) {

    for(m in lazy_check$message) {
      cli::cli_alert_warning(m)
    }
  }


  if(!is.null(x$within.vars)) {
    cli::cli_h2("Within variables:\n")

    ulid <- cli::cli_ul()

    for(v in x$within.vars) {

      cli::cli_h3(v)

      inner <- cli::cli_ul()
      lvls <- x$data %>% dplyr::pull(v)
      for (lvl in x$data %>% dplyr::pull(v) %>% levels()) {
        cli::cli_li(lvl)
      }
      cli::cli_end(inner)

    }

    cli::cli_end(ulid)

  }

  if(!is.null(x$between.vars)) {
    cli::cli_h2("Between variables:\n")

    ulid <- cli::cli_ul()

    for(v in x$within.vars) {

      cli::cli_h3(v)

      inner <- cli::cli_ul()
      lvls <- x$data %>% dplyr::pull(v)
      for (lvl in x$data %>% dplyr::pull(v) %>% levels()) {
        cli::cli_li(lvl)
      }
      cli::cli_end(inner)

    }

    cli::cli_end(ulid)

  }

}




#lazy_analyze<- function(lazy_model) {
#  assert_lazy_model.internal(lazy_model)
#}

#' @noRd
assert_lazy_model.internal <- function(lazy_model) {
  checkmate::assert_list(lazy_model)
  checkmate::assert_names(names(lazy_model), permutation.of = c("data", "participant", "within.vars", "between.vars", "ivs", "source"))
}

#' @noRd
make.factors.internal <- function(data.clean, vars) {
  conv.vars <- c()
  for(var in vars) {
    if(!is.factor(data.clean[[var]])) {
      conv.vars <- c(var, conv.vars)
      #message(paste0("auto-converting column ", var, " to factor."))
      data.clean[[var]] <- as.factor(data.clean[[var]])
    }
  }

  result <- list(data = data.clean, changed = conv.vars)

  return(result)
}
