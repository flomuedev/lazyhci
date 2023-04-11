#' lazy_model
#'
#' This function produces a lazy mode
#' @param data the data in tidy format
#' @param participant the name of the column containing the participant identifier
#' @param ivs a vector of strings containing names of the columns containing the independent variables
#' @param within.vars (optional) a vector of strings containing names of the columns identifying within independent variables
#' @param between.vars (optional) a vector of strings containing names of the columns identifying within independent variables
#' @param make_factor indicates if columns should automatically be converted to factors (default FALSE)
#' @export
lazy_model <- function(data, participant, within.vars = NULL, between.vars = NULL, make_factor=FALSE) {

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

  if(make_factor)
    data.clean <- make.factors.internal(data.clean, c(within.vars.clean, between.vars.clean, participant.clean))

  source <- tibble::lst(data, participant, within.vars, between.vars)

  return(tibble::lst(data = data.clean,
                     participant = participant.clean,
                     within.vars = within.vars.clean,
                     between.vars = between.vars.clean,
                     ivs = c(within.vars.clean, between.vars.clean),
                     source))
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
  for(var in vars) {
    if(!is.factor(data.clean[[var]])) {
      message(paste0("auto-converting column ", var, " to a factor."))
      data.clean[[var]] <- as.factor(data.clean[[var]])
    }
  }

  return(data.clean)
}
