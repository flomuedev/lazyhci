#' lazy_load
#'
#' loads data from multiple files into one data frame.
#'
#'
#' @param pattern pattern describing the path to the files and variable names to extract from the path. Variables should be encapsulated in curly brackets.
#' @param sep path seperator. Defaults to "/"
#' @param read function used for reading the files. defaults to readr::read_csv
#' @examples
#'
#' \dontrun{
#' lazy_load("testdata/logs/day{*}/log_study_{participant}_{*}_{TIME}_{*}_{CONDITION}.csv")
#' }
#' @export
lazy_load <- function(pattern, sep="/", read = readr::read_csv) {
  path.parts <- gtools::split_path(pattern)
  path.parts.globs <- lapply(X = path.parts, FUN = function(t) gsub(pattern = "\\{[^}]*\\}", replacement = "*", x = t))

  base.path <- rev(path.parts.globs)
  first.glob <- which(grepl("*", base.path, fixed = TRUE))[1]

  path <- stringr::str_c(base.path[1:first.glob-1], collapse = "/")
  glob <- stringr::str_c(tail(base.path, length(base.path)-first.glob+1), collapse = "/")

  files <- fs::dir_ls(path=path, glob=paste("*", glob, sep=""), recurse = TRUE)

  data.raw <-read(files, id = "path")

  data.raw <- data.raw %>%
    dplyr::rowwise() %>%
    dplyr::mutate(variables = list(extract_variables.internal(path, pattern))) %>%
    tidyr::unnest_wider(variables) %>%
    dplyr::select(-path)

  return(data.raw)
}


extract_variables.internal <- function(string, template) {
  # Extract the variable names from the template using a regular expression
  variable_names <- gsub("\\{([^\\{\\}]*)\\}", "\\1", unlist(stringr::str_extract_all(template, "\\{([^\\{\\}]*)\\}")))

  # Construct a regular expression pattern to extract the variable values from the string
  pattern <- gsub("\\{[^\\{\\}]*\\}", "([^/]*)", template)
  regex <- paste0("^", pattern, "$")

  # Extract the variable values using the regular expression pattern
  variable_values <- unlist(stringr::str_match(string, regex)[-1])

  # Combine the variable names and values into a named list
  res <- setNames(variable_values, variable_names)

  res <- res[!names(res) %in% c('*')]

  return(res)
}
