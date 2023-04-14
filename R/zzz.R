pkg.env <- NULL
`%>%` <- NULL

.onLoad <- function(libname, pkgname){
  packageStartupMessage('This is version ',  utils::packageVersion(pkgname), " of ", pkgname, ".", domain = NULL, appendLF = TRUE)

  pkg.env <<- new.env()
  pkg.env$assert_colnames_quietly <- purrr::quietly(assertable::assert_colnames)
  pkg.env$googlesheets4_read_sheet_quietly <- purrr::quietly(googlesheets4::read_sheet)

  `%>%` <<- dplyr::`%>%` #woa!
}
