#' se
#'
#' This function allows you to calculate the standard error
#' @param x The data
#' @export
se <- function(x, na.rm=FALSE) {
  return(sd(x, na.rm=na.rm)/sqrt(ifelse(na.rm, length(x[!is.na(x)]), length(x))))
}

#' check_complete_design
#'
#' This function checks if the data is complete for your design for one DV
#' @param data the data
#' @param IVs a vector of the names of your IV cols
#' @param participantCol the name of your participant ID col
#' @param DV the name of the dependent variable column
#' @export
check_complete_design <- function(data, IVs, participantCol, DV) {

  data.sum <- data %>% 
    dplyr::group_by_at(c(IVs, participantCol), .drop=FALSE) %>%
    dplyr::summarise(
      n = n(),
      val = mean(!!as.name(DV))
    )
  
  missing <- data.sum %>%
    filter(n == 0)
  
  higher <- data.sum %>%
    filter(n > 1)
  
  na.nan <- data.sum %>%
    filter(is.na(val) || is.nan(val))
  
  if(nrow(missing) != 0) {
    print("This is not a complete design, go and yell at the student. The following condition data is missing:")
    print("")
    print(as.data.frame(missing))
    return(FALSE)
  }
  
  if(nrow(na.nan) != 0) {
    print("There are NAs or NANs in the table, go and yell at the student. The following conditions contain NAs or NANs:")
    print("")
    print(na.nan)
    return(FALSE)
  }
  
  if(nrow(higher) != 0) {
    print("WARNING: Your data seems to contain multiple repetitions. The table should be collapsed before the ANOVA. The following conditions contain replicated data:")
    print("")
    print(higher)
  }
  
  return(TRUE)
}

#' @NoRd
data_summary.internal <- function(data, varname, groupnames, fun.sum = mean, fun.error = sd){

  data.sum <- data %>% 
    dplyr::group_by_at(groupnames) %>%
    dplyr::summarise(
      mean = fun.sum(!!as.name(varname)),
      error = fun.error(!!as.name(varname))
    )
  
  return(data.sum)
}
