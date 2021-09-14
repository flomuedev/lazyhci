#' data_summary_plot
#'
#' This function produces a summary plot of your data
#' @param data the data
#' @param DV the name of the column containing the dependent variable
#' @param groupnames a vector of the names of the grouping variables
#' @param fun.sum (optional) the summary function to use. Defaults to mean.
#' @param fun.error (optional) the error function to use. Defaults to sd.
#' @param p.scale_fill_manual (optional) a color scale for the plot
#' @param p.basesize (optional) font size of the plot
#' @param p.title (optinal) a title
#' @param theme.fontfamily (optional) a font for your plot. Must be registered using extrafont.
#' @param theme.fontfamily.device (optional) the device for extrafont. Defaults to win.
#' @export
data_summary_plot <- function(data, DV, groupnames, fun.sum = mean, fun.error = sd, p.scale_fill_manual = NULL, p.basesize = 10, p.title = NULL, theme.fontfamily = NULL, theme.fontfamily.device = "win"){
  
  ensure_font_support.internal(theme.fontfamily, theme.fontfamily.device)
  
  if(any(length(groupnames) < 1 | length(groupnames) > 4) )
    stop('Sorry, only 1-4 grouping variables supported')
  
  data.plot <- data_summary.internal(data, DV, groupnames, fun.sum, fun.error)
  
  p<- NULL
  
  if(length(groupnames) == 1)
    p<- ggplot(data.plot, aes_string(x=groupnames[1], y="mean", fill=groupnames[1]))
  else
    p<- ggplot(data.plot, aes_string(x=groupnames[1], y="mean", fill=groupnames[2]))
  
  if(!is.null(p.scale_fill_manual))
    p <- p + scale_fill_manual(values = p.scale_fill_manual)
  
  p <- p + geom_bar(stat="identity",
                    position=position_dodge()) +
    geom_errorbar(aes(ymin=mean-error, ymax=mean+error), width=.2,
                  position=position_dodge(.9))  +
    ylab(DV)
  
  
  if(length(groupnames) == 3) {
    p <- p + facet_wrap(as.formula(paste("~", groupnames[3])))
  }
  
  if(length(groupnames) == 4) {
    p <- p + facet_grid(as.formula(paste(groupnames[3], "~", groupnames[4], sep=" "))) 
    
  }
  
  if(!is.null(theme.fontfamily))
    p <- p + theme_minimal(base_family = theme.fontfamily, base_size = p.basesize)
  else
    p <- p + theme_minimal(base_size = p.basesize)
  
  p <- p + theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position="bottom")
  
  if(!is.null(p.title)) {
    p <- p + ggtitle(p.title)
  }
  
  return(p)
}

#' @NoRd
ensure_font_support.internal <- function(fontfamily, device="win") {
  
  if(is.null(fontfamily))
    return()
  
  if(is.null(fonts()))
    extrafont::loadfonts(device = device)
  
  result <- fontfamily %in% fonts()
  
  if(!result)
    stop(paste0("Sorry, could not load font family ", fontfamily, ". Check spelling or run font_import() if using this the first time. If you get 'No FontName. Skipping' during font_import(), try https://stackoverflow.com/questions/61204259/how-can-i-resolve-the-no-font-name-issue-when-importing-fonts-into-r-using-ext/68642855#68642855"))
  
  warning("You are plotting using a custom font. When exporting to PDF, make sure to use ggsave with device = cairo_pdf for the fonts to be automatically embedded.")
  
}
