#' data_summary_plot_all
#'
#' This function produces summary plots of multiple dependent variables given a fixed set of grouping variables
#' @param data the data
#' @param varnames vector containing the names of the columns with the dependent variables
#' @param groupnames a vector of the names of the grouping variables
#' @param outdir (optional) a file.path pointing to the directory to save the plots. Defaults to 'plots' within the current working dir.
#' @param prefix (optional) a string to prefix the file names of the plots. Defaults to 'plot'
#' @param p.width (optional) the width of the plot
#' @param p.height (optional) the height of the plot
#' @param fun.sum (optional) the summary function to use. Defaults to mean.
#' @param fun.error (optional) the error function to use. Defaults to sd.
#' @param p.scale_fill_manual (optional) a color scale for the plot
#' @param na.rm if nas shoudl be removed before plotting
#' @export
data_summary_plot_all <- function(data, varnames, groupnames, outdir = file.path(getwd(), "plots"), prefix = "plot", p.width = NULL, p.height = NULL, fun.sum = mean, fun.error = sd, scale_fill_manual = NULL, na.rm = FALSE) {

  dir.create(outdir, showWarnings = FALSE)

  groupnames.sets <- sets::set_power(groupnames)

  for(set in groupnames.sets) {
    set <- unlist(set)
    if(any(length(set) < 1 | length(set) > 4) ) {
      #message("Sorry, I don't know how to handle the set.")
    } else {
      data_summary_plot_multiple(data = data, varnames = varnames, groupnames = set, outdir = outdir, p.width = p.width, p.height = p.height, prefix = prefix, fun.sum = fun.sum, fun.error = fun.error, scale_fill_manual = scale_fill_manual, na.rm = na.rm)
      #data_summary_plot_multiple(data, varnames, set, outdir, prefix, p.width, p.height, fun.sum, fun.error, scale_fill_manual)
    }
  }

  files <- list.files(outdir, pattern = paste("^", prefix, ".*pdf$", sep=""))
  cur.wd <- getwd()
  setwd(outdir)
  qpdf::pdf_combine(input = files,
                    output = paste0(prefix, "_summary.pdf"))
  setwd(cur.wd)

}


#' data_summary_plot_multiple
#'
#' This function produces summary plots of multiple dependent variables given a fixed set of grouping variables
#' @param data the data
#' @param varnames vector containing the names of the columns with the dependent variables
#' @param groupnames a vector of the names of the grouping variables
#' @param outdir a file.path pointing to the directory to save the plots
#' @param p.width (optional) the width of the plot
#' @param p.height (optional) the height of the plot
#' @param prefix (optional) a string to prefix the filenames of the plots
#' @param fun.sum (optional) the summary function to use. Defaults to mean.
#' @param fun.error (optional) the error function to use. Defaults to sd.
#' @param p.scale_fill_manual (optional) a color scale for the plot
#' @param na.rm (default = false) if nas should be removed before plotting
#' @export
data_summary_plot_multiple <- function(data, varnames, groupnames, outdir = file.path(getwd(), "plots"), p.width = NULL, p.height = NULL, prefix = NULL, fun.sum = mean, fun.error = sd, scale_fill_manual = NULL, na.rm = FALSE) {

  groupingString <- paste("_by", paste(groupnames, collapse="_"), sep="_")

  dir.create(outdir, showWarnings = FALSE)

  for(varname in varnames) {
    varname_fixed <- str_replace_all(varname, "[!^[:punct:]]", "")
    if(is.null(prefix)) {
      title <- paste(varname_fixed, groupingString, sep="")
    } else {
      title <- paste(prefix, "_", varname_fixed, groupingString, sep="")
    }

    p <- data_summary_plot(data = data, DV = varname, groupnames = groupnames, fun.sum = fun.sum, fun.error = fun.error, p.scale_fill_manual = scale_fill_manual, p.title = title, na.rm = na.rm)

    filename = paste(title, ".pdf", sep="")
    outPath = file.path(outdir, filename)
    message(outPath)
    if(any(is.null(p.width) | is.null(p.height)))
      ggsave(outPath, plot=p, device = cairo_pdf)
    else
      ggsave(outPath, plot=p, device = cairo_pdf, width = p.width, height = p.height)
  }
}


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
#' @param na.rm (default = false) if NAs should be removed before plotting
#' @export
data_summary_plot <- function(data, DV, groupnames, fun.sum = mean, fun.error = sd, p.scale_fill_manual = NULL, p.basesize = 10, p.title = NULL, theme.fontfamily = NULL, theme.fontfamily.device = "win", na.rm = FALSE){

  ensure_font_support.internal(theme.fontfamily, theme.fontfamily.device)

  if(any(length(groupnames) < 1 | length(groupnames) > 4) )
    stop('Sorry, only 1-4 grouping variables supported')

  data.plot <- data_summary.internal(data, DV, groupnames, fun.sum, fun.error, na.rm = na.rm)

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

#' likert_plot_model
#'
#' This function produces a likert model that can be plotted
#' @param data the data
#' @param DV the name of the column containing the dependent variable.
#' @param IVs the names of the columns containing the IVs. The IVs should be numbers representing the likert items.
#' @param participantCol the name of the column containing the participant ID
#' @param grouping (optional) one or more grouping IVs
#' @param drop (optional) one or more of the IVs that should be dropped before plotting.
#' @param survey_vector one of auto, likert-4, likert-5,likert-6, likert-7 or manual. If manual, you need to pass a survey_vector_manual
#' @param survey_vector_manual if 'survey_vector' is 'manual', pass a fitting survey vector here
#'
#' @seealso \code{\link{plot_likert}}
#'
#' @export
likert_plot_model <- function(data, DV, IVs, participantCol, grouping = NULL, drop = NULL, survey_vector = c("auto", "likert-4", "likert-5", "likert-6", "likert-7", "manual"), survey_vector_manual = NULL) {
  require(likert)
  require(rlang)
  require(reshape2)

  data.plot <- as.data.frame(data)
  data.plot$Q <- data.plot[,DV]
  data.plot$P <- data.plot[,participantCol]

  survey_vector <- rlang::arg_match(survey_vector)

  if(survey_vector == "auto") {

    uniqueItems <- length(unique(data.plot$Q))

    print(uniqueItems)

    if(uniqueItems == 4) {
      survey_vector <- "likert-4"
    } else if(uniqueItems == 5) {
      survey_vector <- "likert-5"
    } else if(uniqueItems == 6) {
      survey_vector <- "likert-6"
    } else if(uniqueItems == 7) {
      survey_vector <- "likert-7"
    } else
      stop("Cannot autodetect survey vector (too much options).")


    print(paste("WARNING: Autodetected ", survey_vector, " as survey_vector. Please override if this is not correct.", sep=""))
  }

  if(survey_vector == "likert-4")
    survey_vector <- c("strongly disagree", "disagree", "agree", "strongly agree")
  else if(survey_vector == "likert-5")
    survey_vector <- c("strongly disagree", "disagree", "neutral", "agree", "strongly agree")
  else if (survey_vector == "likert-6")
    survey_vector <- c("strongly disagree", "disagree", "somewhat disagree", "somewhat agree", "agree", "strongly agree")
  else if (survey_vector == "likert-7")
    survey_vector <- c("strongly disagree", "disagree", "somewhat disagree", "neutral", "somewhat agree", "agree", "strongly agree")
  else if (survey_vector == "manual") {
    if(is.null(survey_vector_manual))
      stop("You must pass a 'survey_vector_manual' when using manual survey vector.")
    survey_vector <- survey_vector_manual
  } else
    stop("error parsing survey_vector")

  formulaString <- "P"

  IVs.rest <- IVs
  startFactorCol <- 2
  grouping.merged <- NULL

  if(!is.null(c(grouping, drop))) {
    grouping.col <- paste0(c(grouping, drop), collapse = " + ")
    formulaString <- paste0(formulaString, " + ", grouping.col)
    IVs.rest <- IVs[!IVs %in% c(grouping, drop)]
    startFactorCol <- startFactorCol + length(c(grouping, drop))
  }

  formulaString <- paste0(formulaString, " ~ ")
  IVs.rest.col <- paste(IVs.rest, collapse = " + ")
  formulaString <- paste0(formulaString, IVs.rest.col)

  print(paste("Building Likert model for DV '", DV, "' with ", formulaString, sep=""))

  data.likert <- reshape2::dcast(data.plot, as.formula(formulaString), value.var = "Q")

  for(factorCol in startFactorCol:ncol(data.likert)) {
    data.likert[,factorCol] <- factor(data.likert[,factorCol], levels = seq(1:length(survey_vector)), labels = survey_vector)
  }

  if(!is.null(grouping)) {
    groupingColName <- paste(grouping, collapse = "x")
    grouping.cols <- data.likert %>% select(one_of(grouping))
    grouping.merged <- grouping.cols %>% unite("grouping", 1:ncol(.), sep=" & ", remove = FALSE)
  }

  startCol <- length(c(grouping, drop)) + 2

  likert <- likert::likert(data.likert[,startCol:ncol(data.likert)], grouping = grouping.merged$grouping)

  return(likert)
}

#' plot_likert
#'
#' This function produces a likert plot
#' @param likert.model a model generated by \code{\link{likert_plot_model}}
#' @param title the title of theplot
#' @param yblank (optional) if the y Axis should be blank.
#' @param ordered (optional)if the plot should be ordered.
#' @param colorscale (optional) the colorscale to use.
#' @param p.basesize (optional) that base size of the plot
#' @param percentagelabel (optional) if the percentage labels should be shown
#' @param theme.fontfamily (optional) the font family to use
#' @param theme.fontfamily.device (optional) device to use for plotting. defaults to win
#'
#' @seealso \code{\link{likert_plot_model}}
#'
#' @export
plot_likert <- function(likert.model, title, yblank=FALSE, ordered = FALSE, colorscale=NULL, p.basesize = 19, percentagelabel = FALSE, theme.fontfamily = NULL, theme.fontfamily.device = "win") {

  ensure_font_support.internal(theme.fontfamily, theme.fontfamily.device)

  if(!is.null(colorscale))
    p <- plot(likert.model, ordered = ordered, colors=colorscale)
  else
    p <- plot(likert.model, ordered = ordered)

  p <- p + ggtitle(title)

  if(!is.null(theme.fontfamily))
    p <- p + theme_minimal(base_family = theme.fontfamily, base_size = p.basesize)
  else
    p <- p + theme_minimal(base_size = p.basesize)

  p <- p +  theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position="bottom") + guides(fill = guide_legend(nrow = 1))

  if(yblank)
    p <- p + theme(axis.text.y = element_blank())

  if(!percentagelabel) {
    p <- p + theme(axis.title.x = element_blank())
  }

  return(p)
}



#' @NoRd
ensure_font_support.internal <- function(fontfamily, device="win") {
  require(extrafont)

  if(is.null(fontfamily))
    return()

  if(is.null(fonts()))
    extrafont::loadfonts(device = device)

  result <- fontfamily %in% fonts()

  if(!result)
    stop(paste0("Sorry, could not load font family ", fontfamily, ". Check spelling or run font_import() if using this the first time. If you get 'No FontName. Skipping' during font_import(), try https://stackoverflow.com/questions/61204259/how-can-i-resolve-the-no-font-name-issue-when-importing-fonts-into-r-using-ext/68642855#68642855"))

  warning("You are plotting using a custom font. When exporting to PDF, make sure to use ggsave with device = cairo_pdf for the fonts to be automatically embedded.")
}
