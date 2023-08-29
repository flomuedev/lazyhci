#' lazy_plot
#'
#' This function provides means for plotting lazy models
#'
#' @param lazy_model the data in tidy format
#' @param dv the name of the column containing the dependent variable
#' @param ivs (optional) a vector of strings containing names of the columns identifying the independent variables to plot
#' @param fun.sum (optional) the summary function to use. Defaults to mean.
#' @param fun.error (optional) the error function to use. Defaults to sd.
#' @param p.scale_fill_manual (optional) a color scale for the plot
#' @param p.basesize (optional) font size of the plot
#' @param p.title (optinal) a title
#' @param theme.fontfamily (optional) a font for your plot. Must be registered using extrafont.
#' @param na.rm (default = false) if NAs should be removed before plotting
#' @param position = "dodge" (default = dodge) if NAs should be removed before plotting
#' @param collapse_trials (default = false) if trials should be averaged per participant before plotting
#'
#' @export
lazy_plot <- function(lazy_model, dv, ivs = NULL, fun.sum = mean, fun.error = sd, p.scale_fill_manual = NULL, p.basesize = 10, p.title = NULL, theme.fontfamily = NULL, na.rm = FALSE, position = "dodge", collapse_trials = FALSE){
  assert_lazy_model.internal(lazy_model)
  checkmate::assert_function(fun.sum)
  checkmate::assert_function(fun.error)
  checkmate::assert_list(p.scale_fill_manual, null.ok = TRUE)
  checkmate::assert_integerish(p.basesize)
  checkmate::assert_string(p.title, null.ok = TRUE)
  checkmate::assert_string(theme.fontfamily, null.ok = TRUE)
  checkmate::assert_integer(length(c(lazy_model$widthin.vars, lazy_model$between.vars)), lower = 0, upper = 4)
  assert_font_support.internal(fontfamily = theme.fontfamily)
  pkg.env$assert_colnames_quietly(lazy_model$source$data, dv, only_colnames=FALSE)
  checkmate::assert_character(ivs, null.ok = TRUE)

  DV.pretty <- dv
  dv <- janitor::make_clean_names(dv)

  if(is.null(ivs)) {
    IVs <- lazy_model$ivs
    IVs.pretty <- c(lazy_model$source$within.vars, lazy_model$source$between.vars)
  } else {
    IVs.pretty <- ivs
    IVs <- janitor::make_clean_names(ivs)
  }

  # start plotting
  p<- NULL
  data.plot <- lazy_model$data

  if(collapse_trials) {
    data.plot <- data.plot %>%
      dplyr::group_by(across(all_of(c(IVs, lazy_model$participant)))) %>%
      dplyr::summarise(
        !!dv := fun.sum(!!as.name(dv), na.rm = na.rm),
      )
  }

  data.plot <-  data.plot %>%
    dplyr::group_by(across(all_of(c(IVs)))) %>%
    dplyr::summarise(
      mean = fun.sum(!!as.name(dv), na.rm = na.rm),
      error = fun.error(!!as.name(dv), na.rm = na.rm)
    )

  if(length(IVs) == 1)
    p<- ggplot2::ggplot(data.plot, ggplot2::aes(x=!!rlang::sym(IVs[1]), y=mean, fill=!!rlang::sym(IVs[1]))) + ggplot2::xlab(IVs.pretty[1])

  else
    p<- ggplot2::ggplot(data.plot, ggplot2::aes(x=!!rlang::sym(IVs[1]), y=mean, fill=!!rlang::sym(IVs[2]))) + ggplot2::xlab(IVs.pretty[1]) + ggplot2::labs(fill = IVs.pretty[2])

  if(!is.null(p.scale_fill_manual))
    p <- p + scale_fill_manual(values = p.scale_fill_manual)

  if(position == "dodge") {
    p <- p + ggplot2::geom_bar(stat="identity",
                      position=ggplot2::position_dodge(preserve = "single")) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin=mean-error, ymax=mean+error), width = 0.2,
                    position = ggplot2::position_dodge(width = 0.9, preserve = "single"))  +
      ggplot2::ylab(DV.pretty)
  } else if(position == "stack") {
    p <- p + ggplot2::geom_bar(stat="identity",
                      position="stack") +
      #     ggplot2::geom_errorbar(ggplot2::aes(ymin=mean-error, ymax=mean+error), width = 0.2,
      #                   position = ggplot2::position_dodge(width = 0.9, preserve = "single"))  +
      ggplot2::ylab(DV.pretty)
  }



  if(length(IVs) == 3) {
    p <- p + ggplot2::facet_wrap(as.formula(paste("~", IVs[3])))
  }

  if(length(IVs) == 4) {
    p <- p + ggplot2::facet_grid(as.formula(paste(IVs[3], "~", IVs[4], sep=" ")))

  }

  if(!is.null(theme.fontfamily))
    p <- p + ggplot2::theme_minimal(base_family = theme.fontfamily, base_size = p.basesize)
  else
    p <- p + ggplot2::theme_minimal(base_size = p.basesize)

  p <- p + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) + ggplot2::theme(legend.position="bottom")

  if(!is.null(p.title)) {
    p <- p + ggplot2::ggtitle(p.title)
  }

  return(p)
}

#' likert_plot_model
#'
#' This function produces a likert model that can be plotted
#' @param data the data
#' @param dv the name of the column containing the dependent variable.
#' @param grouping (optional) one or more grouping IVs
#' @param drop (optional) one or more of the IVs that should be dropped before plotting.
#' @param survey_vector one of auto, likert-4, likert-5,likert-6, likert-7, as_is, or manual. If manual, you need to pass a survey_vector_manual
#' @param survey_vector_manual if 'survey_vector' is 'manual', pass a fitting survey vector here
#' @param title (optional) the title of theplot
#' @param yblank (optional) if the y Axis should be blank.
#' @param ordered (optional)if the plot should be ordered.
#' @param colorscale (optional) the colorscale to use.
#' @param p.basesize (optional) that base size of the plot
#' @param percentagelabel (optional) if the percentage labels should be shown
#' @param theme.fontfamily (optional) the font family to use
#'
#' @export
lazy_plot_likert <- function(lazy_model,
                             dv,
                             grouping = NULL,
                             drop = NULL,
                             survey_vector = c("auto", "likert-4", "likert-5", "likert-6", "likert-7", "manual", "as_is"),
                             survey_vector_manual = NULL,
                             title = NULL,
                             yblank=FALSE,
                             ordered = FALSE,
                             colorscale=NULL,
                             p.basesize = 10,
                             percentagelabel = FALSE,
                             theme.fontfamily = NULL,
                             wrap = 1000,
                             group.order = NULL) {

  assert_lazy_model.internal(lazy_model)
  assert_font_support.internal(fontfamily = theme.fontfamily)
  pkg.env$assert_colnames_quietly(lazy_model$source$data, dv, only_colnames=FALSE)

  DV.pretty <- dv
  dv <- janitor::make_clean_names(dv)

  IVs <- lazy_model$source$ivs

  data.plot <- as.data.frame(lazy_model$source$data)
  data.plot$Q <- data.plot[,DV.pretty]
  #data.plot$P <- data.plot[,lazy_model$participant]

  survey_vector <- rlang::arg_match(survey_vector)

  if(survey_vector == "as_is") {
    survey_vector <- "manual"
    survey_vector_manual <- levels(as.factor(data.plot$Q))

    cli::cli_alert_info(paste("Using ", survey_vector_manual, " as survey_vector for as_is. Please override if the order is not correct.", sep=""))
  }

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
      cli::cli_abort("Cannot autodetect survey vector (too much options).")


    cli::cli_alert_info(paste("Autodetected ", survey_vector, " as survey_vector. Please override if this is not correct.", sep=""))
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


  IVs.rest <- IVs[!IVs %in% c(grouping, drop)]
  startFactorCol <- 2 + length(c(grouping, drop))

  data.likert <- data.plot %>%
    dplyr::select(lazy_model$source$participant,grouping,drop,IVs.rest, Q) %>%
    tidyr::pivot_wider(names_from = IVs.rest, values_from = Q, names_sep = " | ")

  data.likert <- as.data.frame(data.likert)

  for(factorCol in startFactorCol:ncol(data.likert)) {
    data.likert[,factorCol] <- factor(data.likert[,factorCol, drop = TRUE], levels = seq(1:length(survey_vector)), labels = survey_vector)
  }

  grouping.cols <- NULL

  if(!is.null(grouping)) {
    #groupingColName <- paste(grouping, collapse = "x")
    #grouping.cols <- data.likert[,grouping]
    #grouping.merged <- grouping.cols %>% tidyr::unite("grouping", 1:ncol(.), sep=" & ", remove = FALSE)

    grouping.cols <- data.likert[,grouping]

    if(length(grouping) > 1) {
      grouping.cols <- data.likert[,grouping] %>% tidyr::unite("grouping", 1:ncol(.), sep=" & ", remove = TRUE) %>% dplyr::pull()
    }

  }

  startCol <- length(c(grouping, drop)) + 2

  likert.model <- likert::likert(data.likert[,startCol:ncol(data.likert)], grouping = grouping.cols)


  assert_font_support.internal(theme.fontfamily)

  if(is.null(colorscale)) {
    colorscale = RColorBrewer::brewer.pal(length(survey_vector), "Dark2")
  }


  if(!is.null(group.order))
    p <- plot(likert.model, ordered = ordered, colors=colorscale, wrap = wrap, group.order=group.order)
  else
    p <- plot(likert.model, ordered = ordered, colors=colorscale, wrap = wrap)

  if(!is.null(title))
    p <- p + ggtitle(title)

  if(!is.null(theme.fontfamily))
    p <- p + ggplot2::theme_minimal(base_family = theme.fontfamily, base_size = p.basesize)
  else
    p <- p + ggplot2::theme_minimal(base_size = p.basesize)

  p <- p +  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) + ggplot2::theme(legend.position="bottom") + ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1))

  if(yblank)
    p <- p + ggplot2::theme(axis.text.y = ggplot2::element_blank())

  if(!percentagelabel) {
    p <- p + ggplot2::theme(axis.title.x = ggplot2::element_blank())
  }

  return(p)
}

#' lazy_arrange_plots
#'
#' Arranges multiple ggplot grobs in one plot
#' @param nrow (optional) number of rows (defaults to 1)
#' @param position (optional) position of the legend, defaults to bottom
#'
#' @export
lazy_arrange_plots <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {

  #require(grid)
  #require(gridExtra)

  plots <- list(...)

  if(length(plots) == 1 & class(plots) == "list") {
    plots = plots[[1]]
  }

  position <- match.arg(position)
  g <- ggplot2::ggplotGrob(plots[[1]] + ggplot2::guides(colour = ggplot2::guide_legend(nrow = 1)) + ggplot2::theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + ggplot2::theme(legend.position="none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)

  combined <- switch(position,
                     "bottom" = gridExtra::arrangeGrob(do.call(gridExtra::arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = grid::unit.c(grid::unit(1, "npc") - lheight, lheight)),
                     "right" = gridExtra::arrangeGrob(do.call(gridExtra::arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = grid::unit.c(grid::unit(1, "npc") - lwidth, lwidth)))

  grid::grid.newpage()
  grid::grid.draw(combined)

  # return gtable invisibly
  invisible(combined)

}

#' @noRd
assert_font_support.internal <- function(fontfamily) {

  if(is.null(fontfamily))
    return()

  #if(is.null(extrafont::fonts()))
  #  extrafont::loadfonts(device = "all")

  extrafont::loadfonts(device = "all", quiet = TRUE)

  result <- fontfamily %in% extrafont::fonts()

  if(!result) {
    cli::cli_abort(paste0("Sorry, could not load font family ", fontfamily, ". Check spelling or run extrafont::font_import() if using this the first time. If you get 'No FontName. Skipping' during font_import(), try https://stackoverflow.com/questions/61204259/how-can-i-resolve-the-no-font-name-issue-when-importing-fonts-into-r-using-ext/68642855#68642855"))
  }

  cli::cli_alert_info("You are plotting using a custom font. When exporting to PDF, make sure to use ggsave with device = cairo_pdf for the fonts to be automatically embedded.")
  }
