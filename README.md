
  

# lazyhci - helper scripts for lazy HCI researchers

This package is a compilation of functions written over the years for fast and easy exploration of data gathered in scientific experiments. The methods can support you in the areas of plotting and analysis.

  

**WARNING: This package is heavily work in progress. Please trust your common sense more than the results of the package.**

  

## Loading

  

`lazy_load` can support you in reading and parsing a large amount of csv files into one large table from a folder structure.  `{*}` accepts any characters and ignores them, `{VARIABLE}`pastes the part of the path as a new variable to the resulting data table.

	lazy_load("testdata/logs/day{*}/log_study_{participant}_{*}_{TIME}_{*}_{CONDITION}.csv")  

## Data Model
All functions in `lazy_hci` work on a `lazy_model` of your data. You can create a `lazy_model` by describing your data:

	data.model <- lazy_model(data = data.raw, participant = "SubjectID", within.vars = c("visualization", "pos_undo", "rot_undo"))
The `print` method provides informations about the model.

## Plotting

`lazy_plot` produces a plot summarizing results for a given dependent variable.
	
	lazy_plot(data.model, dv = "CollectionTime", fun.error = se) 

`lazy_plot_likert` produces a plot using the `likert` package.
`lazy_arrange_plots` arranges plots in one figure.
  
All plotting functions support a variety of customization options. Please see the individual documentation. `lazyhci` returns standard `ggplot2` objects which can be further themed and manipulated with the usual methods.
## Analysis

  `lazy_analyze2` analyzes the given `lazy_model` using different statistical tools. Currently supported are: `aov` (all sorts of (RM) ANOVAs fitted using the `afex` package), `art` (Aligned-Rank Transformation using the `ARTool` package), `lme`,`glmer` (both using the `lme4` package) and `friedman` (using base-r's `friedman.test` function.
The function automatically checks assumptions and runs corresponding post-hoc tests. Information about assumption checks, the results of the omnibus test and post-hoc tests are availble through the `print` function. Please see the documentation of the function for more information.

	data.analysis.art <- lazy_analyze2(lazy_model = data.model, dv = "HappinessRating", analysis_type = "art", posthoc.adj = "bonferroni")


## Helper

`check_complete_design2` checks for a fully-crossed design for repeated meassures designs. Informs you about missing/NA cells and other problems.

## Installing

```

library(devtools)

install_github("flomuedev/lazyhci")

```