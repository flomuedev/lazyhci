
# lazyhci - helper scripts for lazy HCI researchers
This package is a compilation of functions written over the years for fast and easy exploration of data gathered in scientific experiments. The methods can support you in the areas of plotting and analysis.

**WARNING: This package is heavily work in progress. Please trust your common sense more than the results of the package.**

## Plotting

`data_summary_plot` produces a plot summarizing results for a given dependent variable over a set of grouping (independent) variables.

`data_summary_plot_multiple` calls data_summary_plot for multiple dependent variables and saves the results to pdf files.

`data_summary_plot_all` produces summary plots for multiple dependent variables, automatically grouping by all subsets of the grouping (independent) variables.

`likert_plot_model` produces a model using the `likert` package that can be plotted.

`plot_likert` customizes plotting options for models produced by `likert_plot_model`

## Analysis

todo...

## Helper
`check_complete_design` checks for a fully-crossed design for repeated meassures designs. Informs you about missing/NA cells and other problems.

## Installing
```
library(devtools)
install_github("flomuedev/lazyhci")
```
