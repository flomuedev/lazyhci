# summarize - helper scripts for lazy researchers
This package is a compilation of functions written over the years for fast and easy exploration of data gathered in scientific experiments. The methods can support you in the areas of plotting and analysis.

## Plotting

`data_summary_plot` produces a plot summarizing results for a given dependent variable over a set of grouping (independent) variables.
`data_summary_plot_multiple` calls data_summary_plot for multiple dependent variables and saves the results to pdf files.
`data_summary_plot_all` produces summary plots for multiple dependent variables, automatically grouping by all subsets of the grouping (independent) variables.

## Analysis

todo...

## Helper
`check_complete_design` checks for a fully-crossed design for repeated meassures designs. Informs you about missing/NA cells.

## Installing
```
library(devtools)
install_github("flomuedev/summarize")
```
