
**R** offer many tools to analyse the univariate or bivariate
distribution of series. This includes `table` and `prop.table` on base
**R**, and `group_by/summarise` and `count` in **dplyr**. However,
these functions are somehow frustrating as some very common tasks,
like adding a total or computing relative frequencies or percentage
and not counts are not straightforward. Moreover, to our knowledge,
**R** offer weak support for numerical series for which the numerical
value is not known at the individual level, but only the fact that
this value belongs to a certain class. `descstat` is intended to
provide user-friendly tools to perform these kind of operations. More
specifically, three kind of tables can be constructed:

- `freq_table` for frequency tables, suitable for factors or integer
  numerical series,
- `bins_table` for bins tables, suitable for numerical series,
  either provided as a numeric or as a factor containing numerical
  classes,
- `cont_table` for contingency tables of two series.

These function are writen in the **tidyverse** style, which means that
the pipe operator can be used and that the series can be provided
without quotes.
