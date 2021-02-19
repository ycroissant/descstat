Descriptive statistics consist on presenting the distribution of
series for a sample in tables (frequency table for one series,
contingency tables for two series), ploting this distribution and
computing some statistics that summarise it. **descstat** provides
a complete toolbox to perform this tasks. It has been writen using
some packages of the tidyverse (especially **dplyr**, **tidyr** and
**purrr**) and its usage follow the tidyverse conventions,
especially the selection of series using their unquoted names and
the use of the pipe operator and of tibbles.

# The bin class:

In a frequency (or contingency table), continuous numerical series
are presented as bins. Moreover, for some surveys, the individual
values are not known, but only the fact that these values belongs to
a bin. Therefore, it is crucial to be able to work easily with
bins, ie:

- creating bins from numerical values, which is performed by the
`base::cut` function which turns a numerical series to a bin,
- coercing bins to numerical values, eg getting from the `[10,20)`
bin the lower bound (10), the upper bound (20), the center (15) or
whatever other value of the bin,
- reducing the number of bins by merging some of them (for example
`[0,10)`, `[10, 20)`, `[20,30)`, `[30,Inf)` to `[0,20)`, `[20,Inf)`


these latter two tasks are performed using the new `bin` class
provided by this package and the accompanying `as_numeric` function
for the coercion to numeric and the `cut` method for bins
merging. Especially, coercing bins to their center values is the
basis of the computation of descripting statistics for bins.

# Frequency and contingency tables:

The `freq_table` and `cont_table` are based on the `dplyr::count`
function but offer a much richer interface and performs easily
usual operations which are tedious to obtain with `dplyr::count` or
`base::table` functions. This includes:

- adding a total,
- for frequency tables, computing other kind of frequencies than
the counts, for example relative frequencies, percentage,
cummulative frequencies, etc.,
- for contingency tables, computing easily the joint, marginal and
conditional distributions,
- printing easily the contingency table as a double entry table.

# Plotting the distribution:

A `pre_plot` function is provided to put the tibble in form in
order to use classic plots for univariate or bivariate
distributions. This includes histogram, frequency plot, pie chart,
cummulative plot and Lorenz curve. The final plot can then be
obtained using some geoms of **ggplot2**.

# Descriptive statistics:

A full set of statistical functions (of central tendency,
dispersion, shape, concentration and covariation) are provided and
can be applied directly on objects of class `freq_table` or
`cont_table`. Some of them are methods of generics defined by the
`base` or `stats` package, some other are defined as methods for
generics function provided by the **descstat** function when the
corresponding **R** function is not generic. For example,

- `mean` is generic, so that we wrote a
`mean.freq_table` method to compute directly the mean of a series
from a frequency table.

- `var` is not generic, so that we provide the `variance` generic
and a method for `freq_table` objects.
