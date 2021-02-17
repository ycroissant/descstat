#' descstat: a toolbox for descriptive statistics
#'
#'
#' Descriptive statistics consist on presenting the distribution of
#' series for a sample in tables (frequency table for one series,
#' contingency tables for two series), ploting this distribution and
#' computing some statistics that summarise it. **descstat** provides
#' a complete toolbox to perform this tasks. It has been writen using
#' some packages of the tidyverse (especially **dplyr**, **tidyr** and
#' **purrr**) and its usage follow the tidyverse conventions,
#' especially the selection of series using their unquoted names and
#' the use of the pipe operator and of tibbles.
#'
#' @section The bin class:
#'
#' In a frequency (or contingency table), continuous numerical series
#' are presented as bins. Moreover, for some surveys, the individual
#' values are not known, but only the fact that these values belongs to
#' a bin. Therefore, it is crucial to be able to work easily with
#' bins, ie:
#'
#' - creating bins from numerical values, which is performed by the
#' `base::cut` function which turns a numerical series to a bin,
#' - coercing bins to numerical values, eg getting from the `[10,20)`
#' bin the lower bound (10), the upper bound (20), the center (15) or
#' whatever other value of the bin,
#' - reducing the number of bins by merging some of them (for example
#' `[0,10)`, `[10, 20)`, `[20,30)`, `[30,Inf)` to `[0,20)`, `[20,Inf)`
#'
#' 
#' these latter two tasks are performed using the new `bin` class
#' provided by this package and the accompanying `as_numeric` function
#' for the coercion to numeric and the `cut` method for bins
#' merging. Especially, coercing bins to their center values is the
#' basis of the computation of descripting statistics for bins.
#'
#'
#' @section Frequency and contingency tables:
#'
#' The `freq_table` and `cont_table` are based on the `dplyr::count`
#' function but offer a much richer interface and performs easily
#' usual operations which are tedious to obtain with `dplyr::count` or
#' `base::table` functions. This includes:
#'
#' - adding a total,
#' - for frequency tables, computing other kind of frequencies than
#' the counts, for example relative frequencies, percentage,
#' cummulative frequencies, etc.,
#' - for contingency tables, computing easily the joint, marginal and
#' conditional distributions,
#' - printing easily the contingency table as a double entry table.
#'
#' @section Plotting the distribution:
#'
#' A `pre_plot` function is provided to put the tibble in form in
#' order to use classic plots for univariate or bivariate
#' distributions. This includes histogram, frequency plot, pie chart,
#' cummulative plot and Lorenz curve. The final plot can then be
#' obtained using some geoms of **ggplot2**.
#'
#' @section Descriptive statistics:
#'
#' A full set of statistical functions (of central tendency,
#' dispersion, shape, concentration and covariation) are provided and
#' can be applied directly on objects of class `freq_table` or
#' `cont_table`. Some of them are methods of generics defined by the
#' `base` or `stats` package, some other are defined as methods for
#' generics function provided by the **descstat** function when the
#' corresponding **R** function is not generic. For example,
#'
#' - `mean` is generic, so that we wrote a
#' `mean.freq_table` method to compute directly the mean of a series
#' from a frequency table.
#'
#' - `var` is not generic, so that we provide the `variance` generic
#' and a method for `freq_table` objects.
#' 
#' @docType package
#' @name descstat-package
#' @keywords package
NULL


compute_widths <- function(x, xlast = NULL, xfirst = NULL, wlast = NULL){
    xu <- as_numeric(x, 1L, xlast = xlast, xfirst = xfirst, wlast = wlast)
    xl <- as_numeric(x, 0L, xlast = xlast, xfirst = xfirst, wlast = wlast)
    xu - xl
}

compute_freq <- function(x){
    if (! inherits(x, "freq_table")) stop("x should be an freq_table object")
    if (! "f" %in% names(x)){
        if (any(c("f", "p", "n") %in% names(x))){
            col <- na.omit(match(c("f", "p", "n"), names(x)))[1]
            f <- x[[col]]
            f <- f / sum(f)
        }
        else{
            if (any(c("F", "P", "N") %in% names(x))){
                col <- na.omit(match(c("F", "P", "N"), names(x)))[1]
                f <- x[[col]]
                f <- c(f[1], f[-1] - f[- length(f)])
                f <- f / sum(f)
            }
            else stop("the table should contain any of f, p, n, F, P, N")
        }
        f
    }
    else pull(x, f)
}
    
compute_dens <- function(x, xlast = NULL, xfirst = NULL, wlast = NULL){
    if (! inherits(x, "freq_table")) stop("x should be an freq_table object")
    if (! "d" %in% names(x)){
        f <- compute_freq(x)
        a <- compute_widths(x[[1]], xlast = xlast, xfirst = xfirst, wlast = NULL)
        d <- f /a
        d
    }
    else pull(x, d)
}

as_matrix <- function(x){
    x <- select(pre_print(x), -1)
    unname(as.matrix(x))
}

total.omit <- function(x) x[ ! (is.na(x[[1]]) | is.na(x[[2]])) &
                             (x[[1]] != "Total") & (x[[2]] != "Total"), ]


total.omit <- function(x) x[ ! (is.na(x[[1]]) | is.na(x[[2]])) &
                             (x[[1]] != "Total") & (x[[2]] != "Total"), ]



# get the numerical series, which is either the first column if it is
# numeric or the one called xq
get_numval <- function(x){
    x <- x %>% na.omit
    if (is.numeric(x[[1]])) x[[1]]
    else x$x
}

series_type <- function(x){
    if (is.numeric(x)) type <- "numeric"
    else{
        if (is_bin(x)) type <- "bin"
        else{
            series_as_bin <- as_bin(na.omit(unique(x)))
            if (any(is.na(series_as_bin))) type <- "cat"
            else type <- "bin"
        }
    }
    type
}
