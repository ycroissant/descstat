#' Methods for freq_table objects
#'
#' Functions and methods to compute the median, the mean, the mode,
#' the medial and quantiles for freq_table objects
#' 
#' @name freq_table.methods
#' @aliases freq_table.methods
#' @param x a freq_table object,
#' @param y for the quantile method, one of `"value"` or `"mass"`
#' @param center for the `madev` method, this can equal `"median"`
#'     (the default) or `"mean"`,
#' @param r the order of the mean for the `gmean` function
#' @param ... further arguments
#' @param probs the probabilities for which the quantiles have to be
#'     computed
#' @return a  numeric
#' @export
#' @importFrom stats quantile median na.omit
#' @importFrom purrr map_dbl map_dfr
#' @author Yves Croissant
#' @examples
#' library("dplyr")
#' z <- wages %>% freq_table(wage)
#' z %>% median
#' z %>% medial
#' z %>% modval
#' z %>% quantile(probs = c(0.25, 0.5, 0.75))
#' z %>% quantile(y = "mass", probs = c(0.25, 0.5, 0.75))
mean.freq_table <- function(x, ...){
    x <- x %>% na.omit
    if (! is.numeric(x[[1]])){
        lims <- cls2lims(x[[1]])
        if (is.null(lims)) stop("the computation of the mean is meaningless for categorical series")
    }
    if (! "f" %in% names(x)){
        cf <- compute_freq(x)
        x <- x %>% mutate(f = cf)
    }
    weighted.mean(get_numval(x), x$f)
}


#' @rdname freq_table.methods
#' @export
gmean.freq_table <- function(x, r = 1, ...){
    x <- x %>% na.omit
    if (! is.numeric(x[[1]])){
        lims <- cls2lims(x[[1]])
        if (is.null(lims)) stop("the computation of the generalized mean is meaningless for categorical series")
    }
    if (! "f" %in% names(x)){
        cf <- compute_freq(x)
        x <- x %>% mutate(f = cf)
    }
    if (r  == 0) exp(weighted.mean(log(get_numval(x)), x$f))
    else weighted.mean(get_numval(x) ^ r, x$f) ^ (1 / r)
}

#' @rdname freq_table.methods
#' @export
variance.freq_table <- function(x, ...){
    x <- x %>% na.omit
    if (! "f" %in% names(x)){
        cf <- compute_freq(x)
        x <- x %>% mutate(f = cf)
    }
    variance(get_numval(x), x$f)
}

#' @rdname freq_table.methods
#' @export
stdev.freq_table <- function(x, ...)
    x %>% variance %>% sqrt

#' @rdname freq_table.methods
#' @export
skewness.freq_table <- function(x, ...){
    if (! "f" %in% names(x)){
        cf <- compute_freq(x)
        x <- x %>% mutate(f = cf)
    }
    xb <- mean(get_numval(x))
    sd <- sd(get_numval(x))
    mu3 <- weighted.mean((get_numval(x) - xb) ^ 3, x$f)
    mu3 /sd ^ 3
}

#' @rdname freq_table.methods
#' @export
kurtosis.freq_table <- function(x, ...){
    if (! "f" %in% names(x)){
        cf <- compute_freq(x)
        x <- x %>% mutate(f = cf)
    }
    xb <- mean(get_numval(x))
    sd <- sd(get_numval(x))
    mu4 <- weighted.mean((get_numval(x) - xb) ^ 4, x$f)
    mu4 /sd ^ 4 - 3
}


#' @rdname freq_table.methods
#' @export
madev.freq_table <- function(x, center = c("median", "mean"), ...){
    x <- x %>% na.omit
    center <- match.arg(center)
    if (center == "median") center <- median(x)
    else center <- mean(x)
    if (! "f" %in% names(x)){
        cf <- compute_freq(x)
        x <- x %>% mutate(f = cf)
    }
    weighted.mean(abs(get_numval(x) - ctr), w)
}

#' @rdname freq_table.methods
#' @export
modval.freq_table <- function(x, ...){
    if (is.numeric(x[[1]])){
        pos <- which.max(compute_freq(x))
        result <- x[[1]][pos]
    }
    else{
        # check whether the table contains bins
        lims <- cls2lims(x[[1]])
        if (is.null(lims)){
            # if not, the character is categorial, get the highest frequency
            f <- compute_freq(x)
            pos <- which.max(f)
            result <- x[[1]][pos]
        }
        else{
            # the series is a bin, compute the density and get the
            # highest
            d <- compute_dens(x)
            pos <- which.max(d)
            result <- x[pos, , drop =FALSE]
        }
    }
    result
}
        
#' @rdname freq_table.methods
#' @export
quantile.freq_table <- function(x, y = c("value", "mass"), probs = c(0.25, 0.5, 0.75), ...){
    x_is_char <- is.character(x[[1]])
    if (x_is_char){
        lims <- cls2lims(x[[1]])
        if (is.null(lims)) stop("the computation of quantiles is meaningless for categorical series")
    }
    x <- x %>% na.omit
    if (x_is_char) bds <- bounds(x)
    y <- match.arg(y)
    if (! "f" %in% names(x)){
        cf <- compute_freq(x)
        x <- x %>% mutate(f = cf)
    }
    if (y == "mass"){
        if (! "m" %in% names(x)){
            x <- x %>% mutate(m = .data$f * get_numval(x),
                              m = .data$m / sum(.data$m))
        }
        y <- x %>% pull(.data$m)
    }
    else y <- x %>% pull(.data$f)
    x <- x %>% select(1:2) %>% add_column(y = cumsum(y))
    if (x_is_char) x <- x %>% bind_cols(bds)
    aquantile <- function(p){
        if (is.character(x[[1]])){
            I <- which(x$y > p)[1]
            alpha <- (x$y[I] - p) / (x$y[I] - ifelse(I == 1, 0, x$y[I - 1]))
            q <- alpha * x$low[I] + (1 - alpha) * x$up[I]
        }
        else{
            I <- which(x$y > p)[1]
            q <- x[[1]][I]
        }
        q
    }
    sapply(probs, aquantile)
}

#' @rdname freq_table.methods
#' @export
median.freq_table <- function(x, ..., y = c("value", "mass")){
    y <- match.arg(y)
    quantile(x, y = y, 0.5)
}


#' @rdname freq_table.methods
#' @export
medial.freq_table <- function(x, ...){
    quantile(x, y = "mass", probs = 0.5, ...)
}


#' @rdname freq_table.methods
#' @export
gini <- function(x){
    x <- x %>% na.omit
    if (! inherits(x, "freq_table")) stop("x should be a freq_table object")
    x_is_char <- is.character(x[[1]])
    if (x_is_char){
        lims <- cls2lims(x[[1]])
        if (is.null(lims)) stop("the computation of quantiles is meaningless for categorical series")
    }
    else stop("the computation of the gini coefficient is only relevant for bins")

    if (any(! c("F", "M") %in% names(x))){
        cf <- compute_freq(x)
        x <- x %>% mutate(f = cf)
        if (! "F" %in% names(x)) x <- x %>% mutate(F = cumsum(.data$f))
        if (! "M" %in% names(x)) x <- x %>% mutate(m = .data$f * x,
                                                   m = .data$m / sum(.data$m),
                                                   M = cumsum(.data$m))
    }
    x %>% add_row(F = 0, M = 0, .before = 0) %>%
        mutate(tz = (F - lag(F)) * (lag(.data$M) + .data$M) / 2) %>%
        summarise(g = 2 * (0.5 - sum(.data$tz, na.rm = TRUE))) %>%
        pull(.data$g)    
}


bounds <- function(x){
    x <- x %>% na.omit
    lims <- cls2lims(x[[1]])
    lb <- lims$first
    ub <- lims$last
    x <- tibble(low = lb, up = ub, center = x$x)
    x[nrow(x), "up"] <- 2 * x[nrow(x), "center"] - x[nrow(x), "low"]
    x[1, "low"] <- 2 * x[1, "center"] - x[1, "up"]
    x
}

compute_bonds <- function(x, xlast = NULL, xfirst = NULL, wlast = NULL){
    xu <- cls2val(x, 1L, xlast = xlast, xfirst = xfirst, wlast = wlast)
    xl <- cls2val(x, 0L, xlast = xlast, xfirst = xfirst, wlast = wlast)
    tibble(x, xl, xu)
}

compute_widths <- function(x, xlast = NULL, xfirst = NULL, wlast = NULL){
    xu <- cls2val(x, 1L, xlast = xlast, xfirst = xfirst, wlast = wlast)
    xl <- cls2val(x, 0L, xlast = xlast, xfirst = xfirst, wlast = wlast)
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
    
