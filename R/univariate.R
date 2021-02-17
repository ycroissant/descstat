cos#' Functions to compute statistics on univariate distributions
#'
#' **descstat** provide functions to compute statistics on an
#' univariate distribution. This includes central tendency,
#' dispersion, shape and concentration.
#'
#'
#' The following functions are provided:
#' 
#' - central tendency: `mean`, `median`, `medial`, `modval` (for the mode),
#' - dispersion: `variance`, `stdev`, `maddev` (for mean absolute
#' deviation) and quantile,
#' - shape: `skewness` and `kurtosis`,
#' - concentration: `gini`.
#'
#' When a generic function exists in base **R** (or in the `stats`
#' package), methods are provided for `freq_table` or `cont_table`,
#' this is a case for `mean`, `median` and `quantile`. When a function
#' exists, but is not generic, we provide a generic and relevant
#' methods using different names (`stdev`, `variance` and `madev`
#' instead respectively of `sd`, `var` and `mad`). Finally some
#' function don't exist in base **R** and recommended packages, we
#' therefore provide a `modval` function to compute the mode, `gini`
#' for the Gini concentration index, `skewness` and `kurtosis` for
#' Fisher's shape statistics and `gmean` for generalized means (which
#' include the geometric, the quadratic and the harmonic means).
#'
#' `madev` has a center argument which indicates whether the
#' deviations should be computed respective to the mean or to the
#' median.
#'
#' `gmean` has a `r` argument: values of -1, 0, 1 and 2 lead
#' respectively to the harmonic, geometric, arithmetic and quadratic
#' means.
#' 
#' @name univariate
#' @param x a series or a `freq_table` or a `cont_table` object,
#' @param center the center value used to compute the mean absolute
#'     deviations, one of `"median"` or `"mean"`,
#' @param w a vector of weights,
#' @param y for the quantile method, one of `"value"` or `"mass"`,
#' @param r the order of the mean for the `gmean` function,
#' @param probs the probabilities for which the quantiles have to be
#'     computed.
#' @param ... further arguments,
#' @return a numeric or a tibble.
#' @importFrom stats weighted.mean quantile median na.omit
#' @importFrom purrr map_dbl map_dfr
#' @export
#' @author Yves Croissant
#' @keywords univar
#' @examples
#' library("dplyr")
#' z <- wages %>% freq_table(wage)
#' z %>% median
#' # the medial is the 0.5 quantile of the mass of the distribution
#' z %>% medial
#' # the modval function returns the mode, it is a one line tibble
#' z %>% modval
#' z %>% quantile(probs = c(0.25, 0.5, 0.75))
#' # quantiles can compute for the frequency (the default) or the mass
#' # of the series
#' z %>% quantile(y = "mass", probs = c(0.25, 0.5, 0.75))
#' # univariate statistics can be computed on the joint, marginal or
#' # conditional distributions for cont_table objects
#' wages %>% cont_table(wage, size) %>% joint
#' wages %>% cont_table(wage, size) %>% marginal(size) %>% mean
#' wages %>% cont_table(wage, size) %>% conditional(size) %>% mean
#' 
variance <- function(x, ...)
    UseMethod("variance")

#' @rdname univariate
#' @export
gmean <- function(x, r = 1, ...)
    UseMethod("gmean")

#' @rdname univariate
#' @export
gini <- function(x, ...)
    UseMethod("gini")

#' @rdname univariate
#' @export
stdev <- function(x, ...)
    UseMethod("stdev")

#' @rdname univariate
#' @export
madev <- function(x, ...)
    UseMethod("madev")

#' @rdname univariate
#' @export
modval <- function(x, ...)
    UseMethod("modval")

#' @rdname univariate
#' @export
medial <- function(x, ...)
    UseMethod("medial")

#' @rdname univariate
#' @export
kurtosis <- function(x, ...)
    UseMethod("kurtosis")

#' @rdname univariate
#' @export
skewness <- function(x, ...)
    UseMethod("skewness")

# Default method

#' @rdname univariate
#' @export
variance.default <- function(x, w = NULL, ...){
    xb <- weighted.mean(x, w)
    weighted.mean((x - xb) ^ 2, w)
}

#' @rdname univariate
#' @export
gmean.default <- function(x, r = 1, ...){
    if (r == 0) exp(mean(log(x)))
    else mean(x ^ r) ^ (1 / r)
}

#' @rdname univariate
#' @export
stdev.default <- function(x, w = NULL, ...)
    sqrt(variance(x = x, w = w, ...))


#' @rdname univariate
#' @export
madev.default <- function(x, w = NULL, center = c("median", "mean"), ...){
    center <- match.arg(center)
    if (is.null(w)) stop("w should be indicated")
    if (center == "median") ctr <-  median(x = x, w = w)
    if (center == "mean") ctr <- weighted.mean(x, w)
    weighted.mean(abs(x - ctr), w)
}

#' @rdname univariate
#' @export
skewness.default <- function(x, ...){
    N <- length(x)
    xb <- mean(x)
    sd <- sd(x) * sqrt((N - 1) / N)
    x3 <- mean((x - xb) ^ 3)
    x3 / sd ^ 3
}

#' @rdname univariate
#' @export
kurtosis.default <- function(x, ...){
    N <- length(x)
    xb <- mean(x)
    sd <- sd(x) * sqrt((N - 1) / N)
    x4 <- mean((x - xb) ^ 4)
    x4 / sd ^ 4 - 3
}

#' @rdname univariate
#' @export
mean.freq_table <- function(x, ...){
    x <- x %>% na.omit
     if (! "f" %in% names(x)){
        cf <- compute_freq(x)
        x <- x %>% mutate(f = cf)
    }
    weighted.mean(get_numval(x), x$f)
}

#' @rdname univariate
#' @export
gmean.freq_table <- function(x, r = 1, ...){
    x <- x %>% na.omit
     if (! "f" %in% names(x)){
        cf <- compute_freq(x)
        x <- x %>% mutate(f = cf)
    }
    if (r  == 0) exp(weighted.mean(log(get_numval(x)), x$f))
    else weighted.mean(get_numval(x) ^ r, x$f) ^ (1 / r)
}

#' @rdname univariate
#' @export
variance.freq_table <- function(x, ...){
    x <- x %>% na.omit
    if (! "f" %in% names(x)){
        cf <- compute_freq(x)
        x <- x %>% mutate(f = cf)
    }
    variance(get_numval(x), x$f)
}

#' @rdname univariate
#' @export
stdev.freq_table <- function(x, ...)
    x %>% variance %>% sqrt

#' @rdname univariate
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

#' @rdname univariate
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

#' @rdname univariate
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
    weighted.mean(abs(get_numval(x) - center), x$f)
}

#' @rdname univariate
#' @export
modval.freq_table <- function(x, ...){
    if (series_type(x[[1]]) != "bin"){
        f <- compute_freq(x)
        pos <- which.max(f)
        f_max <- x[[2]][pos]
        x_max <- x[[1]][pos]
        result <- tibble(x_max, f_max) %>% set_names(c(names(x)))
    }
    else{
        # the series is a bin, compute the density and get the
        # highest
        d <- compute_dens(x)
        pos <- which.max(d)
        result <- x[pos, , drop =FALSE]
    }
    result
}
        
#' @rdname univariate
#' @importFrom dplyr bind_cols
#' @export
quantile.freq_table <- function(x, y = c("value", "mass"), probs = c(0.25, 0.5, 0.75), ...){
    x_is_char <- is.character(x[[1]])
    x <- x %>% na.omit
    if (! is.numeric(x[[1]])) bds <- extract(x[[1]]) %>% select(low = .data$first, up = .data$last)
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

#' @rdname univariate
#' @export
median.freq_table <- function(x, ..., y = c("value", "mass")){
    y <- match.arg(y)
    quantile(x, y = y, 0.5)
}

#' @rdname univariate
#' @export
medial.freq_table <- function(x, ...){
    quantile(x, y = "mass", probs = 0.5, ...)
}

#' @rdname univariate
#' @importFrom dplyr lag
#' @export
gini.freq_table <- function(x, ...){
    x <- x %>% na.omit
    if (! inherits(x, "freq_table")) stop("x should be a freq_table object")
    x_is_char <- is.character(x[[1]])
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

# Cont_table methods    

fun.cont_table <- function(data, fun = weighted.mean, center = "median", ...){
    type_1 <- series_type(data[[1]])
    type_2 <- series_type(data[[2]])
    x_name <- attr(data, "x")
    fun_name <- deparse(substitute(fun))
    if (is.null(x_name)){
        # joint distribution, compute the marginal, the required
        # statistics and put the result in a tibble
        if (! fun_name %in% c("covariance", "correlation")){
            #joint distribution, just get the two marginal distributions,
            #apply the function and return a one line tibble
            marg_1 <- marginal(data, 1)
            marg_2 <- marginal(data, 2)
            cats <- names(data)[1:2][which(c(type_1, type_2) == "cat")]
            if (length(cats) & fun_name != "modval"){
                if (length(cats) == 1)
                    stop(paste("the computation of the", fun_name, "is irrelevant for",
                               cats, "which is a categorial series"))
                else
                    stop(paste("the computation of the", fun_name, "is irrelevant for",
                               paste(cats, collapse = " and "), "which are a categorial series"))
            }
            x1 <- fun(marg_1, center = center)
            x2 <- fun(marg_2, center = center)
            if (fun_name != "modval") data <- tibble(x1, x2) %>% set_names(names(data)[1:2])
            else{
                series <- c(names(x1)[1], names(x2)[1])
                names(x1)[1] <- names(x2)[1] <- "value"
                x1[[1]] <- as.character(x1[[1]])
                x2[[1]] <- as.character(x2[[1]])
                data <- add_column(bind_rows(as_tibble(x1),as_tibble(x2)),
                                   series = series, .before = 1)
            }
        }
    }
    else{
        # conditional distribution
        cond_name <- setdiff(names(data)[1:2], x_name)
        levs <- pull(data, !! as.symbol(cond_name)) %>% levels
        data <- group_split(data, !! as.symbol(cond_name))
        data <- map_dbl(data,
                        function(x){
                            x <- select(x, - !! as.symbol(cond_name))
                            x <- freq_table(x, !! as.symbol(x_name), freq = .data$f)
                            fun(x)
                        }
                        )
        data <- tibble(levs, data) %>% set_names(c(cond_name, fun_name))
    }
    data
}

#' @rdname univariate
#' @export
modval.cont_table <- function(x, ...){
    fun.cont_table(x, fun = modval, ...)
}

#' @rdname univariate
#' @export
gini.cont_table <- function(x, ...){
    fun.cont_table(x, fun = gini, ...)
}

#' @rdname univariate
#' @export
skewness.cont_table <- function(x, ...)
    fun.cont_table(x, fun = skewness, ...)

#' @rdname univariate
#' @export
kurtosis.cont_table <- function(x, ...)
    fun.cont_table(x, fun = kurtosis, ...)

#' @rdname univariate
#' @export
madev.cont_table <- function(x, center = c("median", "mean"), ...){
    center <- match.arg(center)
    fun.cont_table(x, fun = madev, center = center, ...)
}

#' @rdname univariate
#' @export
mean.cont_table <- function(x, ...)
    fun.cont_table(x, fun = mean, ...)

#' @rdname univariate
#' @export
variance.cont_table <- function(x, ...)
    fun.cont_table(x, fun = variance, ...)

#' @rdname univariate
#' @export
stdev.cont_table <- function(x, ...)
    fun.cont_table(x, fun = stdev, ...)


