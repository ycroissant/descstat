#' Functions to compute statistics
#'
#' **descstat** provide numeric functions to compute statistics on a
#' frequency or contingency table. They can be classified in the
#' following categories:
#'
#' - central tendency: `mean`, `median`, `medial`, `modval` (for the mode),
#' - dispersion: `variance`, `stdev`, `maddev` (for mean absolute
#' deviation) and quantile,
#' - shape: `skewness` and `kurtosis`,
#' - concentration: `gini`,
#' - covariation: `covariance` and `correlation`.
#'
#' When a generic function exists in base **R** (or in the `stats`
#' package), methods are provided for `freq_table` or `cont_table`,
#' this is a case for `mean`, `median` and `quantile`. When a function
#' exists, but is not generic, we provide a generic and relevant
#' methods using different names (`stdev` instead of `sd`, `variance`
#' instead of `var`, `madev` instead of `mad`, `covariance` instead of
#' `cov` and `correlation` instead of `cor`). Finally some function
#' don't exist in base **R** and packages shiped with **R**, we
#' therefore provide a `modval` function to compute the mode, `gini`
#' for the Gini concentration index, `skewness` and `kurtosis` for
#' Fisher's shape statistics and `gmean` for generalized means (which
#' include the geometric, the quadratic and the harmonic means).
#' 
#' @name statistics
#' @param x a series or a `freq_table` or a `cont_table` object,
#' @param center the center value used to compute the mean absolute
#'     deviations, one of `"median"` or `"mean"`,
#' @param w a vector of weights,
#' @param y for the quantile method, one of `"value"` or `"mass"`,
#' @param r the order of the mean for the `gmean` function,
#' @param probs the probabilities for which the quantiles have to be
#'     computed.
#' @param ... further arguments,
#' @return a numeric or a tibble
#' @importFrom stats weighted.mean quantile median na.omit
#' @importFrom purrr map_dbl map_dfr
#' @export
#' @author Yves Croissant
#'
#' @examples
#' library("dplyr")
#' z <- wages %>% freq_table(wage)
#' z %>% median
#' z %>% medial
#' z %>% modval
#' z %>% quantile(probs = c(0.25, 0.5, 0.75))
#' z %>% quantile(y = "mass", probs = c(0.25, 0.5, 0.75))
variance <- function(x, ...)
    UseMethod("variance")

#' @rdname statistics
#' @export
gmean <- function(x, r = 1, ...)
    UseMethod("gmean")

#' @rdname statistics
#' @export
gini <- function(x, ...)
    UseMethod("gini")

#' @rdname statistics
#' @export
covariance <- function(x, ...)
    UseMethod("covariance")

#' @rdname statistics
#' @export
correlation <- function(x, ...)
    UseMethod("correlation")

#' @rdname statistics
#' @export
stdev <- function(x, ...)
    UseMethod("stdev")

#' @rdname statistics
#' @export
madev <- function(x, ...)
    UseMethod("madev")

#' @rdname statistics
#' @export
modval <- function(x, ...)
    UseMethod("modval")

#' @rdname statistics
#' @export
medial <- function(x, ...)
    UseMethod("medial")

#' @rdname statistics
#' @export
kurtosis <- function(x, ...)
    UseMethod("kurtosis")

#' @rdname statistics
#' @export
skewness <- function(x, ...)
    UseMethod("skewness")

# Default method

#' @rdname statistics
#' @export
variance.default <- function(x, w = NULL, ...){
    xb <- weighted.mean(x, w)
    weighted.mean((x - xb) ^ 2, w)
}

#' @rdname statistics
#' @export
gmean.default <- function(x, r = 1, ...){
    if (r == 0) exp(mean(log(x)))
    else mean(x ^ r) ^ (1 / r)
}

#' @rdname statistics
#' @export
stdev.default <- function(x, w = NULL, ...)
    sqrt(variance(x = x, w = w, ...))


#' @rdname statistics
#' @export
madev.default <- function(x, w = NULL, center = c("median", "mean"), ...){
    center <- match.arg(center)
    if (is.null(w)) stop("w should be indicated")
    if (center == "median") ctr <-  median(x = x, w = w)
    if (center == "mean") ctr <- weighted.mean(x, w)
    weighted.mean(abs(x - ctr), w)
}

#' @rdname statistics
#' @export
skewness.default <- function(x, ...){
    N <- length(x)
    xb <- mean(x)
    sd <- sd(x) * sqrt((N - 1) / N)
    x3 <- mean((x - xb) ^ 3)
    x3 / sd ^ 3
}

#' @rdname statistics
#' @export
kurtosis.default <- function(x, ...){
    N <- length(x)
    xb <- mean(x)
    sd <- sd(x) * sqrt((N - 1) / N)
    x4 <- mean((x - xb) ^ 4)
    x4 / sd ^ 4 - 3
}

#' @rdname statistics
#' @export
mean.freq_table <- function(x, ...){
    x <- x %>% na.omit
     if (! "f" %in% names(x)){
        cf <- compute_freq(x)
        x <- x %>% mutate(f = cf)
    }
    weighted.mean(get_numval(x), x$f)
}

#' @rdname statistics
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

#' @rdname statistics
#' @export
variance.freq_table <- function(x, ...){
    x <- x %>% na.omit
    if (! "f" %in% names(x)){
        cf <- compute_freq(x)
        x <- x %>% mutate(f = cf)
    }
    variance(get_numval(x), x$f)
}

#' @rdname statistics
#' @export
stdev.freq_table <- function(x, ...)
    x %>% variance %>% sqrt

#' @rdname statistics
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

#' @rdname statistics
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

#' @rdname statistics
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

#' @rdname statistics
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
        
#' @rdname statistics
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

#' @rdname statistics
#' @export
median.freq_table <- function(x, ..., y = c("value", "mass")){
    y <- match.arg(y)
    quantile(x, y = y, 0.5)
}

#' @rdname statistics
#' @export
medial.freq_table <- function(x, ...){
    quantile(x, y = "mass", probs = 0.5, ...)
}

#' @rdname statistics
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

fun.cont_table <- function(x, fun = weighted.mean, center = "median", ...){
    type_1 <- series_type(x[[1]])
    type_2 <- series_type(x[[2]])
    y_name <- attr(x, "y")
    fun_name <- deparse(substitute(fun))
    if (is.null(y_name)){
        # joint distribution, compute the marginal, the required
        # statistics and put the result in a tibble
        if (! fun_name %in% c("covariance", "correlation")){
            #joint distribution, just get the two marginal distributions,
            #apply the function and return a one line tibble
            marg_1 <- marginal(x, 1)
            marg_2 <- marginal(x, 2)
            cats <- names(x)[1:2][which(c(type_1, type_2) == "cat")]
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
            if (fun_name != "modval") x <- tibble(x1, x2) %>% set_names(names(x)[1:2])
            else{
                series <- c(names(x1)[1], names(x2)[1])
                names(x1)[1] <- names(x2)[1] <- "value"
                x1[[1]] <- as.character(x1[[1]])
                x2[[1]] <- as.character(x2[[1]])
                x <- add_column(bind_rows(as_tibble(x1),as_tibble(x2)), series = series, .before = 1)
            }
        }
    }
    else{
        # conditional distribution
        cond_name <- setdiff(names(x)[1:2], y_name)
        levs <- pull(x, !! as.symbol(cond_name)) %>% levels
        x <- group_split(x, !! as.symbol(cond_name))
        x <- map_dbl(x,
                     function(x){
                         x <- select(x, - !! as.symbol(cond_name))
                         x <- freq_table(x, !! as.symbol(y_name), freq = .data$f)
                         fun(x)
                     }
                     )
        x <- tibble(levs, x) %>% set_names(c(cond_name, fun_name))
    }
    x
}

#' @rdname statistics
#' @export
modval.cont_table <- function(x, ...){
    fun.cont_table(x, fun = modval, ...)
}

#' @rdname statistics
#' @export
gini.cont_table <- function(x, ...){
    fun.cont_table(x, fun = gini, ...)
}

#' @rdname statistics
#' @export
skewness.cont_table <- function(x, ...)
    fun.cont_table(x, fun = skewness, ...)

#' @rdname statistics
#' @export
kurtosis.cont_table <- function(x, ...)
    fun.cont_table(x, fun = kurtosis, ...)

#' @rdname statistics
#' @export
madev.cont_table <- function(x, center = c("median", "mean"), ...){
    center <- match.arg(center)
    fun.cont_table(x, fun = madev, center = center, ...)
}

#' @rdname statistics
#' @export
mean.cont_table <- function(x, ...)
    fun.cont_table(x, fun = mean, ...)

#' @rdname statistics
#' @export
variance.cont_table <- function(x, ...)
    fun.cont_table(x, fun = variance, ...)

#' @rdname statistics
#' @export
stdev.cont_table <- function(x, ...)
    fun.cont_table(x, fun = stdev, ...)

#' @rdname statistics
#' @importFrom rlang .data
#' @export
covariance.cont_table <- function(x, ...){
    limits <- attr(x, "limits")
    x <- total.omit(x)
    x1 <- sort(unique(x[[1]])) %>%
        as_numeric(0.5, xfirst  = limits[[1]]$xfirst,
                   xlast = limits[[1]]$xlast,
                   wlast = limits[[1]]$wlast)
    x2 <- sort(unique(x[[2]])) %>%
        as_numeric(0.5, xfirst  = limits[[1]]$xfirst,
                   xlast = limits[[1]]$xlast,
                   wlast = limits[[1]]$wlast)
    x1b <- mean(x1)
    x2b <- mean(x2)
    x12 <- outer(x1 - x1b, x2-x2b)
    x <- as_matrix(x)
    sum(x12 * x / sum(x))
}

#' @rdname statistics
#' @export
correlation.cont_table <- function(x, ...){
    sdevs <- stdev(x)
    covar <- covariance(x)
    covar / sdevs[1, 1, drop = TRUE] / sdevs[1, 2, drop = TRUE]
}

