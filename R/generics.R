#' Generic functions provided by the statecoaes package
#'
#' Some generics were created, some of them don't exist in base R (to
#' compute the mode and the mediale of a distribution for example) or
#' because the functions exist but are not generic (this is the case
#' of `var`, `sd`, `mad`, `cov` and `cor` which are replaced by our
#' `variance`, `stdev`, `madev`, `covariance` and `correlation`
#' generic functions
#'
#' @name generics
#' @param x the main argument, basically a character containing the
#'     classes
#' @param center the center value used to compute the deviations, one
#'     of `"median"` or `"mean"`
#' @param w a vector of weights
#' @param r the order of the mean for the `gmean` function
#' @param ... further arguments
#' @return a numeric or a tibble
#' @importFrom stats weighted.mean
#' @export
#' @author Yves Croissant
#' 
variance <- function(x, ...)
    UseMethod("variance")

#' @rdname generics
#' @export
gmean <- function(x, r = 1, ...)
    UseMethod("gmean")

#' @rdname generics
#' @export
covariance <- function(x, ...)
    UseMethod("covariance")

#' @rdname generics
#' @export
correlation <- function(x, ...)
    UseMethod("correlation")

#' @rdname generics
#' @export
stdev <- function(x, ...)
    UseMethod("stdev")

#' @rdname generics
#' @export
madev <- function(x, ...)
    UseMethod("madev")

#' @rdname generics
#' @export
modval <- function(x, ...)
    UseMethod("modval")

#' @rdname generics
#' @export
medial <- function(x, ...)
    UseMethod("medial")

#' @rdname generics
#' @export
kurtosis <- function(x, ...)
    UseMethod("kurtosis")

#' @rdname generics
#' @export
skewness <- function(x, ...)
    UseMethod("skewness")


#' @rdname generics
#' @export
variance.default <- function(x, w = NULL, ...){
    xb <- weighted.mean(x, w)
    weighted.mean((x - xb) ^ 2, w)
}

#' @rdname generics
#' @export
gmean.default <- function(x, r = 1, ...){
    if (r == 0) exp(mean(log(x)))
    else mean(x ^ r) ^ (1 / r)
}


#' @rdname generics
#' @export
stdev.default <- function(x, w = NULL, ...)
    sqrt(variance(x = x, w = w, ...))


#' @rdname generics
#' @export
madev.default <- function(x, w = NULL, center = c("median", "mean"), ...){
    center <- match.arg(center)
    if (is.null(w)) stop("w should be indicated")
    if (center == "median") ctr <-  median(x = x, w = w)
    if (center == "mean") ctr <- weighted.mean(x, w)
    weighted.mean(abs(x - ctr), w)
}

#' @rdname generics
#' @export
skewness.default <- function(x, ...){
    xb <- mean(x)
    sd <- sd(x)
    x3 <- mean((x - xb) ^ 3)
    x3 / sd ^ 3
}

#' @rdname generics
#' @export
kurtosis.default <- function(x, ...){
    xb <- mean(x)
    sd <- sd(x)
    x4 <- mean((x - xb) ^ 4)
    x4 / sd ^ 4 - 3
}

#' @rdname generics
#' @export
pre_print <- function(x)
    UseMethod("pre_print")
