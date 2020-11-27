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
#' @param na.rm should the missing values be stripped before the
#'     computation of the statistic,
#' @param center the center value used to compute the deviations, one
#'     of `"median"` or `"mean"`
#' @param xlast the center of the last class
#' @param xfirst the center of the first class
#' @param w a vector of weights
#' @param probs a vector of probabilities for the quantile method
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
variance.default <- function(x, w = NULL, xlast = NULL, xfirst = NULL, ...){
    if (! (is.character(x) | is.factor(x))) stop("x should be either a factor or a character")
    if (is.factor(x)) x <- as.character(x)
    if (is.null(w)) stop("w should be indicated")
    x <- cls2val(x, 0.5, xlast = xlast, xfirst = xfirst)
    xb <- weighted.mean(x, w)
    weighted.mean((x - xb) ^ 2, w)
}


#' @rdname generics
#' @export
variance.numeric <- function(x, w = NULL, ...){
    xb <- weighted.mean(x, w)
    weighted.mean((x - xb) ^ 2, w)
}

#' @rdname generics
#' @export
stdev.default <- function(x, w = NULL, xlast = NULL, xfirst = NULL, ...)
    sqrt(variance(x = x, w = w, xlast = xlast, xfirst = xfirst, ...))

#' @rdname generics
#' @export
stdev.numeric <- function(x, w = NULL, ...)
    sqrt(variance(x = x, w = w, ...))


#' @rdname generics
#' @export
madev.default <- function(x, w = NULL, center = c("median", "mean"), xlast = NULL, xfirst = NULL, ...){
    center <- match.arg(center)
    if (! (is.factor(x) | is.character(x))) stop("x should be a character or a factor")
    if (is.factor(x)) x <- as.character(x)
    if (is.null(w)) stop("w should be indicated")
    if (center == "median") ctr <-  median(x = x, w = w, xlast = xlast, xfirst = xfirst)
    x <- cls2val(x, 0.5, xlast = xlast, xfirst = xfirst)
    if (center == "mean") ctr <- weighted.mean(x, w)
    weighted.mean(abs(x - ctr), w)
}

#' @rdname generics
#' @export
quantile.character <- function(x, w, probs = c(0.25, 0.50, 0.75), xlast = NULL, xfirst = NULL, ...){
    if (is.null(probs)) stop("don't know what values of tiles to compute")
    if (is.null(w)) stop("weights should be provided")
    if (is.null(x)) stop("a character vector should be provided")
    W <- cumsum(w)
    get_quant <- function(aprob){
        id <- (W > aprob) %>% which %>% .[1]
        if (id == 1L) Fm1 <- 0
        else Fm1 <- W[id - 1]
        F <- W[id]
        a_x_cls <- as.character(x[id])
        if (id != length(w)) xlast <- NULL
        acls2val(a_x_cls, (aprob - Fm1) / (F - Fm1), xlast = xlast, xfirst = xfirst)
    }
    map_dbl(probs, get_quant)
}
    
#' @rdname generics
#' @export
quantile.factor <- function(x, w, probs = c(0.25, 0.50, 0.75), xlast = NULL, xfirst = NULL, ...)
    quantile(x = as.character(x), w = w, probs = probs, xlast = xlast, xfirst = xfirst)


#' @rdname generics
#' @export
median.character <- function(x, na.rm, w, xlast = NULL, xfirst = NULL, ...)
    quantile(x = x, w = w, 0.5, xlast = xlast, xfirst = xfirst, ...)

#' @rdname generics
#' @export
median.factor <- function(x, na.rm, w, xlast = NULL, xfirst = NULL, ...)
    median(x = as.character(x), w = w, xlast = xlast, xfirst = xfirst, ...)

#' @rdname generics
#' @export
mean.character <- function(x, w, xlast = NULL, xfirst = NULL, ...){
    x <- cls2val(x, 0.5, xlast = xlast, xfirst = xfirst)
    weighted.mean(x = x, w = w)
}

#' @rdname generics
#' @export
mean.factor <- function(x, w, xlast = NULL, xfirst = NULL, ...){
    mean(x = as.character(x), w = w, xlast = xlast, xfirst = xfirst)
}

#' @rdname generics
#' @export
pre_print <- function(x)
    UseMethod("pre_print")
