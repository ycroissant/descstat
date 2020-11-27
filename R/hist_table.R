#' Histograms
#'
#' Compute the counts for each class of  a numerical variable
#' 
#' @name hist_table
#' @aliases hist_table
#' @param data un tibble
#' @param x a numerical series (either numerical values or numerical
#'     classes)
#' @param cols a string containing `n` for counts, `f` pour
#'     frequencies and `p` for percentages ; the cumulative series are
#'     obtained using the same letters in upper caps. Compared to
#'     `freq_table`, densities can be computed using the letter `d`.
#' @param vals a character containing letters indicating the values of
#'     the variable that should be returned ; `x` for the center of
#'     the class, `l` and `u` for the lower and upper limit of the
#'     class, `a` for the range
#' @param breaks a numerical vector of class limits
#' @param xfirst a numeric indicating the value of the center of the
#'     first class
#' @param xlast a numeric indicating the center of the last class
#' @param right a logical indicating whether classes should be closed
#'     (`right = TRUE`) or open (`right = FALSE`) on the right
#' @param total a logical indicating whether the total should be
#'     returned
#' @param inflate if `xlast` is not set, the width of the last class
#'     is set to the one of the second to last width times this
#'     parameter
#' @return a tibble containing the specified values of `vals` and
#'     `cols`
#' @export
#' @importFrom dplyr all_of slice arrange tibble
#' @author Yves Croissant
#' @examples
#'
#' # price is a numeric variable, a vector of breaks should be provided
#' Padoue %>% hist_table(price, breaks = c(50, 100, 150, 200, 250, 300, 350, 400),
#'                       right = TRUE)
#' Padoue %>% hist_table(price, breaks = c(50, 100, 150, 200, 250, 300, 350, 400),
#'                       right = TRUE, cols = "fd", vals = "xa")
#' # salaire is a factor that represents the classes
#' Salaires %>% hist_table(salaire, "d")
#' # a breaks argument is provided to reduce the number of classes
#' Salaires %>% hist_table(salaire, breaks = c(10, 20, 30, 40, 50))
#' 
hist_table <- function(data, x, cols = "n", vals = "x", breaks = NULL,
                       xfirst = NULL, xlast = NULL, right = NULL,
                       total = FALSE, inflate = NULL){
    if (is.null(xlast) & is.null(inflate)) inflate <- 1
    # check wether the computation of densities is required and if so
    # create a boolean and remove d from cols
    cols_vec <- strsplit(cols, "")[[1]]
    remove_counts <- FALSE
    if (any(c("d", "m", "M") %in% cols_vec)){
        compute_densities <- ifelse("d" %in% cols_vec, TRUE, FALSE)
        compute_masses <- ifelse("m" %in% cols_vec, TRUE, FALSE)
        compute_cummasses <- ifelse("M" %in% cols_vec, TRUE, FALSE)
        cols_vec <- setdiff(cols_vec, c("m","d", "M"))
        if (! "n" %in% cols_vec){
            cols_vec <- c("n", cols_vec)
            remove_counts <- TRUE
        }
        cols <- paste(cols_vec, collapse = "")
        ## if (compute_densities) cols_vec <- c(cols_vec, "d")
        ## if (remove_counts) cols_vec <- setdiff(cols_vec, "n")
    }
    else{
        compute_densities <- FALSE
        compute_masses <- FALSE
        compute_cummasses <- FALSE
    }
    vals_vec <- strsplit(vals, "")[[1]]
    vals_na <- setdiff(vals_vec, c("a", "x", "l", "u"))
    if (length(vals_na) > 0)
        stop(paste(paste(sort(vals_na), collapse = ", "),
                   paste(" are provided in the vals argument but are not regular values", sep = ""),
                   sep = ""))
    is_numeric_x <- is.numeric(data %>% pull({{ x }}))
    if (is_numeric_x){
        # x is numeric, cut it according to the break and the left argument and then count
        if (is.null(breaks)) stop("the argument breaks should be provided")
        # right = TRUE is the default value of cut, so keep it at is
        if (is.null(right)) right <- TRUE
        # if the max value of break is lower than the maximum value of
        # x, add Inf to the vectors of breaks
        if (max(breaks) < max(data %>% pull({{ x }}))) breaks <- c(breaks, Inf)
        # if the min value of break is greater than the minimum value
        # of x, add either 0 (if min(x) >= 0) or -Inf to the vector of breaks
        if (min(breaks) > min(data %>% pull({{ x }})))
            breaks <- c(ifelse(min(data %>% pull({{ x }})) < 0, - Inf, 0), breaks)
        data <- data %>% mutate("{{ x }}" := cut({{ x }}, breaks, right = right))
    }
    else{
        if (! is.null(breaks)) data <- data %>% mutate("{{ x }}" := recut({{ x }}, breaks = breaks))
    }
    res <- freq_table(data, {{ x }}, cols = cols, total = FALSE)

    if ((any(c("x", "a") %in% vals_vec)) | compute_densities){
        res <- res %>% mutate(x = cls2val({{ x }}, 0.5, xfirst = xfirst,
                                          xlast = xlast, inflate = inflate))
    }
    if ((any(c("l", "a") %in% vals_vec)) | compute_densities)
        res <- res %>% mutate(l = cls2val({{ x }}, 0))
    if ((any(c("u", "a") %in% vals_vec)) | compute_densities)
        res <- res %>% mutate(u = cls2val({{ x }}, 1, inflate = inflate, xlast = xlast))
    if (("a" %in% vals_vec) | compute_densities){
        NR <- nrow(res)
        xlast_inf <- res %>% slice(NR) %>% pull(u) %>% is.infinite
        if (xlast_inf){
            res <- res %>% slice(NR) %>% mutate(u2 = l + 2 * (x - l)) %>% pull(u2)
            res <- res %>% mutate(u2 = ifelse(is.infinite(u), u2, u))
            res <- res %>% mutate(a = u2 - l) %>% select(- u2)
        }
        else res <- res %>% mutate(a = u - l)
        if (! "l" %in% vals_vec) res <- res %>% select(- l)
        if (! "u" %in% vals_vec) res <- res %>% select(- u)
        if (compute_densities) res <- res %>% mutate(d = n / sum(n) / a)
        if (! "a" %in% vals_vec) res <- res %>% select(- a)
    }
    if (compute_masses | compute_cummasses){
        res <- res %>% mutate(m = n * x,
                              m = m / sum(m))
        if (compute_cummasses) res <- res %>% mutate(M = cumsum(m))
        if (! compute_masses) res <- res %>% select(- m)
    }
    if (remove_counts) res <- res %>% select(- n)
    cols_pos <- match(c("n", "f", "p", "N", "F", "P", "d", "m", "M"),
                      names(res)) %>% na.omit %>% sort
    vals_pos <- match(c("x", "l", "a", "u"), names(res)) %>% na.omit %>% sort
    res <- res %>% select({{ x }}, all_of(c(vals_pos, cols_pos)))
    structure(res, class = c("hist_table", class(res)))
}
    
#' Methods for hist_table objects
#'
#' Functions and methods to compute the median, the mean, the mode,
#' the medial and quantiles for hist_table objects
#' 
#' @name hist_table.methods
#' @aliases hist_table.methods
#' @param x a hist_table object,
#' @param y for the quantile method, one of `"value"` or `"mass"`
#' @param center for the `madev` method, this can equal `"median"`
#'     (the default) or `"mean"`
#' @param ... further arguments
#' @param probs the probabilities for which the quantiles have to be
#'     computed
#' @return a  numeric
#' @export
#' @importFrom stats quantile median
#' @importFrom purrr map_dbl map_dfr
#' @author Yves Croissant
#' @examples
#'
#' z <- Salaires %>% hist_table(salaire)
#' z %>% median
#' z %>% medial
#' z %>% modval
#' z %>% quantile(probs = c(0.25, 0.5, 0.75))
#' z %>% quantile(y = "mass", probs = c(0.25, 0.5, 0.75))
mean.hist_table <- function(x, ...){
    xfirst <- x$x[1]
    xlast <- rev(x$x)[1]
    if (! "f" %in% names(x)) x <- x %>% mutate(f = compute_freq(.))
    mean(x[[1]], x$f, xlast = xlast, xfirst = xfirst)
}

#' @rdname hist_table.methods
#' @export
variance.hist_table <- function(x, ...){
    x <- x %>% rename(cls = 1)
    if (! "f" %in% names(x)) x <- x %>% mutate(f = compute_freq(.))
    xfirst <- (x$x)[1]
    xlast <- rev(x$x)[1]
    variance(x[[1]], x$f, xlast = xlast, xfirst = xfirst)
}

#' @rdname hist_table.methods
#' @export
stdev.hist_table <- function(x, ...)
    x %>% variance %>% sqrt

#' @rdname hist_table.methods
#' @export
madev.hist_table <- function(x, center = c("median", "mean"), ...){
    center <- match.arg(center)
    xfirst <- (x$x)[1]
    xlast <- rev(x$x)[1]
    if (! "f" %in% names(x)) x <- x %>% mutate(f = compute_freq(.))
    madev(x[[1]], w = x$f, center = center, xlast = xlast, xfirst = xfirst)
}

#' @rdname hist_table.methods
#' @export
modval.hist_table <- function(x, ...){
    xfirst <- (x$x)[1]
    xlast <- rev(x$x)[1]
    if (! "d" %in% names(x))
        d <- compute_dens(x, xlast = xlast, xfirst = xfirst)
    pos <- which.max(d)
    x[pos, , drop =FALSE]
}
        
#' @rdname hist_table.methods
#' @export
quantile.hist_table <- function(x, y = c("value", "mass"), probs = c(0.25, 0.5, 0.75), ...){
    y <- match.arg(y)
    if (! "f" %in% names(x)) x <- x %>% mutate(f = compute_freq(.))
    if (y == "mass"){
        if (! "m" %in% names(x)){
            x <- x %>% mutate(m = f * x,
                              m = m / sum(m))
        }
        y <- x %>% pull(m)
    }
    else y <- x %>% pull(f)
    xfirst <- pull(x, x)[1]
    xlast <- rev(pull(x, x))[1]
    quantile(as.character(x[[1]]), y, probs = probs, xlast = xlast, xfirst = xfirst)
}


#' @rdname hist_table.methods
#' @export
median.hist_table <- function(x, ..., y = c("value", "mass")){
    y <- match.arg(y)
    quantile(x, y = y, 0.5)
}


#' @rdname hist_table.methods
#' @export
medial.hist_table <- function(x, ...){
    quantile(x, y = "mass", probs = 0.5, ...)
}


#' @rdname hist_table.methods
#' @export
gini <- function(x){
    if (! inherits(x, "hist_table")) stop("x should be a hist_table object")
    if (any(! c("F", "M") %in% names(x))){
        x <- x %>% mutate(f = compute_freq(.))
        if (! "F" %in% names(x)) x <- x %>% mutate(F = cumsum(f))
        if (! "M" %in% names(x)) x <- x %>% mutate(m = f * x,
                                                   m = m / sum(m),
                                                   M = cumsum(m))
    }
    x %>% add_row(F = 0, M = 0, .before = 0) %>%
        mutate(tz = (F - lag(F)) * (lag(M) + M) / 2) %>%
        summarise(g = 2 * (0.5 - sum(tz, na.rm = TRUE))) %>%
        pull(g)    
}


compute_bonds <- function(x, xlast = NULL, xfirst = NULL, inflate = NULL){
    xu <- cls2val(x, 1L, xlast = xlast, xfirst = xfirst, inflate = inflate)
    xl <- cls2val(x, 0L, xlast = xlast, xfirst = xfirst, inflate = inflate)
    tibble(x, xl, xu)
}

compute_widths <- function(x, xlast = NULL, xfirst = NULL, inflate = NULL){
    xu <- cls2val(x, 1L, xlast = xlast, xfirst = xfirst, inflate = inflate)
    xl <- cls2val(x, 0L, xlast = xlast, xfirst = xfirst, inflate = inflate)
    xu - xl
}

compute_freq <- function(x){
    if (! inherits(x, "hist_table")) stop("x should be an hist_table object")
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
    
compute_dens <- function(x, xlast = NULL, xfirst = NULL, inflate = NULL){
    if (! inherits(x, "hist_table")) stop("x should be an hist_table object")
    if (! "d" %in% names(x)){
        f <- compute_freq(x)
        a <- compute_widths(x[[1]], xlast = xlast, xfirst = xfirst, inflate = NULL)
        d <- f /a
        d
    }
    else pull(x, d)
}
