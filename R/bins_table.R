#' Bins table
#'
#' Compute the counts for each class of  a numerical variable
#' 
#' @name bins_table
#' @aliases bins_table
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
#' @param weights a series that contain the weights tant enables the
#'     sample to mimic the population
#' @param breaks a numerical vector of class limits
#' @param xfirst a numeric indicating the value of the center of the
#'     first class
#' @param xlast a numeric indicating the center of the last class
#' @param right a logical indicating whether classes should be closed
#'     (`right = TRUE`) or open (`right = FALSE`) on the right
#' @param total a logical indicating whether the total should be
#'     returned
#' @param wlast if `xlast` is not set, the width of the last class
#'     is set to the one of the second to last width times this
#'     parameter
#' @return a tibble containing the specified values of `vals` and
#'     `cols`
#' @export
#' @importFrom dplyr all_of slice arrange tibble add_row
#' @author Yves Croissant
#' @examples
#'
#' # in table padova, price is a numeric variable, a vector of breaks should be provided
#' padova %>% bins_table(price, breaks = c(50, 100, 150, 200, 250, 300, 350, 400),
#'                       right = TRUE)
#' padova %>% bins_table(price, breaks = c(50, 100, 150, 200, 250, 300, 350, 400),
#'                       right = TRUE, cols = "fd", vals = "xa")
#' # in table wages, wage  is a factor that represents the classes
#' wages %>% bins_table(wage, "d")
#' # a breaks argument is provided to reduce the number of classes
#' wages %>% bins_table(wage, breaks = c(10, 20, 30, 40, 50))
#' 
bins_table <- function(data, x, cols = "n", vals = "x",
                       weights = NULL, breaks = NULL,
                       xfirst = NULL, xlast = NULL, right = NULL,
                       total = FALSE, wlast = NULL){
    mc <- match.call()
    m <- match(c("data", "x", "cols", "weights"), names(mc), 0)
    mc <- mc[c(1, m)]
    mc[[1]] <- as.name("freq_table")
    mc$cols <- "n"
    # check whether there are some weights, if so sum the weights,
    # else count the observations
    wgts_lgc <- deparse(substitute(weights)) != "NULL"
    if (is.null(xlast) & is.null(wlast)) wlast <- 1
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
    mc$data <- data
    mc$cols <- cols
    res <- eval(mc, parent.frame())
#    res <- freq_table(data, {{ x }}, cols = cols, weights = weights, total = FALSE)

    if ((any(c("x", "a") %in% vals_vec)) | compute_densities){
        res <- res %>% mutate(x = cls2val({{ x }}, 0.5, xfirst = xfirst,
                                          xlast = xlast, wlast = wlast))
    }
    if ((any(c("l", "a") %in% vals_vec)) | compute_densities)
        res <- res %>% mutate(l = cls2val({{ x }}, 0))
    if ((any(c("u", "a") %in% vals_vec)) | compute_densities)
        res <- res %>% mutate(u = cls2val({{ x }}, 1, wlast = wlast, xlast = xlast))
    if (("a" %in% vals_vec) | compute_densities){
        NR <- nrow(res)
        xlast_inf <- res %>% slice(NR) %>% pull(.data$u) %>% is.infinite
        if (xlast_inf){
            res <- res %>% slice(NR) %>% mutate(u2 = .data$l + 2 * (x - .data$l)) %>% pull(.data$u2)
            res <- res %>% mutate(u2 = ifelse(is.infinite(.data$u), .data$u2, .data$u))
            res <- res %>% mutate(a = .data$u2 - .data$l) %>% select(- .data$u2)
        }
        else res <- res %>% mutate(a = .data$u - .data$l)
        if (! "l" %in% vals_vec) res <- res %>% select(- .data$l)
        if (! "u" %in% vals_vec) res <- res %>% select(- .data$u)
        if (compute_densities) res <- res %>% mutate(d = n / sum(n) / .data$a)
        if (! "a" %in% vals_vec) res <- res %>% select(- .data$a)
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
    # TODO how to add a total in an bins_table
    if (total){
        lowcaps <- select(res, matches("^[nfp]{1}$", ignore.case = FALSE))
        # The series is a factor for which a Total level should be added
        levelsx <- c(levels(res[[1]]), "Total")
        res[[1]] <- factor(res[[1]], levels = levelsx)
        total_low <- lowcaps %>% summarise_all(sum) %>%
            mutate("{{ x }}" :=  factor("Total", levels = levelsx))#ifelse(x_is_num, NA, "Total"))
        res <- res %>% bind_rows(total_low)
    }

    structure(res, class = c("bins_table", class(res)))
}
    
#' Methods for bins_table objects
#'
#' Functions and methods to compute the median, the mean, the mode,
#' the medial and quantiles for bins_table objects
#' 
#' @name bins_table.methods
#' @aliases bins_table.methods
#' @param x a bins_table object,
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
#' z <- wages %>% bins_table(wage)
#' z %>% median
#' z %>% medial
#' z %>% modval
#' z %>% quantile(probs = c(0.25, 0.5, 0.75))
#' z %>% quantile(y = "mass", probs = c(0.25, 0.5, 0.75))
mean.bins_table <- function(x, ...){
    xfirst <- x$x[1]
    xlast <- rev(x$x)[1]
    if (! "f" %in% names(x)){
        cf <- compute_freq(x)
        x <- x %>% mutate(f = cf)
    }
    mean(x[[1]], x$f, xlast = xlast, xfirst = xfirst)
}

#' @rdname bins_table.methods
#' @export
variance.bins_table <- function(x, ...){
    x <- x %>% rename(cls = 1)
    if (! "f" %in% names(x)){
        cf <- compute_freq(x)
        x <- x %>% mutate(f = cf)
    }
    xfirst <- (x$x)[1]
    xlast <- rev(x$x)[1]
    variance(x[[1]], x$f, xlast = xlast, xfirst = xfirst)
}

#' @rdname bins_table.methods
#' @export
stdev.bins_table <- function(x, ...)
    x %>% variance %>% sqrt

#' @rdname bins_table.methods
#' @export
madev.bins_table <- function(x, center = c("median", "mean"), ...){
    center <- match.arg(center)
    xfirst <- (x$x)[1]
    xlast <- rev(x$x)[1]
    
    if (! "f" %in% names(x)) x <- x %>% mutate(f = compute_freq(x))
    madev(x[[1]], w = x$f, center = center, xlast = xlast, xfirst = xfirst)
}

#' @rdname bins_table.methods
#' @export
modval.bins_table <- function(x, ...){
    xfirst <- (x$x)[1]
    xlast <- rev(x$x)[1]
    if (! "d" %in% names(x))
        d <- compute_dens(x, xlast = xlast, xfirst = xfirst)
    pos <- which.max(d)
    x[pos, , drop =FALSE]
}
        
#' @rdname bins_table.methods
#' @export
quantile.bins_table <- function(x, y = c("value", "mass"), probs = c(0.25, 0.5, 0.75), ...){
    y <- match.arg(y)
    if (! "f" %in% names(x)){
        cf <- compute_freq(x)
        x <- x %>% mutate(f = cf)
    }
    if (y == "mass"){
        if (! "m" %in% names(x)){
            x <- x %>% mutate(m = .data$f * x,
                              m = .data$m / sum(.data$m))
        }
        y <- x %>% pull(.data$m)
    }
    else y <- x %>% pull(.data$f)
    xfirst <- pull(x, x)[1]
    xlast <- rev(pull(x, x))[1]
    quantile(as.character(x[[1]]), y, probs = probs, xlast = xlast, xfirst = xfirst)
}


#' @rdname bins_table.methods
#' @export
median.bins_table <- function(x, ..., y = c("value", "mass")){
    y <- match.arg(y)
    quantile(x, y = y, 0.5)
}


#' @rdname bins_table.methods
#' @export
medial.bins_table <- function(x, ...){
    quantile(x, y = "mass", probs = 0.5, ...)
}


#' @rdname bins_table.methods
#' @export
gini <- function(x){
    if (! inherits(x, "bins_table")) stop("x should be a bins_table object")
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
    if (! inherits(x, "bins_table")) stop("x should be an bins_table object")
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
    if (! inherits(x, "bins_table")) stop("x should be an bins_table object")
    if (! "d" %in% names(x)){
        f <- compute_freq(x)
        a <- compute_widths(x[[1]], xlast = xlast, xfirst = xfirst, wlast = NULL)
        d <- f /a
        d
    }
    else pull(x, d)
}
