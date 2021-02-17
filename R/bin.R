#' Bin series
#'
#' A new class called `bin` is provided, along with different
#' functions which enable to deal easily with bins, ie creating `bin`
#' objects (`as_bin`) coercing bins to numerical values
#' (`as_numeric`), merging bins (`cut`) and checking than an object is
#' a bin (`is_bin`).
#'
#' 
#' - `extract` methods for characters and factors are provided which
#' split the character strings in a four tibble columns: the open
#' bracket, the lower bound, the upper bound and the closing bracket.
#' - `as_bin` takes as argument a character or a factor that
#' represents a bin, check the consistency of the string and return a
#' bin object with levels in the correct order and NAs when the
#' strings are malformed,
#' - the default `cut` method takes a numerical series as argument and
#' returns a factor containing bins according to a `break` vector; for
#' the bin's method, the break should be a subset of the original
#' set of breaks and a bin with fewer levels results,
#' - `as_numeric` converts a bin to a value of the underlying variable
#' defined by its relative position (from 0 lower bound to 1 upper
#' bound in the bin),
#' - `is_bin` check if the argument is a bin.
#' 
#' @name bin
#' @param data a character or a factor containing bins,
#' @param x a character or a factor: the first and last characters
#'     should be any of `[`, `(`, `]`, `)` and the other characters
#'     should be interpreted as two numerical values separated by a
#'     comma,
#' @param breaks a numerical vector of breaks which should be a subset
#'     of the initial set of breaks. If only one break is provided,
#'     all the bins with greater values are merged,
#' @param pos a numeric between 0 and 1, 0 for the lower bond, 1 for
#'     the upper bond, 0.5 for the center of the class (or any other
#'     value between 0 and 1), which indicates to `as_numeric` how the
#'     bins should be coerced to numerical values,
#' @param xfirst,xlast the center of the first (last) class, if one
#'     wants to specify something different from the average of the
#'     lower and the upper bonds,
#' @param wlast in the case where the upper bond is infinite and
#'     `xlast` is not provided, the width of the last class is set to
#'     the one of the before last class. If `wlast` is provided, it is
#'     set to the width of the before last class times `wlast`,
#' @param .name_repair see [tidyr::extract()].
#' @param ... see [base::cut()] for the `cut` method and
#'     [tidyr::extract()] for the `extract` method,
#' @return `as_bin` returns a `bin` object, `is_bin` a logical, the
#'     `extract` method a tibble, `as_numeric` a numeric and the `cut`
#'     method a `bin` object with fewer levels.
#' @importFrom tidyr extract
#' @importFrom forcats fct_relevel
#' @importFrom tidyr extract
#' @importFrom dplyr mutate select filter
#' @importFrom dplyr left_join
#' @author Yves Croissant
#' @keywords classes
#' @examples
#' # create a factor containing bins using cut on a numeric
#' z <- c(1, 5, 10, 12, 4, 9, 8)
#' bin1 <- cut(z, breaks = c(1, 8, 12, Inf), right = FALSE)
#' # extract the elements of the levels in a tibble
#' extract(bin1)
#' # coerce to a bin object
#' bin2 <- as_bin(bin1)
#' # coerce to a numeric using the center of the bins
#' as_numeric(bin2, pos = 0.5)
#' # special values for the center of the first and of the last bin
#' as_numeric(bin2, pos = 0.5, xfirst = 5, xlast = 16)
#' # same, but indicating that the width of the last class should be
#' # twice the one of the before last
#' as_numeric(bin2, pos = 0.5, xfirst = 5, wlast = 2)
#' # merge in order to get only two bins
#' cut(bin2, breaks = c(1, 12))
#' # if length of breaks is 1, this is the value for which all the bins
#' # containing greater values are merged
#' cut(bin2, breaks = 8)
#' # check that bin1 and bin2 are objects of class bin
#' is_bin(bin1)
#' is_bin(bin2)

#' @export
as_bin <- function(x){
    # coerce a series to a bin, and return NA for uncorrectly formated
    # values
    if (! is_bin(x)){
        ind_data <- length(x) > length(unique(x))
        if (ind_data){
            # for individual data, coerce to factor if necessary and
            # get the resulting levels (bin), storing the old ones in
            # olevels
            if (! is.factor(x)) x <- factor(x)
            olevels <- levels(x)
            bin <- levels(x)
        }
        # otherwise, take bin as the series or its levels if the
        # series is a factor
        else if(is.factor(x)) bin <- levels(x) else bin <- x
        # construct a tibble containing bin, extract the 4 components
        # and sort it according to the lower bond value
        bin_tbl <- extract(bin)
        # get a vector containing the bin or NA if the format is incorrect
        new_bin <- bin_tbl %>% mutate(new_bin = ifelse(is.na(.data$left), NA, bin)) %>%
            select(bin, new_bin)
        # get a vector containing the incorrect levels
        na_levels <- filter(new_bin, is.na(new_bin)) %>% pull(bin)
        new_levels <- filter(new_bin, ! is.na(new_bin)) %>% pull(bin)

        if (ind_data){            
            # for individual data, incorrect levels lead to NA values
            # and the uncorrect levels are dropped
            x[x %in% na_levels] <- NA
            x <- x[, drop = TRUE]
        }
        else{
            # for unique values of the bin, just replaced uncorrect
            # values by NAs and coerce to factor
            x[! x %in% new_levels] <- NA
            x <- factor(x)
        }
        # order the levels according to the lower bound
        x <- forcats::fct_relevel(x, new_levels)
        x <- structure(x, class = c("bin", class(x)))
    }
    x
}

#' @rdname bin
#' @export
is_bin <- function(x) inherits(x, "bin")

#' @rdname bin
#' @export
as_numeric <- function(x, pos = 0, xfirst = NULL, xlast = NULL, wlast = NULL){
    # coerce a bin to a numeric. pos indicate the position of the
    # value in the bin, from 0 (lower bound) to 1 (upper bound). The
    # center of the first and the last class can be set using xfirst
    # and xlast. For the last bin, its width can be set using the
    # wlast argument which is a multiple of the width of the just
    # before last bin
    if (! is.null(xlast) & ! is.null(wlast)) stop("only one of last or wlast should be set")
    if (! is.numeric(pos)) stop("pos should be numeric")
    if (is.numeric(pos) & ! (pos >= 0 & pos <= 1)) stop("pos should be between 0 and 1")
    # the series can contain individual data or unique values of
    # bins. The computation is done on the levels
    if (inherits(x, "bin")) bins <- extract(levels(x))
    else bins <- extract(x)
    K <- nrow(bins)
    if (! is.null(xfirst)){
        # xfirst should be inside the range of the first bin
        if (! (xfirst >= bins$first[1] & xfirst <= bins$last[1])) stop("irrelevant value for xfirst")
        # the lower bound is reset so that xfirst is now the center of the bin
        bins$first[1] <- xfirst - (bins$last[1] - xfirst)
    }
    if (! is.null(xlast)){
        if (! (xlast >= bins$first[K] & xlast <= bins$last[K])) stop("irrelevant value for last")
        # xlast should be inside the range of the last bin
        bins$last[K] <- bins$first[K] + 2 * (xlast - bins$first[K])
        # the upper bound is reset so that xlast is now the center of
        # the bin
    }
    else{
        if (is.infinite(bins$last[K])){
            # no xlast argument and the upper bound is infinite. In
            # this case define the width of the last bin by default =
            # the width of the before last bin or wlast x its width
            if (is.null(wlast)) wlast <- 1
            bins$last[K] <- bins$first[K] + wlast * (bins$last[K-1]- bins$first[K - 1])
        }
    }
    # compute the value as a weighted average of the two limits
    xnum <- (1 - pos) * bins$first + pos * bins$last
    # join on tibble that contains the original bins with the one that
    # contains the levels and the corresponding numerical value and
    # return the numerical series
    x <- tibble(bin = as.character(x)) %>%
        left_join(tibble(bin = bins$bin, xnum = xnum), by = "bin") %>%
        pull(.data$xnum)
    x
}

#' @rdname bin
#' @export
cut.bin <- function(x, breaks = NULL, ...){
    if (is.null(breaks)) stop("new breaks should be specified")
    # x breaks are provided in order to reduce the number of classes
    # first extract the bin
    new_x <- extract(x)
    # guess the value of right
    left_op <- new_x[2, 2, drop = TRUE]
    if (left_op == "[") right_closed <- FALSE else right_closed <- TRUE
    # get the initial classes and computs the breaks
    initial_breaks <- sort(union(pull(new_x, .data$first), pull(new_x, .data$last)))
    if (length(breaks) == 1){
        if (! breaks %in% initial_breaks) stop("the break value should be a bound of one of the bins")
        breaks <- initial_breaks[initial_breaks <= breaks]
    }
    # min/max values of the new breaks lower/larger than the
    # min/max values of the initial breaks are not allowed
    if (min(breaks) < min(initial_breaks)) stop("the minimal value provided is lower than the initial lower bond")
    if (max(breaks) > max(initial_breaks)) stop("the minimal value provided is lower than the initial lower bond")
    # min/max values of the initial breaks are included in the
    # new breaks if necessary
    if (! min(initial_breaks) %in% breaks) breaks <- c(min(initial_breaks), breaks)
    if (! max(initial_breaks) %in% breaks) breaks <- c(breaks, max(initial_breaks))
    # put in form the vector of new breaks and check whether
    # some values are not part of the initial breaks
    breaks <- sort(unique(breaks))
    na_breaks <- setdiff(breaks, initial_breaks)
    if (length(na_breaks) > 0) stop(paste(paste(sort(na_breaks), collapse = ", "),
                                      ifelse(length(na_breaks) == 1, "is", "are"),
                                      paste("provided in the breaks argument but ",
                                            ifelse(length(na_breaks) == 1, "is", "are"),
                                            " not part of the  initial set of breaks", sep = "")),
                                    sep = "")
    new_x <- new_x %>% mutate(center = as_numeric(as_bin(.data$bin), pos = 0.5),
                              center = cut(.data$center, breaks = breaks, right = right_closed))
    tibble(bin = as.character(x)) %>%
        left_join(new_x, by = "bin") %>%
        pull(.data$center) %>% as_bin
}

#' @rdname bin
#' @export
cut.character <- function(x, breaks = NULL, ...){
    x <- as_bin(x)
    cut(x, breaks = breaks, ...)
}

#' @rdname bin
#' @export
cut.factor <- function(x, breaks = NULL, ...){
    x <- as_bin(x)
    cut(x, breaks = breaks, ...)
}

#' @rdname bin
#' @method extract character
#' @export
extract.character <- function(data, ..., .name_repair = "check_unique"){
    # take a character series as argument that contains a bin, extract
    # the four components (lower/upper bonds and brackets) in a 4
    # columns tibble sorted according to the lower bound. Note that
    # unique values are considered (usefull when individual data are
    # provided).
    bin_tbl <- tibble(bin = unique(data))
    a_float <- "[-+]?[0-9]*\\.?[0-9]*[eE]?[-+]?[0-9]+"
    a_pattern <- paste("^", "(\\[|\\()", "(", a_float, "),(", a_float, "|Inf)", "(\\]|\\))", "$", sep = "")
    bin_tbl %>%
        tidyr::extract(.data$bin,
                       c("left", "first", "last", "right"),
                       a_pattern,
                       remove = FALSE, convert = TRUE) %>%
        arrange(.data$first)
}

#' @rdname bin
#' @method extract factor
#' @export
extract.factor <- function(data, ..., .name_repair = "check_unique"){
    # for a bin series, just coerce to character
    extract(as.character(data))
}

#' @importFrom tidyr extract
#' @export
tidyr::extract





