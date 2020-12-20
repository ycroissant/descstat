#' Frequency table
#'
#' Compute the frequency table of a categorical or a quantitative series
#' 
#' @name freq_table
#' @aliases freq_table
#' @param data a tibble
#' @param x a categorical or numerical series
#' @param cols a string containing `n` for counts, `f` for relative
#'     frequencies, `p` for percentages and `m` for mass frequencies.
#'     Cumulative series are obtained using the same letters in upper
#'     caps
#' @param vals a character containing letters indicating the values of
#'     the variable that should be returned ; `x` for the center of
#'     the class, `l` and `u` for the lower and upper limit of the
#'     class, `a` for the range
#' @param weights a series that contain the weights tant enables the
#'     sample to mimic the population
#' @param total a logical indicating whether the total should be
#'     returned
#' @param max if the series is a discrete numerical value, this
#'     argument indicates that all the values greater than `max`
#'     should be merged in the same modality
#' @param breaks a numerical vector of class limits
#' @param right a logical indicating whether classes should be closed
#'     (`right = TRUE`) or open (`right = FALSE`) on the right
#' @param xfirst a numeric indicating the value of the center of the
#'     first class
#' @param xlast a numeric indicating the center of the last class
#' @param wlast if `xlast` is not set, the width of the last class is
#'     set to the one of the second to last width times this parameter
#' @param freq a series that contains the frequencies (only relevant
#'     if `data` is already a frequency table)
#' @param mass a series that contains the masses of the variable (only
#'     relevant if `data` is already a frequency table)
#' @param center a series that contains the center of the class of the
#'     variable (only relevant if `data` is already a frequency table)
#' @param ... argument passed to `format.tbl`
#' @param n argument passed to `format.tbl`
#' @param n_extra argument passed to `format.tbl`
#' @param width argument passed to `format.tbl`
#' @return a tibble containing the specified values of `vals` and
#'     `cols`
#' @export
#' @importFrom dplyr all_of slice arrange tibble add_row across near pull `%>%` n summarise_if summarise_all
#' @importFrom tibble add_column
#' @importFrom rlang `:=` .data
#' @importFrom tidyselect matches everything
#' @author Yves Croissant
#' @examples
#'
#' # in table padova, price is a numeric variable, a vector of breaks should be provided
#' library("dplyr")
#' padova %>% freq_table(price, breaks = c(50, 100, 150, 200, 250, 300, 350, 400),
#'                       right = TRUE)
#' padova %>% freq_table(price, breaks = c(50, 100, 150, 200, 250, 300, 350, 400),
#'                       right = TRUE, cols = "fd", vals = "xa")
#' # in table wages, wage  is a factor that represents the classes
#' wages %>% freq_table(wage, "d")
#' # a breaks argument is provided to reduce the number of classes
#' wages %>% freq_table(wage, breaks = c(10, 20, 30, 40, 50))
#' 
freq_table <- function(data, x, cols = "n", vals = NULL, weights = NULL, total = FALSE,
                       max = NULL,
                       breaks = NULL, right = NULL,
                       xfirst = NULL, xlast = NULL, wlast = NULL,
                       freq = NULL, mass = NULL, center = NULL){

    
    # If freq is provided, data is already a freq table
    freq_lgc <- deparse(substitute(freq)) != "NULL"
    mass_lgc <- deparse(substitute(mass)) != "NULL"
    center_lgc <- deparse(substitute(center)) != "NULL"
    if (center_lgc & mass_lgc) stop("only one of mass and center should be provided")

    is_freq_table <- freq_lgc
    # If x contains unique values, data should be a frequency table
    # and the freq argument is mandatory
    the_series <- pull(data, {{ x }})
    if (length(unique(the_series)) == length(the_series) & ! freq_lgc)
        stop("data seems to be a frequency table and the freq argument is mandatory")

    
    # Separate the letters of cols to get cols_vec and check whether
    # incorrect letters are provided
    get_letters <- function(x) strsplit(x, "")[[1]]
    # typology of the series
    all_cols <- c("n", "f", "p", "d", "m", "N", "F", "P", "M", "d")
    char_cols <- c("n", "f", "p")
    cols_vec <- get_letters(cols)
    vals_vec <- character(0)
    # check for incorrect series
    na_cols <- setdiff(cols_vec, all_cols)
    if (length(na_cols))
        stop(paste(paste(na_cols, collapse = ", "),
                   ifelse(length(na_cols) == 1,
                          "is not a valid column",
                          "are not valid columns")))


    # Get the type of the series : if not num, coerce to character and
    # check whether this is a cat or a bin
    if (is.numeric(the_series[1:10])){
        x_is_num <- TRUE
        x_is_cat <- FALSE
        x_is_bin <- FALSE
    }
    else{
        x_is_num <- FALSE
        if (is.factor(pull(data, {{ x }})))
            data <- mutate(data, "{{ x }}" := as.character({{ x }}))
        if (is.null(cls2lims(the_series[1:10]))){
            # the series is a cat
            x_is_cat <- TRUE
            x_is_bin <- FALSE
            # check with a narrower set of acceptable series
            na_cols <- setdiff(cols_vec, char_cols)
            if (length(na_cols))
                stop(paste(paste(na_cols, collapse = ", "),
                           ifelse(length(na_cols) == 1,
                                  "is not a valid column",
                                  "are not valid columns"),
                           "for character series"))

        }
        else{
            # the series is a bin
            x_is_cat <- FALSE
            x_is_bin <- TRUE
        }
    }


    # If breaks is provided, either a numeric series is provided and
    # it is coerced to bins using cut, or a bin series is provided and
    # the number of bins is reduced according to breaks using recut
    # (the case where a frequency table is provided is treated below)
    if (! is.null(breaks) & ! is_freq_table){
        if (x_is_cat) stop("the breaks argument is irrelevant for a categorical series")
        if (x_is_num){
            # x is numeric, cut it according to the break and the left argument and then count
            # right = TRUE is the default value of cut, so keep it at is
            if (is.null(right)) right <- FALSE
            # if the max value of break is lower than the maximum value of
            # x, add Inf to the vectors of breaks
            if (max(breaks) < max(pull(data, {{ x }}), na.rm = TRUE)) breaks <- c(breaks, Inf)
            # if the min value of break is greater than the minimum value
            # of x, add either 0 (if min(x) >= 0) or -Inf to the vector of breaks
            if (min(breaks) > min(pull(data, {{ x }}), na.rm = TRUE))
                breaks <- c(ifelse(min(pull(data, {{ x }})) < 0, - Inf, 0), breaks)
            data <- mutate(data, "{{ x }}" := cut({{ x }}, breaks, right = right))
            # the series is now a bin and not numeric
            x_is_num <- FALSE
            x_is_bin <- TRUE
        }
        else data <- mutate(data, "{{ x }}" := recut({{ x }}, breaks = breaks))
    }

    # densities and vals argument only relevant for bins
    if (! x_is_bin & "d" %in% cols_vec)
        stop("the computation of densities is only relevant for bins")
    if (! x_is_bin & ! is.null(vals))
        stop("the vals argument is only relevant for bins")
    # if the vals argument is provided, extract the letters and check
    # that all of them are relevant
    if (! is.null(vals)){
        vals_vec <- get_letters(vals)
        vals_na <- setdiff(vals_vec, c("a", "x", "l", "u"))
        if (length(vals_na) > 0)
            stop(paste(paste(sort(vals_na), collapse = ", "),
                       ifelse(length(na_cols) == 1,
                              "is not a valid values",
                              "are not valid values")))
    }
    
    # check whether there are some weights, if so sum the weights,
    # else count the observations
    if (! is_freq_table){
        wgts_lgc <- deparse(substitute(weights)) != "NULL"
        if (! wgts_lgc) ct <- data %>% group_by({{ x }}) %>%
                            summarise(n = n())
        else  ct <- data %>% group_by({{ x }}) %>%
              summarise(n = sum({{ weights }}))
        if (is.factor(ct %>% pull({{ x }})))
            ct <- mutate(ct, "{{ x }}" := as.character({{ x }}))
        # remove the missing values
        ct <- na.omit(ct)
    }
    # bin specific stuff ; compute the center of the class in any
    # case, the lower/upper bound if required or if density has to be
    # computed
    if (x_is_bin){
        # in case of bins, add a column x which is the center of the
        # bin except in the case where a freq table is provided with
        # either a mass or a center series is provided.
        if (is_freq_table){
            if (! is.null(breaks)){
                data <- mutate(data, "{{ x }}" := recut({{ x }}, breaks = breaks))
                if (center_lgc)
                    stop("the center argument is irrelevant when a frequency table is provided along with a break argument")
                data <- data %>% group_by({{ x }}) %>% summarise_if(is.numeric, sum)
            }
            if (mass_lgc | center_lgc){
                if (center_lgc) ct <- select(data,
                                             {{x }},
                                             n = {{ freq }},
                                             x = {{ center }})
                else ct <- data %>% mutate(x = {{ mass }} / {{freq}}) %>%
                         select({{ x }}, n = {{ freq }}, x)
            }
            else ct <- select(data, {{ x }}, n = {{ freq }})
        }
        if (! mass_lgc & ! center_lgc){
            ct <- add_column(ct, x = cls2val(pull(ct, {{ x }}),
                                             pos = 0.5,
                                             xlast = xlast,
                                             xfirst = xfirst,
                                             wlast = wlast),
                             .before = 2)
        }
        
        if (! is.null(vals) | "d" %in% cols_vec){
            if ("d" %in% cols_vec | any(c("a", "l") %in% vals_vec))
                ct <- add_column(ct, l = cls2val(pull(ct, {{ x }}),
                                                 pos = 0,
                                                 xlast = xlast,
                                                 xfirst = xfirst,
                                                 wlast = wlast))
            if ("d" %in% cols_vec | any(c("a", "u") %in% vals_vec))
                ct <- add_column(ct, u = cls2val(pull(ct, {{ x }}),
                                                 pos = 1,
                                                 xlast = xlast,
                                                 xfirst = xfirst,
                                                 wlast = wlast))
            if ("d" %in% cols_vec | "a" %in% vals_vec)
                ct <- mutate(ct, a = .data$u - .data$l)
        }
        # the default ordering of the bins may not be the desired one
        ct <- ct[order_bin(ct[[1]]), ]
    }
    
    # num specific stuff: merge the upper values in the last cell if
    # max is provided
    if (! is.null(max)){
        if (! x_is_num)
            stop("the max argument is only suitable for numerical series")
        ct1 <- filter(ct, {{ x }} < max)
        ct2 <- filter(ct, {{ x }} >= max) %>%
            summarise("{{ x }}" := sum(.data$n * {{ x }}) / sum(.data$n), n = sum(.data$n))
        ct <- bind_rows(ct1, ct2)
    }
    # Computation of the statistics, no check of relevance is required
    # at this point as the potential errors were previously checked
    # compute the frequencies if required
    if (any(c("f", "F", "d") %in% cols_vec)) ct <- mutate(ct, f = .data$n / sum(.data$n))
    # compute the percentages if required
    if (any(c("p", "P") %in% cols_vec)) ct <- mutate(ct, p = .data$n / sum(.data$n) * 100)
    # compute the masses if required
    if (any(c("m", "M") %in% cols_vec))
        ct <- mutate(ct, m = (.data$n * get_numval(ct)) / sum(.data$n * get_numval(ct)))
    # compute the densities if required
    if ("d" %in% cols_vec) ct <- ct %>% mutate(d = .data$f / .data$a)
    # compute the cummulative distribution if required
    if (any(c("N", "F", "P", "M") %in% cols_vec)){
        if ("N" %in% cols_vec) ct <- ct %>% mutate(ct, N = cumsum(.data$n))
        if ("F" %in% cols_vec) ct <- ct %>% mutate(ct, F = cumsum(.data$f))
        if ("P" %in% cols_vec) ct <- ct %>% mutate(ct, P = cumsum(.data$p))
        if ("M" %in% cols_vec) ct <- ct %>% mutate(ct, M = cumsum(.data$m))
    }

    # Return the required subset of statistics
    if (x_is_bin){
        cols_vec <- c(cols_vec, vals_vec)
        cols_vec <- c("x", setdiff(cols_vec, "x"))
    }
    ct <- select(ct, {{ x }}, !! cols_vec)

    # Add a total if required; in this case compute the total only for
    # non cummulative frequencies for which a total is irrelevant
    if (total){
        lowcaps <- select(ct, matches("^[nfpm]{1}$", ignore.case = FALSE))
        if (length(lowcaps)){
            total_low <- lowcaps %>% summarise_all(sum) %>%
                mutate("{{ x }}" :=  NA)
            ct <- ct %>% bind_rows(total_low)
        }
    }
    structure(ct, class = c("freq_table", class(ct)))
}


#' @rdname freq_table
#' @export
format.freq_table <- function(x, ..., n = NULL, width = NULL, n_extra = NULL){
    x <- pre_print(x)
    class(x) <- setdiff(class(x), "freq_table")
    format(x, ..., n = n, width = width, n_extra = n_extra)
}    

#' @rdname freq_table
#' @export
pre_print.freq_table <- function(x){
    is_numeric_series <- is.numeric(x[[1]])
    if (is_numeric_series){
        max_pos <- which(! near(x[[1]], floor(x[[1]])))
        max_val <- x[[1]][max_pos - 1] + 1
    }
    a_NA <- which(is.na(x[[1]]))
    if (length(a_NA)) x[[1]][a_NA] <- "Total"
    if (is_numeric_series){
        if (length(max_pos)){
            x[[1]][max_pos] <- paste(">=", max_val, sep = "")
            x[[1]] <- factor(x[[1]], levels = x[[1]])
        }
    }
    x
}

