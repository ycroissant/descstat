#' Frequency table
#'
#' Compute the frequency table of a categorical or a numerical series.
#' 
#' @name freq_table
#' @aliases freq_table
#' @param data a tibble,
#' @param x a categorical or numerical series,
#' @param f a string containing `n` for counts, `f` for relative
#'     frequencies, `p` for percentages and `m` for mass frequencies.
#'     Cumulative series are obtained using the same letters in upper
#'     caps,
#' @param vals a character containing letters indicating the values of
#'     the variable that should be returned; `x` for the center of the
#'     class, `l` and `u` for the lower and upper limit of the class,
#'     `a` for the range,
#' @param weights a series that contain the weights that enable the
#'     sample to mimic the population,
#' @param total a logical indicating whether the total should be
#'     returned,
#' @param max if the series is a discrete numerical value, this
#'     argument indicates that all the values greater than `max`
#'     should be merged in the same modality,
#' @param breaks a numerical vector of class limits,
#' @param right a logical indicating whether classes should be closed
#'     (`right = TRUE`) or open (`right = FALSE`) on the right,
#' @param xfirst,xlast,wlast see [as_numeric()],
#' @param freq a series that contains the frequencies (only relevant
#'     if `data` is already a frequency table),
#' @param mass a series that contains the masses of the variable (only
#'     relevant if `data` is already a frequency table),
#' @param center a series that contains the center of the class of the
#'     variable (only relevant if `data` is already a frequency
#'     table).
#' @return a tibble containing the specified values of `vals` and
#'     `f`.
#' @export
#' @importFrom dplyr all_of slice arrange tibble add_row across near
#'     pull `%>%` n summarise_if summarise_all group_by summarise
#'     bind_rows
#' @importFrom tibble add_column
#' @importFrom rlang `:=` .data
#' @importFrom tidyselect matches everything
#' @keywords manip
#' @author Yves Croissant
#' @examples
#' # in table padova, price is a numeric variable, a vector of breaks should be provided
#' library("dplyr")
#' padova %>% freq_table(price,
#'                       breaks = c(50, 100, 150, 200, 250, 300, 350, 400),
#'                       right = TRUE)
#' # return relative frequencies and densities, and the center value
#' # of the series and the width of the bin
#' padova %>% freq_table(price,
#'                       breaks = c(50, 100, 150, 200, 250, 300, 350, 400),
#'                       right = TRUE, f = "fd", vals = "xa")
#' # in table wages, wage is a factor that represents the classes
#' wages %>% freq_table(wage, "d")
#' # a breaks argument is provided to reduce the number of classes
#' wages %>% freq_table(wage, breaks = c(10, 20, 30, 40, 50))
#' # a total argument add a total to the frequency table
#' wages %>% freq_table(wage, breaks = c(10, 20, 30, 40, 50), total = TRUE)
#' # ìncome is already a frequency table, the freq argument
#' # is mandatory
#' income %>% freq_table(inc_class, freq = number)
#' # the mass argument can be indicated if on column contains the
#' # mass of the series in each bin. In this case, the center of the
#' # class are exactly the mean of the series in each bin
#' income %>% freq_table(inc_class, freq = number, mass = tot_inc)
#' # rgp contains a children series which indicates the number of
#' # children of the households
#' rgp %>% freq_table(children)
#' # a max argument can be indicated to merge the unusual high
#' # values of number of childre
#' rgp %>% freq_table(children, max = 4)
#' # employment is a non random survey, there is a weights series
#' # that can be used to compute the frequency table according to the
#' # sum of weights and not to counts
#' employment %>% freq_table(education)
#' employment %>% freq_table(education, weights = weights)
 
freq_table <- function(data, x, f = "n", vals = NULL, weights = NULL, total = FALSE,
                       max = NULL,
                       breaks = NULL, right = NULL,
                       xfirst = NULL, xlast = NULL, wlast = NULL,
                       freq = NULL, mass = NULL, center = NULL){

    # x can be either of class numeric, character/factor or bin ; if
    # character/factor, it is coerced to bin if possible
    
    # If freq is provided, data is already a freq table
    freq_lgc <- deparse(substitute(freq)) != "NULL"
    mass_lgc <- deparse(substitute(mass)) != "NULL"
    center_lgc <- deparse(substitute(center)) != "NULL"
    if (center_lgc & mass_lgc) stop("only one of mass and center should be provided")
    freq_data <- freq_lgc
    if (is.null(vals)) vals_vec <- character(0)
    
    # If x contains unique values, data should be a frequency table
    # and the freq argument is mandatory
    the_series <- pull(data, {{ x }})
    if (length(unique(the_series)) == length(the_series) & ! freq_data)
        stop("data seems to be a frequency table and the freq argument is mandatory")
    
    # Separate the letters of f to get f_vec and check whether
    # incorrect letters are provided
    get_letters <- function(x) strsplit(x, "")[[1]]
    # typology of the series
    all_f <- c("n", "f", "p", "d", "m", "N", "F", "P", "M", "d")
    char_f <- c("n", "f", "p")
    f_vec <- get_letters(f)
    # check for incorrect series
    na_f <- setdiff(f_vec, all_f)
    if (length(na_f))
        stop(paste(paste(na_f, collapse = ", "),
                   ifelse(length(na_f) == 1,
                          "is not a valid column",
                          "are not valid columns")))

    # Get the type of the series : if not numeric, coerce to character
    # and check whether this is a cat or a bin
    type <- series_type(the_series)
    if (type == "bin" & ! inherits(the_series, "bin"))
        data <- mutate(data, "{{ x }}" := as_bin({{ x }}))
    
    # If breaks is provided, either a numeric series is provided and
    # it is coerced to bins using cut, or a bin series is provided and
    # the number of bins is reduced according to breaks using recut
    # (the case where a frequency table is provided is treated below)
    if (! is.null(breaks) & ! freq_data){
        if (type == "cat") stop("the breaks argument is irrelevant for a categorical series")
        if (type == "numeric"){
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
            data <- mutate(data, "{{ x }}" := as_bin(cut({{ x }}, breaks, right = right)))
            # the series is now a bin and not numeric
            type <- "bin"
        }
        else data <- mutate(data, "{{ x }}" := cut({{ x }}, breaks = breaks))
    }
    # densities and vals argument only relevant for bins
    if (type != "bin" & "d" %in% f_vec)
        stop("the computation of densities is only relevant for bins")
    if (type != "bin" & ! is.null(vals))
        stop("the vals argument is only relevant for bins")
    # if the vals argument is provided, extract the letters and check
    # that all of them are relevant
    if (! is.null(vals)){
        vals_vec <- get_letters(vals)
        vals_na <- setdiff(vals_vec, c("a", "x", "l", "u"))
        if (length(vals_na) > 0)
            stop(paste(paste(sort(vals_na), collapse = ", "),
                       ifelse(length(na_f) == 1,
                              "is not a valid values",
                              "are not valid values")))
    }
    
    # check whether there are some weights, if so sum the weights,
    # else count the observations
    if (! freq_data){
        wgts_lgc <- deparse(substitute(weights)) != "NULL"
        if (! wgts_lgc) ct <- data %>% group_by({{ x }}) %>%
                            summarise(n = n())
        else  ct <- data %>% group_by({{ x }}) %>%
                  summarise(n = sum({{ weights }}))
        ## if (is.factor(ct %>% pull({{ x }})))
        ##     ct <- mutate(ct, "{{ x }}" := as.character({{ x }}))
        # remove the missing values
        ct <- na.omit(ct)
    }
    if (type != "bin" & freq_data) ct <- select(data, {{ x }}, n = {{ freq }})
    # bin specific stuff ; compute the center of the class in any
    # case, the lower/upper bound if required or if density has to be
    # computed
    if (type == "bin"){
        # in case of bins, add a column x which is the center of the
        # bin except in the case where a freq table is provided with
        # either a mass or a center series is provided.
        if (freq_data){
            if (! is.null(breaks)){
                data <- mutate(data, "{{ x }}" := cut({{ x }}, breaks = breaks))
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
        za <- pull(ct, {{ x }})
        if (! mass_lgc & ! center_lgc){
            ct <- add_column(ct, x = as_numeric(pull(ct, {{ x }}),
                                             pos = 0.5,
                                             xlast = xlast,
                                             xfirst = xfirst,
                                             wlast = wlast),
                             .before = 2)
        }

        if (! is.null(vals) | "d" %in% f_vec){
            if ("d" %in% f_vec | any(c("a", "l") %in% vals_vec))
                ct <- add_column(ct, l = as_numeric(pull(ct, {{ x }}),
                                                 pos = 0,
                                                 xlast = xlast,
                                                 xfirst = xfirst,
                                                 wlast = wlast))
            if ("d" %in% f_vec | any(c("a", "u") %in% vals_vec))
                ct <- add_column(ct, u = as_numeric(pull(ct, {{ x }}),
                                                 pos = 1,
                                                 xlast = xlast,
                                                 xfirst = xfirst,
                                                 wlast = wlast))
            if ("d" %in% f_vec | "a" %in% vals_vec)
                ct <- mutate(ct, a = .data$u - .data$l)
        }
        # the default ordering of the bins may not be the desired one
    }

    
    # num specific stuff: merge the upper values in the last cell if
    # max is provided
    if (! is.null(max)){
        if (type != "numeric")
            stop("the max argument is only suitable for numerical series")
        ct1 <- filter(ct, {{ x }} < max)
        ct2 <- filter(ct, {{ x }} >= max) %>%
            summarise("{{ x }}" := sum(.data$n * {{ x }}) / sum(.data$n), n = sum(.data$n))
        ct <- bind_rows(ct1, ct2)
    }
    # Computation of the statistics, no check of relevance is required
    # at this point as the potential errors were previously checked
    # compute the frequencies if required
    if (any(c("f", "F", "d") %in% f_vec)) ct <- mutate(ct, f = .data$n / sum(.data$n))
    # compute the percentages if required
    if (any(c("p", "P") %in% f_vec)) ct <- mutate(ct, p = .data$n / sum(.data$n) * 100)
    # compute the masses if required
    if (any(c("m", "M") %in% f_vec))
        ct <- mutate(ct, m = (.data$n * get_numval(ct)) / sum(.data$n * get_numval(ct)))
    # compute the densities if required
    if ("d" %in% f_vec) ct <- ct %>% mutate(d = .data$f / .data$a)
    # compute the cummulative distribution if required
    if (any(c("N", "F", "P", "M") %in% f_vec)){
        if ("N" %in% f_vec) ct <- ct %>% mutate(ct, N = cumsum(.data$n))
        if ("F" %in% f_vec) ct <- ct %>% mutate(ct, F = cumsum(.data$f))
        if ("P" %in% f_vec) ct <- ct %>% mutate(ct, P = cumsum(.data$p))
        if ("M" %in% f_vec) ct <- ct %>% mutate(ct, M = cumsum(.data$m))
    }

    # Return the required subset of statistics
    if (type == "bin"){
        f_vec <- c(f_vec, vals_vec)
        f_vec <- c("x", setdiff(f_vec, "x"))
    }
    ct <- select(ct, {{ x }}, !! f_vec)

    # coerce bin as character at this point
    if (is.factor(pull(ct, {{ x }})))
        ct <- mutate(ct, "{{ x }}" := as.character({{ x }}))
    
    
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


#' @importFrom magrittr %>%
#' @export
magrittr::`%>%` 
