#' Frequency table
#'
#' A frequency table is suitable for a factor or for a discrete
#' numerical variable. It returns the modalities/values and the
#' associated frequencies
#'
#' @name freq_table
#' @aliases freq_table
#' @param data a tibble
#' @param x a factor or a discrete numerical variable
#' @param cols a string containing `n` for counts, `f` for relative
#'     frequencies and `p` for percentages : cumulative series are
#'     obtained using the same letters in upper caps
#' @param weights a series that contain the weights than enables the
#'     sample to mimic the population
#' @param na.rm with the default value `TRUE`, missing values of `x`
#'     are removed l'échantillon
#' @param total if `TRUE` (the default value), a total is added to the
#'     table
#' @param max if the series is a discrete numerical value, this
#'     argument indicates that all the values greater than `max`
#'     should be merged in the same modality
#' @param n the number of rows to print (passed to `format`)
#' @param width the width of the table to be printed (passed to
#'     `format`)
#' @param n_extra the number of extra columns described (passed to
#'     `format`)
#' @param ... other arguments (for `format`)
#' @return an object of class `freq_table` which inherits from
#'     `tbl_df`
#' @export
#' @importFrom dplyr group_by summarise mutate_if bind_cols bind_rows
#'     `%>%` n matches pull summarise_all
#' @importFrom stats na.omit
#' @importFrom rlang `:=`
#' @author Yves Croissant
#' @examples
#'
#' freq_table(employment, activity, "n")
#' freq_table(employment, activity, "nN")
#' freq_table(employment, activity, "fF", weights = weights)
#' freq_table(rgp, children, "npNP")
#' freq_table(rgp, children, "npNP", max = 5)
freq_table <- function(data, x, cols = "n", weights = NULL, na.rm = TRUE, total = TRUE, max = NA){
    # check whether there are some weights, if so sum the weights,
    # else count the observations
    wgts_lgc <- deparse(substitute(weights)) != "NULL"
    # check whether the variable is numeric
    x_is_num <- is.numeric(data %>% pull({{ x }}))
    if (! wgts_lgc) ct <- data %>% group_by({{ x }}) %>%
                        summarise(n = n())
    else  ct <- data %>% group_by({{ x }}) %>%
              summarise(n = sum({{ weights }}))
    # get the cols that should be returned
    cols <- strsplit(cols, "")[[1]]
    any_scaps <- any(c("f", "n", "p") %in% cols)
    if (! any_scaps) total <- FALSE
    # if max is filled, return a >= max category
    if (! is.na(max)){
        if (! x_is_num)
            stop("l'argument max n'a de sens que si la variable est numérique")
        ct1 <- filter(ct, {{ x }} < max)
        ct2 <- filter(ct, {{ x }} >= max) %>%
            summarise(n = sum(n), "{{ x }}" := max )
        ct <- ct1 %>% bind_rows(ct2)
    }
    # remove na values if required
    if (na.rm) ct <- na.omit(ct)
    # compute the frequencies if required
    if (any(c("f", "F") %in% cols)) ct <- ct %>% mutate(ct, f = n / sum(n))
    # compute the percentages if required
    if (any(c("p", "P") %in% cols)) ct <- ct %>% mutate(ct, p = n / sum(n) * 100)
    # compute the cummulative distribution if required
    if (any(c("N", "F", "P") %in% cols)){
        if ("N" %in% cols) ct <- ct %>% mutate(ct, N = cumsum(n))
        if ("F" %in% cols) ct <- ct %>% mutate(ct, F = cumsum(f))
        if ("P" %in% cols) ct <- ct %>% mutate(ct, P = cumsum(p))
    }
    if (total){
        lowcaps <- select(ct, matches("^[nfp]{1}$", ignore.case = FALSE))
        total_low <- lowcaps %>% summarise_all(sum) %>% mutate("{{ x }}" := ifelse(x_is_num, Inf, "Total"))
        ct <- ct %>% bind_rows(total_low)
    }
    ct <- select(ct, {{ x }}, !! cols)
    structure(ct, class = c("freq_table", class(ct)), max = max, total = total)
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
    max <- attr(x, "max")
    total <- attr(x, "total")
    nr <- nrow(x)
    if (total | ! is.na(max)) x[[1]] <- as.character(x[[1]])
    if (total) x[[1]][nr] <- "Total"
    if (! is.na(max)) x[[1]][nr - total] <- paste(">= ", max, sep ="")
    x
}


