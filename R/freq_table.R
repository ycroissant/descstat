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
freq_table <- function(data, x, cols = "n", weights = NULL, na.rm = FALSE, total = FALSE, max = NA){
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
            stop("the max argument is only suitable for numerical series")
        ct1 <- filter(ct, {{ x }} < max)
        ct2 <- filter(ct, {{ x }} >= max) %>%
            summarise(n = sum(n), "{{ x }}" := max + 0.5 )
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
        if ("N" %in% cols) ct <- ct %>% mutate(ct, N = cumsum(.data$n))
        if ("F" %in% cols) ct <- ct %>% mutate(ct, F = cumsum(.data$f))
        if ("P" %in% cols) ct <- ct %>% mutate(ct, P = cumsum(.data$p))
    }
    if (total){
        lowcaps <- select(ct, matches("^[nfp]{1}$", ignore.case = FALSE))
        total_low <- lowcaps %>% summarise_all(sum) %>%
            mutate("{{ x }}" :=  NA)#ifelse(x_is_num, NA, "Total"))
        ct <- ct %>% bind_rows(total_low)
    }
    ct <- select(ct, {{ x }}, !! cols)
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
        dec_part <- x[[1]] %% floor(x[[1]])
        max_pos <- which(dec_part == 0.5)
        max_val <- x[[1]][max_pos]
    }
    a_NA <- which(is.na(x[[1]]))
    if (length(a_NA)) x[[1]][a_NA] <- "Total"
    if (is_numeric_series){
        if (length(max_pos)){
            x[[1]][max_pos] <- paste(">=", floor(max_val), sep = "")
            x[[1]] <- factor(x[[1]], levels = x[[1]])
        }
    }
    x
}


