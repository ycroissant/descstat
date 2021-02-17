#' Contingency table
#'
#' A contingency table returns the counts of all the combinations of
#' the modalities of two series in a table for which every modality of
#' the first series is a row and every modality of the second series
#' is a column. The `joint`, `marginal` and `conditional` functions
#' compute these three distributions from the contingency table (by
#' indicating one series for the last two).
#'
#' `cont_table` actually returns a tibble in "long format", as the
#' `dplyr::count` table does. As the returned object is of class
#' `cont_table`, this is the `format` and `print` methods that turns
#' the tibble in a wide format before printing.
#'
#' The `conditional` and `joint` functions return a `cont_table`
#' object, as the `marginal` function returns a `freq_table` object.
#' 
#' 
#' @name cont_table
#' @aliases cont_table
#' @param data a tibble,
#' @param x the series on which the operation should be computed,
#' @param x1,x2 the two series used the construct the contingency
#'     table, the distinct values of the first and the second will
#'     respectively be the rows and the columns of the contingency
#'     table,
#' @param weights a series containing the weights that should be used
#'     to mimic the population,
#' @param freq the frequencies (in the case where data is already
#'     contingency table),
#' @param total if `TRUE`, a total is added to the table,
#' @param xfirst1,xfirst2,xlast1,xlast2,wlast1,wlast2 see [as_numeric()],
#' @param f see [freq_table()],
#' @param vals see [freq_table()],
#' @return a tibble
#' @export
#' @importFrom dplyr group_by summarise mutate_if bind_cols bind_rows
#'     mutate filter ungroup select
#' @importFrom tidyr pivot_wider
#' @importFrom rlang set_names
#' @importFrom stats var
#' @importFrom dplyr left_join rename lag group_split distinct
#' @keywords manip
#' @author Yves Croissant
#' @examples
#' library("dplyr")
#' # get a contingency table containing education and sex
#' cont_table(employment, education, sex)
#' # instead of counts, sum the weights
#' cont_table(employment, education, sex, weights = weights)
#' # get the joint distribution and the conditional and marginal
#' # distribution of sex
#' cont_table(employment, education, sex) %>% joint
#' cont_table(employment, education, sex) %>% marginal(sex)
#' cont_table(employment, education, sex) %>% conditional(sex)

cont_table <- function(data, x1, x2, weights = NULL, freq = NULL,
                       total = FALSE,
                       xfirst1 = NULL, xlast1 = NULL, wlast1 = NULL,
                       xfirst2 = NULL, xlast2 = NULL, wlast2 = NULL){
    # If freq is provided, data is already a freq table
    freq_lgc <- deparse(substitute(freq)) != "NULL"
    # If x contains unique values, data should be a frequency table
    # and the freq argument is mandatory
    the_series <- select(data, {{ x1 }}, {{ x2 }})
    if (nrow(distinct(the_series)) == nrow(the_series) & ! freq_lgc)
        stop("data seems to be a frequency table and the freq argument is mandatory")
    wgts_lgc <- deparse(substitute(weights)) != "NULL"
    x1_name <- deparse(substitute(x1))
    x2_name <- deparse(substitute(x2))
    # get the type of the two series
    type_1 <- series_type(pull(data, {{ x1 }}))
    type_2 <- series_type(pull(data, {{ x2 }}))
    if (type_1 == "bin" & ! inherits(pull(data, {{ x1 }}), "bin"))
        data <- mutate(data, "{{ x1 }}" := as_bin({{ x1 }}))
    if (type_2 == "bin" & ! inherits(pull(data, {{ x2 }}), "bin"))
        data <- mutate(data, "{{ x2 }}" := as_bin({{ x2 }}))
    if (freq_lgc) ct <- data %>% select({{ x1 }}, {{ x2 }}, n = {{ freq }})
    else{
        if (! wgts_lgc) ct <- data %>% group_by({{ x1 }}, {{ x2 }}) %>%
                            summarise(n = n()) %>% ungroup
        else  ct <- data %>% group_by({{ x1 }}, {{ x2 }}) %>%
                  summarise(n = sum({{ weights }})) %>% ungroup
              #    if (na.rm) ct <- na.omit(ct)
    }
    if (total){
        mg_1 <- ct %>% group_by({{ x1 }}) %>%
            summarise(n = sum(n)) %>%
            bind_cols("Total" = 1,
                      "{{ x2 }}" := ct[1, 2, drop = TRUE])
        mg_2 <- ct %>% group_by({{ x2 }}) %>%
            summarise(n = sum(n)) %>%
            bind_cols("Total" = 2,
                      "{{ x1 }}" := ct[1, 1, drop = TRUE])
        mg_tot <- summarise(mg_1, n = sum(n)) %>%
            bind_cols("Total" = 3,
                      "{{ x2 }}" := ct[1, 2, drop = TRUE],
                      "{{ x1 }}" := ct[1, 1, drop = TRUE])
        ct <- ct %>% bind_cols(Total = 0)
        ct <- bind_rows(ct, mg_1, mg_2, mg_tot)
        ct[ct$Total == 1, 2] <- NA
        ct[ct$Total == 2, 1] <- NA
        ct[ct$Total == 3, 1:2] <- NA
        ct <- select(ct, - .data$Total)
    }
    limits = list(list(xfirst = xfirst1, xlast = xlast1, wlast = wlast1),
                  list(xfirst = xfirst2, xlast = xlast2, wlast = wlast2))
    names(limits) <- c(x1_name, x2_name)
    structure(ct,
              class = c("cont_table", class(ct)),
              total = total,
              limits = limits)
}

#' @rdname cont_table
#' @export
joint <- function(data)
    data %>% total.omit %>% mutate(n = .data$n / sum(.data$n)) %>% rename(f = n)

#' @rdname cont_table
#' @export
conditional <- function(data, x = NULL){
    limits <- attr(data, "limits")
    x_name <- deparse(substitute(x))
    if (x_name == "NULL") x_name <- NA
    if (is.na(x_name)) stop("the variable should be indicated")
    if (! x_name %in% names(data)[1:2]){
        if (is.numeric(x)){
            x <- as.integer(x)
            if (! x %in% 1L:2L) stop("x should be equal to 1 or 2")
            x_name <- names(data)[x]
        }
        if (is.character(x)){
            if (! x %in% names(data)[1:2]) stop("x don't exist")
            else x_name <- x
        }
    }
    cond_name <- setdiff(names(data)[1:2], x_name)
    data <- data %>% total.omit %>% group_by(!! as.symbol(cond_name)) %>%
        mutate(n = .data$n / sum(.data$n)) %>% ungroup %>% rename(f = .data$n)
    structure(data, class = c("cont_table", class(data)), x = x_name, limits = limits)
}

#' @rdname cont_table
#' @export
marginal <- function(data, x = NULL, f = "f", vals = NULL){
    limits <- attr(data, "limits")
    has_total <- any(is.na(pull(data, {{ x }})))
    x_name <- deparse(substitute(x))
    f_name <- names(data)[3]
    if (x_name == "NULL") x_name <- NA
    if (is.na(x_name)) stop("the variable should be indicated")
    if (! x_name %in% names(data)[1:2]){
        if (is.numeric(x)){
            x <- as.integer(x)
            if (! x %in% 1L:2L) stop("x should be equal to 1 or 2")
            x_name <- names(data)[x]
        }
        if (is.character(x)){
            if (! x %in% names(data)[1:2]) stop("x don't exist")
            else x_name <- x
        }
    }
    limits <- limits[[x_name]]
    data <- summarise(group_by(na.omit(data), !! as.symbol(x_name)),
                   f = sum(!! as.symbol(f_name))) %>%
        mutate(f = .data$f / sum(.data$f))
    data <- freq_table(data, !! as.symbol(x_name), f = f, vals = vals, freq = .data$f, 
                       xfirst = limits$xfirst,
                       xlast = limits$xlast,
                       wlast = limits$wlast, total = has_total)
    data
}

