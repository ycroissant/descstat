#' Contingency table
#'
#' A contingency table returns the counts of all the combinations of
#' the modalities of two series in a table for which every modality of
#' the first series is a row and every modality of the second series
#' is a column. The `joint`, `marginal` and `conditional` functions
#' compute these three distributions from the contingency table (by
#' indicating one series for the last two). `mean`, `variance`,
#' `stdev`, `covariance` and `correlation` methods are
#' provided. `regline` computes the regression line and `var_decomp`
#' the variance decomposition.
#'
#' @name cont_table
#' @aliases cont_table
#' @param data a tibble,
#' @param x,object a tibble containing the contingency table
#' @param formula a formula which describe the model to estimate with
#'     the `regline` function,
#' @param y the series on which the operation should be computed
#' @param y1,y2 the two series used the construct the contingency
#'     table, the distinct values of the first and the second will
#'     respectively be the rows and the columns of the contingency
#'     table,
#' @param weights a series containing the weights that should be used
#'     to mimic the population,
#' @param freq the frequencies (in case where data is a contingency
#'     table),
#' @param total if `TRUE`, a total is added to the table,
#' @param xfirst1,xfirst2 the center of the first class for the two
#'     series,
#' @param xlast1,xlast2, the center of the last class for the two
#'     series,
#' @param wlast1,wlast2 the width of the last class for the two
#'     series,
#' @param cols see [freq_table()],
#' @param vals see [freq_table()],
#' @param ... further arguments.
#' @return a tibble
#' @export
#' @importFrom dplyr group_by summarise mutate_if bind_cols bind_rows
#'     mutate filter ungroup select
#' @importFrom tidyr pivot_wider
#' @importFrom rlang set_names
#' @importFrom stats var
#' @importFrom dplyr left_join rename lag group_split distinct
#' @author Yves Croissant
#' @examples
#' library("dplyr")
#' cont_table(employment, education, sex)
#' cont_table(employment, education, sex, weights = weights)
#' cont_table(employment, education, sex) %>% conditional(sex)
#' cont_table(wages, wage, size)
#' cont_table(wages, wage, size) %>% joint
#' cont_table(wages, wage, size) %>% joint %>% mean
#' cont_table(wages, wage, size) %>% marginal(size)
#' cont_table(wages, wage, size) %>% conditional(size)
#' 
cont_table <- function(data, y1, y2, weights = NULL, freq = NULL,
                       total = FALSE,
                       xfirst1 = NULL, xlast1 = NULL, wlast1 = NULL,
                       xfirst2 = NULL, xlast2 = NULL, wlast2 = NULL){
    # If freq is provided, data is already a freq table
    freq_lgc <- deparse(substitute(freq)) != "NULL"
    # If x contains unique values, data should be a frequency table
    # and the freq argument is mandatory
    the_series <- select(data, {{ y1 }}, {{ y2 }})
    if (nrow(distinct(the_series)) == nrow(the_series) & ! freq_lgc)
        stop("data seems to be a frequency table and the freq argument is mandatory")
    wgts_lgc <- deparse(substitute(weights)) != "NULL"
    y1_name <- deparse(substitute(y1))
    y2_name <- deparse(substitute(y2))
    # get the type of the two series
    type_1 <- series_type(pull(data, {{ y1 }}))
    type_2 <- series_type(pull(data, {{ y2 }}))
    if (type_1 == "bin" & ! inherits(pull(data, {{ y1 }}), "bin"))
        data <- mutate(data, "{{ y1 }}" := as_bin({{ y1 }}))
    if (type_2 == "bin" & ! inherits(pull(data, {{ y2 }}), "bin"))
        data <- mutate(data, "{{ y2 }}" := as_bin({{ y2 }}))
    if (freq_lgc) ct <- data %>% select({{ y1 }}, {{ y2 }}, n = {{ freq }})
    else{
        if (! wgts_lgc) ct <- data %>% group_by({{ y1 }}, {{ y2 }}) %>%
                            summarise(n = n()) %>% ungroup
        else  ct <- data %>% group_by({{ y1 }}, {{ y2 }}) %>%
                  summarise(n = sum({{ weights }})) %>% ungroup
              #    if (na.rm) ct <- na.omit(ct)
    }
    if (total){
        mg_1 <- ct %>% group_by({{ y1 }}) %>%
            summarise(n = sum(n)) %>%
            bind_cols("Total" = 1,
                      "{{ y2 }}" := ct[1, 2, drop = TRUE])
        mg_2 <- ct %>% group_by({{ y2 }}) %>%
            summarise(n = sum(n)) %>%
            bind_cols("Total" = 2,
                      "{{ y1 }}" := ct[1, 1, drop = TRUE])
        mg_tot <- summarise(mg_1, n = sum(n)) %>%
            bind_cols("Total" = 3,
                      "{{ y2 }}" := ct[1, 2, drop = TRUE],
                      "{{ y1 }}" := ct[1, 1, drop = TRUE])
        ct <- ct %>% bind_cols(Total = 0)
        ct <- bind_rows(ct, mg_1, mg_2, mg_tot)
        ct[ct$Total == 1, 2] <- NA
        ct[ct$Total == 2, 1] <- NA
        ct[ct$Total == 3, 1:2] <- NA
        ct <- select(ct, - .data$Total)
    }
    limits = list(list(xfirst = xfirst1, xlast = xlast1, wlast = wlast1),
                  list(xfirst = xfirst2, xlast = xlast2, wlast = wlast2))
    names(limits) <- c(y1_name, y2_name)
    structure(ct,
              class = c("cont_table", class(ct)),
              total = total,
              limits = limits)
}

#' @rdname cont_table
#' @export
joint <- function(x)
    x %>% total.omit %>% mutate(n = .data$n / sum(.data$n)) %>% rename(f = n)

#' @rdname cont_table
#' @export
conditional <- function(x, y = NULL){
    limits <- attr(x, "limits")
    y_name <- deparse(substitute(y))
    if (y_name == "NULL") y_name <- NA
    if (is.na(y_name)) stop("the variable should be indicated")
    if (! y_name %in% names(x)[1:2]){
        if (is.numeric(y)){
            y <- as.integer(y)
            if (! y %in% 1L:2L) stop("y should be equal to 1 or 2")
            y_name <- names(x)[y]
        }
        if (is.character(y)){
            if (! y %in% names(x)[1:2]) stop("y don't exist")
            else y_name <- y
        }
    }
    cond_name <- setdiff(names(x)[1:2], y_name)
    x <- x %>% total.omit %>% group_by(!! as.symbol(cond_name)) %>%
        mutate(n = .data$n / sum(.data$n)) %>% ungroup %>% rename(f = .data$n)
    structure(x, class = c("cont_table", class(x)), y = y_name, limits = limits)
}

#' @rdname cont_table
#' @export
marginal <- function(x, y = NULL, cols = "f", vals = NULL){
    limits <- attr(x, "limits")
    has_total <- any(is.na(pull(x, {{ y }})))
    y_name <- deparse(substitute(y))
    f_name <- names(x)[3]
    if (y_name == "NULL") y_name <- NA
    if (is.na(y_name)) stop("the variable should be indicated")
    if (! y_name %in% names(x)[1:2]){
        if (is.numeric(y)){
            y <- as.integer(y)
            if (! y %in% 1L:2L) stop("y should be equal to 1 or 2")
            y_name <- names(x)[y]
        }
        if (is.character(y)){
            if (! y %in% names(x)[1:2]) stop("y don't exist")
            else y_name <- y
        }
    }
    limits <- limits[[y_name]]
    x <- summarise(group_by(na.omit(x), !! as.symbol(y_name)),
                   f = sum(!! as.symbol(f_name))) %>%
        mutate(f = .data$f / sum(.data$f))
    x <- freq_table(x, !! as.symbol(y_name), cols = cols, vals = vals, freq = .data$f, 
                    xfirst = limits$xfirst,
                    xlast = limits$xlast,
                    wlast = limits$wlast, total = has_total)
    x
}

#' @rdname cont_table
#' @export
var_decomp <- function(x, y){
    y_name <- paste(substitute(y))
    print(y_name)
    if (! y_name %in% names(x)){
        if (is.numeric(y)) y_name <- names(x)[y]
    }
    cond <- setdiff(names(x)[1:2], y_name)
    cond_pos <- match(cond, names(x)[1:2])
    m_x <- x %>% conditional(y_name) %>% mean
    s2_x <- x %>% conditional(y_name) %>% variance
    f_y <- x %>% marginal(cond)
    x <- f_y %>% left_join(m_x, by = cond) %>%
        left_join(s2_x, by = cond)
    structure(x, class = c("var_decomp", class(x)))
}

#' @rdname cont_table
#' @export
summary.var_decomp <- function(object, ...){
    object %>% summarise(gmean = sum(mean * .data$f),
                         inter = sum( (mean - .data$gmean) ^ 2 * .data$f),
                         intra = sum(variance * .data$f),
                         total = .data$inter + .data$intra,
                         ratio = .data$inter / .data$total) %>%
        select(- .data$gmean)
}

#' @rdname cont_table
#' @export
regline <- function(formula, data){
    if (! inherits(data, "cont_table")) stop("regline only suitable for cont_table data")
    formula <- as.list(formula)
    y <- paste(deparse(formula[[2]]))
    x <- paste(deparse(formula[[3]]))
    if (! y %in% names(data)[1:2]) stop(paste(y, "doesn't exist"))
    if (! x %in% names(data)[1:2]) stop(paste(x, "doesn't exist"))
    c_xy <- data %>% joint %>% covariance
    m_x <- data %>% marginal(x) %>% mean
    m_y <- data %>% marginal(y) %>% mean
    v_x <- data %>% marginal(x) %>% mean
    slope <- c_xy / v_x
    intercept <- m_y - slope * m_x
    c(intercept, slope)
}

    
