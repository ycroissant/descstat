#' Functions to compute statistics on bivariate distributions
#'
#' These functions are intended to compute from a `cont_table` objects
#' covariation statistics, ie the covariance, the correlation
#' coefficient, variance decomposition and regression line.
#'
#' @name bivariate
#' @param data,object a `cont_table` object,
#' @param x the series for which the analyse of variance should be
#'     computed,
#' @param formula symbolic description of the model,
#' @param ... further arguments.
#' @return a numeric or a tibble
#' @author Yves Croissant
#' @keywords bivariate
#' @export
#' @examples
#' # the covariance and the linear correlation coefficient are
#' # computed using only the `cont_table`
#' # First reduce the number of bins
#' wages2 <- wages %>%
#'           dplyr::mutate(size = cut(as_bin(size), c(20, 50, 100)),
#'                         wage = cut(as_bin(wage), c(10, 30, 50)))
#' wages2 %>% cont_table(wage, size) %>% covariance
#' wages2 %>% cont_table(wage, size) %>% correlation
#' # For the analyse of variance, one of the two series should be
#' # indicated
#' wages2 %>% cont_table(wage, size) %>% anova(wage)
#' wages2 %>% cont_table(wage, size) %>% anova(wage) %>% summary
#' # For the regression line, a formula should be provided
#' wages2 %>% cont_table(wage, size) %>% regline(formula = wage ~ size)
covariance <- function(data, ...)
    UseMethod("covariance")

#' @rdname bivariate
#' @export
correlation <- function(data, ...)
    UseMethod("correlation")

#' @rdname bivariate
#' @importFrom rlang .data
#' @export
covariance.cont_table <- function(data, ...){
    limits <- attr(data, "limits")
    data <- total.omit(data)
    x1 <- sort(unique(data[[1]])) %>%
        as_numeric(0.5, xfirst  = limits[[1]]$xfirst,
                   xlast = limits[[1]]$xlast,
                   wlast = limits[[1]]$wlast)
    x2 <- sort(unique(data[[2]])) %>%
        as_numeric(0.5, xfirst  = limits[[1]]$xfirst,
                   xlast = limits[[1]]$xlast,
                   wlast = limits[[1]]$wlast)
    x1b <- mean(x1)
    x2b <- mean(x2)
    x12 <- outer(x1 - x1b, x2-x2b)
    data <- as_matrix(data)
    sum(x12 * data / sum(data))
}

#' @rdname bivariate
#' @export
correlation.cont_table <- function(data, ...){
    sdevs <- stdev(data)
    covar <- covariance(data)
    covar / sdevs[1, 1, drop = TRUE] / sdevs[1, 2, drop = TRUE]
}


#' @rdname bivariate
#' @export
anova.cont_table <- function(object, x, ...){
    data <- object
    x_name <- paste(substitute(x))
    if (! x_name %in% names(data)){
        if (is.numeric(x)) x_name <- names(data)[x]
    }
    cond <- setdiff(names(data)[1:2], x_name)
    cond_pos <- match(cond, names(data)[1:2])
    m_x <- data %>% conditional(x_name) %>% mean
    s2_x <- data %>% conditional(x_name) %>% variance
    f_y <- data %>% marginal(cond)
    data <- f_y %>% left_join(m_x, by = cond) %>%
        left_join(s2_x, by = cond)
    structure(data, class = c("anova.cont_table", class(data)))
}

#' @rdname bivariate
#' @export
summary.anova.cont_table <- function(object, ...){
    object %>% summarise(gmean = sum(mean * .data$f),
                         inter = sum( (mean - .data$gmean) ^ 2 * .data$f),
                         intra = sum(variance * .data$f),
                         total = .data$inter + .data$intra,
                         ratio = .data$inter / .data$total) %>%
        select(- .data$gmean)
}

#' @rdname bivariate
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

    
