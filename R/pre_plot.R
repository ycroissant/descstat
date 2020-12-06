#' Put a tibble in form to plot
#'
#' Convert a tibble built using `freq_table`, `bins_table` or
#' `cont_table` in a shape that make it easy to plot
#'
#' 
#' @name pre_plot
#' @aliases pre_plot
#' @param x a tibble returned by any of the `freq_table`, `bins_table`
#'     or `cont_table` function, which should contain the center of the
#'     classes (`x`) and at least one measure of the frequencies or
#'     densities (one of `f`, `n`, `p`, `d`)
#' @param y mandatory argument if the tibble contains more than one
#'     frequency or density
#' @param plot for object of class `bins_table` one of `histogram`
#'     (the default) and `freqpoly` : in the first case a tibble is
#'     returned with columns `x`, `y`, `xend`, `yend` and in the
#'     second case `x` and `y` ; for object of class `freq_table` one
#'     of `banner` (the default) and `cumulative` : in the first case
#'     a tibble is returned with columns `x`, `y` and in the second
#'     case `x`, `y`, `xend` and `yend`.
#' @param ... further arguments
#' @return a tibble
#' @importFrom dplyr desc as_tibble transmute
#' @importFrom purrr map_df
#' @export
#' @author Yves Croissant
#' @examples
#' library("ggplot2")
#' pad <- padova %>%
#'        bins_table(price, breaks = c(100, 200, 300, 400, 500, 1000),
#'        right = TRUE, cols = "Npd")
#' pad %>% pre_plot(y = "d") %>% ggplot() + geom_polygon(aes(x, y))
#' pad %>% pre_plot(y = "d", plot = "freqpoly") %>%
#' ggplot() + geom_line(aes(x, y))
#' ## A pie chart
#' wages %>% freq_table(sector, "p", total = FALSE) %>%
#'   pre_plot("p") %>% ggplot(aes(x = 2, y = p, fill = sector)) +
#'   geom_col() + geom_text(aes(y = ypos, label = round(p))) +
#'   coord_polar(theta = "y")
#' 
pre_plot <- function(x, y = NULL, plot = NULL, ...)
    UseMethod("pre_plot")

#' @rdname pre_plot
#' @export
pre_plot.bins_table <- function(x, y = NULL,
                                plot = c("histogram", "freqpoly", "lorenz"), ...){
    plot <- match.arg(plot)
    data <- x
    if (plot %in% c("histogram", "freqpoly")){
        if (! "x" %in% names(data))
            stop("the table should contains the center of the classes")
        if (is.null(y)){
            ys <- c("d", "f", "p", "n")
            cols <- match(names(data), ys) %>% na.omit %>% as.numeric
            if (length(cols) == 0L)
                stop("nothing to plot, the tibble should contain either d, f or n")
            if (length(cols) > 1L)
                stop("the variable to plot should be specified")
            data <- rename(data, y = ys[cols]) %>%
                select(1, x, y)
        }
        else{
            data <- data %>% select(1, x, y = matches(paste("^[", y, "]{1}$", sep = ""),
                                                      ignore.case = FALSE))
        }        
        K <- nrow(data)
        xu <- data %>% pull(1) %>% cls2val(1)
        xl <- data %>% pull(1) %>% cls2val(0)
        x <- data %>% pull(x)
        xu[K] <- xl[K] + 2 * (x[K] - xl[K])
        xl[1] <- xu[1] - 2 * (xu[1] - x[1])
        if (plot == "histogram"){
            data <- data %>%
                rename(cls = 1) %>%
                select(.data$cls, y_ne = y) %>%
                mutate(x_sw = xl,
                       y_sw = 0,
                       x_nw = xl,
                       y_nw = .data$y_ne,
                       x_ne = xu,
                       y_ne = .data$y_ne,
                       x_se = xu,
                       y_se = 0) %>%
                pivot_longer( - .data$cls) %>%
                separate(.data$name, into = c("axe", "pos")) %>%
                pivot_wider(names_from = .data$axe, values_from = .data$value) %>%
                mutate(pos = factor(.data$pos, levels = c("sw", "nw", "ne", "se"))) %>%
                arrange(desc(.data$cls), .data$pos)
        }
        if (plot == "freqpoly"){
            xo <- xl[1] - (x[1] - xl[1])
            xs <- xu[K] + (xu[K] - x[K])
            data <- data %>%
                select(- 1) %>%
                add_row(x = xo, y = 0, .before = 0) %>%
                add_row(x = xs, y = 0, .after = Inf)
        }
    }
    if (plot == "lorenz"){
        if (! all(c("M", "F") %in% names(data))) stop("the table should contain M and F")
        data <- data %>% add_row(F = 0, M = 0, .before = 0) %>%
            transmute(cls = !! as.symbol(names(data)[1]),
                      F_sw = lag(F), F_nw = lag(F), F_se = F, F_ne = F,
                      M_sw = 0, M_se = 0, M_ne = .data$M, M_nw = lag(.data$M)) %>%
            pivot_longer(.data$F_sw:.data$M_nw) %>%
            separate(.data$name, into = c("axe", "pos")) %>%
            mutate(pos = factor(.data$pos, levels = c("sw", "nw", "ne", "se")),
                   pts = .data$pos %in% c("nw", "ne")) %>%
            arrange(.data$cls, .data$pos) %>%
            filter(! is.na(.data$cls)) %>%
            tidyr::pivot_wider(names_from = .data$axe, values_from = .data$value)
    }
    structure(data, class = c("bins_table", class(data)))
}

#' @rdname pre_plot
#' @export
pre_plot.freq_table <- function(x, y = NULL, plot = c("banner", "cumulative"), ...){
    plot <- match.arg(plot)
    if (plot == "banner"){
        if (is.null(y)) y <- names(x)[2]
        x <- x %>% select(1, all_of(y))
        z <- names(x)[1]
        x <- x %>% total.omit %>%
            mutate(ypos = cumsum(!! as.symbol(y)) - 0.5 * !! as.symbol(y)) %>% 
            map_df(rev)
        x[[1]] <- factor(x[[1]], levels = x[[1]])
    }
    if (plot == "cumulative"){
        if (! "F" %in% names(x)) stop("the frequency table should contain F")
        x <- x %>% select(x = 1, y = F) %>% 
            mutate(ly = lag(y), lx =  lag(x)) %>%
            transmute(x_hor = x, xend_hor = lag(x), y_hor = y, yend_hor = y,
                      x_vert = lag(x), xend_vert = lag(x), y_vert = y, yend_vert = lag(y)) %>%
            pivot_longer(1:8) %>%
            separate(.data$name, into = c("coord", "pos")) %>%
            bind_cols(id = rep(1:(2 * nrow(x)), each = 4)) %>%
            pivot_wider(names_from = .data$coord, values_from = .data$value) %>%
            select(- .data$id)
    }
    x
}

#' @rdname pre_plot
#' @export
pre_plot.cont_table <- function(x, ...){
    x <- x %>% total.omit
    lim1 <- attr(x, "limits")[[1]]
    lim2 <- attr(x, "limits")[[2]]
    x <- as_tibble(x)
    x[[1]] <- cls2val(x[[1]], 0.5, xfirst = lim1$first, xlast = lim1$last)
    x[[2]] <- cls2val(x[[2]], 0.5, xfirst = lim2$first, xlast = lim2$last)
    x
}
