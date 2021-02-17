#' Put a tibble in form to plot
#'
#' Convert a tibble built using `freq_table` or `cont_table` in a
#' shape that makes it easy to plot.
#' 
#' @name pre_plot
#' @aliases pre_plot
#' @param data a tibble returned by the `freq_table` or the
#'     `cont_table` function, which should contain the center of the
#'     classes (`x`) and at least one measure of the frequencies or
#'     densities (one of `f`, `n`, `p`, `d`),
#' @param f mandatory argument if the tibble contains more than one
#'     frequency or density,
#' @param plot for object of class `freq_table` one of `histogram`,
#'     `freqpoly`, `stacked`, `cumulative` and `lorenz` (see the
#'     details section),
#' @param ... further arguments.
#' @keywords dplot
#' @details
#'
#' The `pre_plot` function returns a tibble containing:
#'  
#' - if `plot = histogram`, `x`, `y` that should be
#'     passed to `geom_polygon`,
#' - if `plot = freqpoly` `x` and `y` that should be passed to `geom_line`,
#' - if `plot = stacked` `x` and `ypos` that should be passed
#'     respectively to `geom_col` and to `geom_text` to draw labels on
#'     the right position,
#' - if `plot = cumulative` `x`, `y`, `xend` and `yend` that should be passed to
#'     `geom_segment`,
#' - if `plot = lorenz` for the Lorenz curve, `F` and `M` for the
#' coordinates of the polygons under the Lorenz curve, `pts` is
#' logical which the defines the subset of points that belongs to the
#' Lorenz curve.
#' 
#' @return a tibble
#' @importFrom dplyr desc as_tibble transmute rename
#' @importFrom purrr map_df
#' @importFrom tidyr separate pivot_wider pivot_longer
#' @export
#' @author Yves Croissant
#' @examples
#' library("dplyr")
#' library("ggplot2")
#' pad <- padova %>%
#'        freq_table(price, breaks = c(100, 200, 300, 400, 500, 1000),
#'        right = TRUE, f = "Npd")
#' pad %>% pre_plot(f = "d") %>% ggplot() + geom_polygon(aes(x, y))
#' pad %>% pre_plot(f = "d", plot = "freqpoly") %>%
#' ggplot() + geom_line(aes(x, y))
#' ## A pie chart
#' wages %>% freq_table(sector, "p", total = FALSE) %>%
#'   pre_plot("p", plot = "stacked") %>% ggplot(aes(x = 2, y = p, fill = sector)) +
#'   geom_col() + geom_text(aes(y = ypos, label = sector)) +
#'   coord_polar(theta = "y") + theme_void() + guides(fill = FALSE)
#' 
pre_plot <- function(data, f = NULL, plot = NULL, ...)
    UseMethod("pre_plot")

#' @rdname pre_plot
#' @export
pre_plot.freq_table <- function(data, f = NULL,
                                plot = c("histogram", "freqpoly", "lorenz", "stacked", "cumulative"), ...){
    plot <- match.arg(plot)
    if (plot %in% c("histogram", "freqpoly")){
        if (! "x" %in% names(data))
            stop("the table should contains the center of the classes")
        if (is.null(f)){
            fs <- c("d", "f", "p", "n")
            f <- match(names(data), fs) %>% na.omit %>% as.numeric
            if (length(f) == 0L)
                stop("nothing to plot, the tibble should contain either d, f or n")
            if (length(f) > 1L)
                stop("the variable to plot should be specified")
            data <- rename(data, f = fs[f]) %>%
                select(1, x, f)
        }
        else{
            data <- data %>% select(1, x, f = matches(paste("^[", f, "]{1}$", sep = ""),
                                                      ignore.case = FALSE))
        }        
        K <- nrow(data)
        xu <- data %>% pull(1) %>% as_numeric(1)
        xl <- data %>% pull(1) %>% as_numeric(0)
        x <- data %>% pull(x)
        xu[K] <- xl[K] + 2 * (x[K] - xl[K])
        xl[1] <- xu[1] - 2 * (xu[1] - x[1])
        if (plot == "histogram"){
            data <- data %>%
                rename(cls = 1) %>%
                select(.data$cls, f_ne = f) %>%
                mutate(x_sw = xl,
                       f_sw = 0,
                       x_nw = xl,
                       f_nw = .data$f_ne,
                       x_ne = xu,
                       f_ne = .data$f_ne,
                       x_se = xu,
                       f_se = 0) %>%
                pivot_longer( - .data$cls) %>%
                separate(.data$name, into = c("axe", "pos")) %>%
                pivot_wider(names_from = .data$axe, values_from = .data$value) %>%
                mutate(pos = factor(.data$pos, levels = c("sw", "nw", "ne", "se"))) %>%
                arrange(desc(.data$cls), .data$pos) %>%
                rename(y = f)
        }
        if (plot == "freqpoly"){
            xo <- xl[1] - (x[1] - xl[1])
            xs <- xu[K] + (xu[K] - x[K])
            data <- data %>%
                select(- 1) %>%
                add_row(x = xo, f = 0, .before = 0) %>%
                add_row(x = xs, f = 0, .after = Inf) %>%
                rename(y = f)
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
            pivot_wider(names_from = .data$axe, values_from = .data$value) %>%
            select(- .data$pos)
    }
    if (plot == "stacked"){
        if (is.null(f)) f <- names(data)[2]
        data <- data %>% select(1, all_of(f))
        z <- names(data)[1]
        data <- data %>% total.omit %>%
            mutate(ypos = cumsum(!! as.symbol(f)) - 0.5 * !! as.symbol(f)) %>% 
            map_df(rev)
        data[[1]] <- factor(data[[1]], levels = data[[1]])
    }
    if (plot == "cumulative"){
        if (! "F" %in% names(data)) stop("the frequency table should contain F")
        data <- data %>% select(x = 1, f = F) %>% 
            mutate(lf = lag(f), lx =  lag(x)) %>%
            transmute(x_hor = x, xend_hor = lag(x), y_hor = f, yend_hor = f,
                      x_vert = lag(x), xend_vert = lag(x), y_vert = f, yend_vert = lag(f)) %>%
            pivot_longer(1:8) %>%
            separate(.data$name, into = c("coord", "pos")) %>%
            bind_cols(id = rep(1:(2 * nrow(data)), each = 4)) %>%
            pivot_wider(names_from = .data$coord, values_from = .data$value) %>%
            select(- .data$id)
    }
    structure(data, class = c("freq_table", class(data)))
}


#' @rdname pre_plot
#' @export
pre_plot.cont_table <- function(data, ...){
    data <- data %>% total.omit
    lim1 <- attr(data, "limits")[[1]]
    lim2 <- attr(data, "limits")[[2]]
    data <- as_tibble(data)
    data[[1]] <- as_numeric(data[[1]], 0.5, xfirst = lim1$first, xlast = lim1$last)
    data[[2]] <- as_numeric(data[[2]], 0.5, xfirst = lim2$first, xlast = lim2$last)
    data
}
