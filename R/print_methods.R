#' Print methods for bin, freq_table and cont_table objects
#'
#' `freq_table` and `cont_table` are tibbles with specific format and
#' print methods for pretty printing. A `pre_print` generic is
#' provided with specific methods to put in form `freq_table` and
#' `cont_table` objects.
#'
#' @name print_method
#' @param x a `bin`, a `freq_table` or a `cont_table` object,
#' @param n,width,n_extra see [tibble::formatting] and
#'     [tibble::formatting].
#' @param row_name a logical that indicates whether the first column
#'     in the two-ways contingency table, that contains the levels of
#'     the first series, should be named,
#' @param total_name the name of the line (and of the column for
#'     `cont_table`) that contains the total (default is `"Total"`),
#' @param ... further arguments,
#' @return a tibble, for the `cont_table` it is a tibble in wide
#'     format as the `cont_table` object is in long format.
#' @rdname print_methods
#' @keywords print
#' @export
pre_print <- function(x, ...)
    UseMethod("pre_print")

#' @rdname print_methods
#' @export
pre_print.freq_table <- function(x, ...){
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

#' @rdname print_methods
#' @export
pre_print.cont_table <- function(x, ..., row_name = TRUE, total_name = "Total"){
    if (length(x) == 3){
        if (row_name) names_x <- paste(names(x)[1], "|", names(x)[2], sep = "")
        else names_x <- " "
        names(x)[1] <- names_x
        # the ordering of the columns may be problematic if x[[2]] is
        # a factor, so get the levels
        if (is.factor(x[[2]])) levs <- levels(x[[2]]) else levs <- NULL
        x <- x %>% pivot_wider(names_from = 2, values_from = 3)
        if (! is.null(levs)){
            has_total <- any(names(x) == "NA")
            new_names <- c(names_x, levs)
            if (has_total) new_names <- c(new_names, "NA")
            x <- x[, new_names]
        }
        if ("NA" %in% names(x)) names(x)[names(x) == "NA"] <- total_name
        if (any(is.na(x[[1]])))
            x[[1]] <- ifelse(is.na(x[[1]]), total_name, x[[1]])
    }
    x
}

#' @rdname print_methods
#' @export
format.freq_table <- function(x, ..., n = NULL, width = NULL, n_extra = NULL){
    x <- pre_print(x)
    class(x) <- setdiff(class(x), "freq_table")
    format(x, ..., n = n, width = width, n_extra = n_extra)
}    

#' @rdname print_methods
#' @export
format.cont_table <- function(x, ..., n = NULL, width = NULL, n_extra = NULL, row_name = TRUE, total_name = "Total"){
    x <- pre_print(x, row_name = row_name, total_name = total_name)
    class(x) <- setdiff(class(x), "cont_table")
    format(x, ..., n = n, width = width, n_extra = n_extra)
}    

#' @rdname print_methods
#' @importFrom cli cat_line
#' @export
print.cont_table <- function(x, ..., n = NULL, width = NULL, n_extra = NULL, row_name = TRUE, total_name = "Total"){
    cli::cat_line(format(x, ..., n = n, width = width, n_extra = n_extra, row_name = row_name, total_name = total_name))
    invisible(x)
}

#' @rdname print_methods
#' @export
print.bin <- function(x, ...){
    class(x) <- setdiff(class(x), "bin")
    print(x, ...)
}
