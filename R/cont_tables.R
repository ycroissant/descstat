#' Contingency table
#'
#' A contingency table returns the counts of all the combinations of
#' the modalities of two factors in a table for which every modality
#' of the first factor is a row and every modality of the second
#' factor is a column. The `joint`, `marginal` and `conditional`
#' functions compute these three distribution from the contingency
#' table (by indicating on series for the last two). `mean`,
#' `variance`, `stdev`, `covariance` and `correlation` methods are
#' provided. `regline` computes the regression line and `var_decomp`
#' the variance decomposition.
#'
#' @name cont_table
#' @aliases cont_table
#' @param data a tibble
#' @param x,object a tibble containing the contingency table
#' @param formula a formula which describe the model to estimate with
#'     the `regline` function
#' @param y the series on which the operation should be computed
#' @param y1 a first factor
#' @param y2 a second factor
#' @param pond a series containing the weights that should be used to
#'     mimic the population
#' @param total if `TRUE` (the defaut values), a total is added to the
#'     table
#' @param first1 the center of the first class for the first variable
#' @param last1 the center of the last class for the first variable
#' @param inflate1 the width of the last class for the first variable
#' @param first2 the center of the first class for the second variable
#' @param last2 the center of the last class for the second variable
#' @param inflate2 the width of the last class for the second variable
#' @param drop if `TRUE`, the default, a numeric is returned,
#'     otherwise the result is a tibble
#' @param ... further arguments
#' @param n the number of lines to print
#' @param width the width of the table to print
#' @param n_extra extra n lines
#' @return a tibble
#' @export
#' @importFrom dplyr group_by summarise mutate_if bind_cols bind_rows
#'     mutate filter ungroup select
#' @importFrom tidyr pivot_wider
#' @importFrom rlang set_names
#' @importFrom stats var
#' @author Yves Croissant
#' @examples
#'
#' cont_table(Emploi, diplome, sexe)
#' cont_table(Emploi, diplome, sexe, pond = ponderations)
#' cont_table(Emploi, diplome, sexe) %>% conditional(sexe)
#' cont_table(Salaires, salaire, taille)
#' cont_table(Salaires, salaire, taille) %>% joint
#' cont_table(Salaires, salaire, taille) %>% joint %>% mean
#' cont_table(Salaires, salaire, taille) %>% marginal(taille)
#' cont_table(Salaires, salaire, taille) %>% conditional(taille) %>% mean
#' 
cont_table <- function(data, y1, y2, pond = NULL,
                       total = TRUE,
                       first1 = NULL, last1 = NULL, inflate1 = NULL,
                       first2 = NULL, last2 = NULL, inflate2 = NULL){
    pond_lgc <- deparse(substitute(pond)) != "NULL"
    y1_name <- deparse(substitute(y1))
    y2_name <- deparse(substitute(y2))
    if (! pond_lgc) ct <- data %>% group_by({{ y1 }}, {{ y2 }}) %>%
                        summarise(eff = n()) %>% ungroup
    else  ct <- data %>% group_by({{ y1 }}, {{ y2 }}) %>%
              summarise(eff = sum({{ pond }})) %>% ungroup
#    if (na.rm) ct <- na.omit(ct)
    ct <- ct %>% mutate_if(is.factor, as.character)
    if (total){
        mg_1 <- ct %>% group_by({{ y1 }}) %>%
            summarise(eff = sum(eff)) %>%
            bind_cols("{{ y2 }}" := NA)
        mg_2 <- ct %>% group_by({{ y2 }}) %>%
            summarise(eff = sum(eff)) %>%
            bind_cols("{{ y1 }}" := NA)
        mg_tot <- summarise(mg_1, eff = sum(eff)) %>%
            bind_cols("{{ y2 }}" := NA,
                      "{{ y1 }}" := NA)
        ct <- bind_rows(ct, mg_1, mg_2, mg_tot)
    }
    limits = list(list(first = first1, last = last1, inflate = inflate1),
                  list(first = first2, last = last2, inflate = inflate2))
    names(limits) <- c(y1_name, y2_name)
    structure(ct,
              class = c("cont_table", class(ct)),
              total = total,
              limits = limits)
}



fun.cont_table <- function(x, fun = weighted.mean, drop = TRUE, ...){
    if (! is.null(attr(x, "y"))){
        y_name <- attr(x, "y")
        y <- x %>% .[[y_name]] %>% unique %>% setdiff("Total")
        limits <- attr(x, "limits")[[y_name]]
        y_ctr <- cls2val(y, 0.5,
                         xfirst = limits$first,
                         xlast = limits$last,
                         inflate = limits$inflate)
        names(y_ctr) <- y
    }
    else y_name <- NULL
    if (length(x) == 2){
        # marginal distribution
        x[[y_name]] <- y_ctr[x[[y_name]]]
        x <- x %>% summarise(stat = fun(!! as.symbol(y_name), w = f, ...)) %>%
            set_names(y_name)
    }
    else{
        if (! is.null(y_name)){
            # conditional distribution
            cond_name <- setdiff(names(x)[1:2], y_name)
            x[[y_name]] <- y_ctr[x[[y_name]]]
            # put the levels of the conditional variable in the right order
            y <- x %>% pull(cond_name) %>% unique %>% tibble %>% set_names(cond_name)
            x <- x %>% group_by( !! as.symbol(cond_name)) %>%
                summarise(stat = fun(!! as.symbol(y_name), w = f, ...)) %>%
                set_names(c(cond_name, y_name))
            x <- y %>% left_join(x, by = cond_name)
        }
        else{
            # joint distribution
            limits <- attr(x, "limits")
            y1 <- x %>% pull(1) %>% unique %>% setdiff("Total")
            y1_ctr <- cls2val(y1, 0.5,
                              xfirst = limits[[1]]$first,
                              xlast = limits[[1]]$last,
                              inflate = limits[[1]]$inflate)
            names(y1_ctr) <- y1
            x[[1]] <- y1_ctr[x[[1]]]
            y2 <- x %>% pull(2) %>% unique %>% setdiff("Total")
            y2_ctr <- cls2val(y2, 0.5,
                              xfirst = limits[[2]]$first,
                              xlast = limits[[2]]$last,
                              inflate = limits[[2]]$inflate)
            names(y2_ctr) <- y2
            x[[2]] <- y2_ctr[x[[2]]]
            x <- x %>% summarise(stat1 = fun(!! as.symbol(names(x)[1]), w = f, ...),
                                 stat2 = fun(!! as.symbol(names(x)[2]), w = f, ...)) %>%
                set_names(c(names(x)[1], names(x)[2]))
        }
    }
    if (ncol(x) == 1) x <- x %>% pull
    x
}

#' @rdname cont_table
#' @export
mean.cont_table <- function(x, ..., drop = TRUE)
    fun.cont_table(x, fun = weighted.mean, drop = drop, ...)

#' @rdname cont_table
#' @export
variance.cont_table <- function(x, ..., drop = TRUE)
    fun.cont_table(x, fun = variance, drop = drop, ...)

#' @rdname cont_table
#' @export
stdev.cont_table <- function(x, ..., drop = TRUE)
    fun.cont_table(x, fun = stdev, drop = drop, ...)

#' @rdname cont_table
#' @export
covariance.cont_table <- function(x, drop = TRUE, ...){
    x <- total.omit(x)
    limits <- attr(x, "limits")
    means <- x %>% mean
    vals_1 <- tibble(unique(x[[1]])) %>% set_names(names(x)[1])
    vals_1$val1 <- cls2val(vals_1[[1]], 0.5,
                           xfirst  = limits[[1]]$first,
                           xlast = limits[[1]]$last)
    vals_2 <- tibble(unique(x[[2]])) %>% set_names(names(x)[2])
    vals_2$val2 <- cls2val(vals_2[[1]], 0.5,
                           xfirst  = limits[[2]]$first,
                           xlast = limits[[2]]$last)
    x <- x %>%
        left_join(vals_1, by = names(vals_1)[1]) %>%
        left_join(vals_2, by = names(vals_2)[1])
    x <- x %>% summarise(covariance = sum(f * (val1 - means[1]) * (val2 - means[2])))
    if (drop) x <- x %>% pull
    x
}

#' @rdname cont_table
#' @export
correlation.cont_table <- function(x, drop = TRUE, ...){
    sdevs <- stdev(x)
    covar <- covariance(x, drop = drop)
    covar / sdevs[1, 1, drop = TRUE] / sdevs[1, 2, drop = TRUE]
}


#total.omit <- function(x) x[ x[[1]] != "Total" & x[[2]] != "Total", ]
total.omit <- function(x) x[ ! (is.na(x[[1]]) | is.na(x[[2]])), ]

#' @rdname cont_table
#' @export
joint <- function(x)
    x %>% total.omit %>% mutate(eff = eff / sum(eff)) %>% rename(f = eff)

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
        mutate(eff = eff / sum(eff)) %>% ungroup %>% rename(f = eff)
    structure(x, class = c("cont_table", class(x)), y = y_name, limits = limits)
}

#' @rdname cont_table
#' @export
marginal <- function(x, y = NULL){
    limits <- attr(x, "limits")
    N <- x %>% total.omit %>% summarise(N = sum(eff)) %>% pull(N)
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
    # put the classes in the right order
    y_ord <- tibble(unique(na.omit(x[[y_name]]))) %>% set_names(y_name)
    x <- x %>% total.omit %>% group_by(!! as.symbol(y_name)) %>%
        summarise(f = sum(eff)) %>% mutate(f = f / sum(f))
    x <- y_ord %>% left_join(x, by = y_name)
    structure(x, class = c("cont_table", class(x)), y = y_name, limits = limits)
}


#' @rdname cont_table
#' @export
var_decomp <- function(x, y){
    if (is.numeric(y)) y <- names(x)[y]
    cond <- setdiff(names(x)[1:2], y)
    cond_pos <- match(cond, names(x)[1:2])
    var_name <- y
    m_x <- x %>% conditional(y) %>% mean
    s2_x <- x %>% conditional(y) %>% variance
    f_y <- x %>% marginal(cond)
    val_y <- x %>% cls2val(cond_pos)
    names(m_x)[2] <- "mean"#paste("mean", var_name, sep = "_")
    names(s2_x)[2] <- "var"#paste("var", var_name, sep = "_")
    x <- val_y %>% left_join(f_y, by = cond) %>% left_join(m_x, by = cond) %>% left_join(s2_x, by = cond)
    structure(x, class = c("var_decomp", class(x)))
}

#' @rdname cont_table
#' @export
summary.var_decomp <- function(object, ...){
    object %>% summarise(gmean = sum(mean * f),
                         inter = sum( (mean - gmean) ^ 2 * f),
                         intra = sum(var * f),
                         total = inter + intra,
                         ratio = inter / total) %>%
        select(- gmean)
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

#' @rdname cont_table
#' @export
pre_print.cont_table <- function(x){
    if (length(x) == 3){
        x <- x %>% pivot_wider(names_from = 2, values_from = 3)
        if ("NA" %in% names(x)) x <- x %>% rename(Total = `NA`)
        if (any(is.na(x[[1]])))
            x[[1]] <- ifelse(is.na(x[[1]]), "Total", x[[1]])
    }
    x
}

#' @rdname cont_table
#' @export
format.cont_table <- function(x, ..., n = NULL, width = NULL, n_extra = NULL){
    x <- pre_print(x)
    class(x) <- setdiff(class(x), "cont_table")
    format(x, ..., n = n, width = width, n_extra = n_extra)
}    
