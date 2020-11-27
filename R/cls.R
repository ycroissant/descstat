#' Convert class to values
#'
#' Convert a string (or factor) which represents a class to a value of
#' the underlying variable
#' 
#' @name cls2val
#' @aliases cls2val
#' @param x a series that contains a class of values, the first and
#'     last characters should be any of `[`, `(`, `]`, `)` and the
#'     other characters should be interpreted as two numerical values
#'     separated by a `,`
#' @param pos a numeric between 0 and 1, 0 for the lower bond, 1
#'     for the upper bond, 0.5 for the center of the class (and any
#'     other value between 0 and 1)
#' @param xfirst center of the first class, if one wants to specifie
#'     something different from the average of the lower and the upper
#'     bonds
#' @param xlast the center of the last class, if one wants to specifie
#'     something different from the average of the lower and the upper
#'     bonds
#' @param inflate in the case where the upper bond is infinite and
#'     `xlast` is not provided, the upper bond of the last class is set
#'     to the lower bond of the last class and the range of the
#'     previous class times this coefficient (which default value is
#'     one)
#' @param ... further arguments
#' @return a numerical vector
#' @export
#' @author Yves Croissant
#' @examples
#'
#' # salaire is a class of wage in the Salaires data set ; first
#' # extract unique values
#' sals <- Salaires %>% pull(salaire) %>% levels
#' # compute the lower bonds
#' sals %>% cls2val(0)
#' # lower bonds with a user specified center value for the first class
#' sals %>% cls2val(0, xfirst = 0.18) %>% head
#' # compute the upper bonds
#' sals %>% cls2val(1)
#' # note that the Inf upper bond is replaced by 50 + (50 - 40), ie
#' # the lower bond plus the range of the previous class
#' sals %>% cls2val(1, xlast = 100) %>% tail
#' # xlast is provided (the center of the last class) and the upper
#' # bond is adapted accordingly, which means 50 + (100 - 50) * 2 =
#' # 150
#' sals %>% cls2val(1, inflate = 3) %>% tail
#' # inflate is provided, so that the range of the last class is three
#' # times the range of the previous one
cls2val <- function(x, pos = 0, xfirst = NULL, xlast = NULL, inflate = NULL, ...)
    UseMethod("cls2val")


#' @rdname cls2val
#' @export
cls2val.character <- function(x, pos = 0, xfirst = NULL, xlast = NULL, inflate = NULL, ...){
    K <- length(x)
    ox <- x
    if (length(unique(x)) != K){
#        warning("no duplicated values allowed for the character method of cls2val")
        ox <- x
        x <- unique(x)
        K <- length(x)
    }
    cls <- x
    if (! is.null(xlast) & ! is.null(inflate)) stop("only one of last or inflate should be set")
    if (! is.numeric(pos)) stop("pos should be numeric")
    if (is.numeric(pos) & ! (pos >= 0 & pos <= 1)) stop("pos should be between 0 and 1")
    x <- x %>% as.character %>% strsplit(",")
    if (length(x[[1]]) == 1) stop("the series doesn't seem to be numeric")
    xl <- sapply(x, function(x) x[1])
    xl <- as.numeric(substr(xl, 2, nchar(xl)))
    xu <- sapply(x, function(x) x[2])
    xu <- as.numeric(substr(xu, 1, nchar(xu) - 1))
    ord_x <- order(xu)
    xu <- xu[ord_x]
    xl <- xl[ord_x]
    x <- x[ord_x]
    cls <- cls[ord_x]
    if (any(is.na(xl))) stop("the series doesn't seem to be numeric")
    if (! is.null(xfirst)){
        if (! (xfirst >= xl[1] & xfirst <= xu[1])) stop("irrelevant value for xfirst")
        xl[1] <- xfirst - (xu[1] - xfirst)
    }
    if (! is.null(xlast)){
        if (! (xlast >= xl[K] & xlast <= xu[K])) stop("irrelevant value for last")
        xu[K] <- xl[K] + 2 * (xlast - xl[K])
    }
    else{
        if (is.infinite(xu[K])){
            if (is.null(inflate)) inflate <- 1
            if (is.infinite(inflate)) xu[K] <- Inf
            else xu[K] <- xl[K] + inflate * (xl[K]- xl[K - 1])
        }
    }
    xnum <- (1 - pos) * xl + pos * xu
    x2 <- tibble(cls = cls, center = xnum)
    x <- tibble(cls = ox) %>% left_join(tibble(cls = cls, center = xnum), by = "cls") %>% pull(center)
    x
}

#' @rdname cls2val
#' @export
cls2val.factor <- function(x, pos = 0, xfirst = NULL, xlast = NULL, inflate = NULL, ...){
    lev_x <- levels(x)
    cls_val <- tibble(x = lev_x,
                      x_center = cls2val(x = x, pos = pos, xfirst = xfirst,
                                         xlast = xlast, inflate = inflate))
    left_join(tibble(x = as.character(x)), cls_val, by = "x") %>% pull(x_center)
}

acls2val <- function(x, pos = 0, xfirst = NULL, xlast = NULL){
    if (! is.numeric(pos)) stop("pos should be numeric")
    if (is.numeric(pos) & ! (pos >= 0 & pos <= 1)) stop("pos should be between 0 and 1")
    x <- x %>% as.character %>% strsplit(",")
    xl <- sapply(x, function(x) x[1])
    xl <- as.numeric(substr(xl, 2, nchar(xl)))
    xu <- sapply(x, function(x) x[2])
    xu <- as.numeric(substr(xu, 1, nchar(xu) - 1))
    if (! is.null(xfirst))
        if ((xfirst >= xl & xfirst <= xu))  xl <- xfirst - (xu - xfirst)
    if (! is.null(xlast)){
        if ((xlast >= xl & xlast <= xu))  xu <- xl + 2 * (xlast - xl)
        else stop("irrelevant value for xlast")
    }
    else{
        if (is.infinite(xu))
            stop("last should be set as the upper bond is infinite")
    }
    (1 - pos) * xl + pos * xu
}


#' Recode a classified variable
#'
#' `cut` take a numerical series as argument and return a class
#' according to a `break` vector ; this function recode a classified
#' series according to a vector of breaks which is a subset of the
#' original one
#'
#' @name recut
#' @aliases recut
#' @param x the variable to recode
#' @param breaks a numerical vector of breaks
#' @author Yves Croissant
#' @export
recut <- function(x, breaks = NULL){
    if (is.null(breaks)) stop("new breaks should be specified")
    if (is.numeric(x)) stop("recode is not relevant for a numeric series")
    # x breaks are provided in order to reduce the number of classes
    # first guess the value of right
    left_op <- x %>% .[2] %>% substr(1, 1)
    if (left_op == "[") right <- FALSE else right <- TRUE
    # get the initial classes and computs the breaks
    init_cls <- x %>% unique %>% sort
    lbond <- cls2val(init_cls, 0L)
    ubond <- cls2val(init_cls, 1L, inflate = Inf)
    cls_table <- tibble(x = init_cls, lbond, ubond) %>% arrange(lbond)
    init_bks <- sort(union(lbond, ubond))
    cls_table <- cls_table %>% mutate(center = cls2val(x, 0.5))
    # min/max values of the new breaks lower/larger than the
    # min/max values of the initial breaks are not allowed
    if (min(breaks) < min(init_bks)) stop("the minimal value provided is lower than the initial lower bond")
    if (max(breaks) > max(init_bks)) stop("the minimal value provided is lower than the initial lower bond")
    # min/max values of the initial breaks are included in the
    # new breaks if necessary
    if (! min(init_bks) %in% breaks) breaks <- c(breaks, min(init_bks))
    if (! max(init_bks) %in% breaks) breaks <- c(breaks, max(init_bks))
    # put in form the vector of new breaks and check whether
    # some values are not part of the initial breaks
    breaks <- sort(unique(breaks))
    dbrks <- setdiff(breaks, init_bks)
    if (length(dbrks) > 0) stop(paste(paste(sort(dbrks), collapse = ", "),
                                ifelse(length(dbrks) == 1, "is", "are"),
                                paste("provided in the breaks argument but ",
                                      ifelse(length(dbrks) == 1, "is", "are"),
                                      " not part of the  initial set of breaks", sep = "")),
                                sep = "")
    cls_table <- cls_table %>% mutate(new_cls = cut(center, breaks, right = right)) %>%
        select(x, new_cls)
    tibble(x = x) %>% left_join(cls_table, by = "x") %>% pull(new_cls)
}

#' @rdname cls2val
#' @export
cls2val.cont_table <- function(x, y = 1, pos = 0.5, ...){
    nms_x <- names(x)[[y]]
    x <- x %>% total.omit
    lim <- attr(x, "limits")[[y]]
    x_cls <- x[[y]]
    x_val <- cls2val(x[[y]], pos = pos, xfirst = lim$first, xlast = lim$last, inflate = lim$inflate)
    tibble(cls = x_cls, val = x_val) %>% unique %>% set_names(c(nms_x, paste(nms_x, "val", sep = "_")))
}
