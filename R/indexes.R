#' Composite indexes
#'
#' Computation of composite indexes (Laspeyres, Paasche and Fisher),
#' chained or not.
#' 
#' @name indexes
#' @aliases indexes
#' @param data a tibble
#' @param an the date of observation
#' @param bien the series containing the goods that are part of the
#'     computation of the indexes
#' @param quant the quantity
#' @param prix the price
#' @param base the base date
#' @param chained if `TRUE`, chained indexes are computed
#' @return a tibble
#' @export
#' @importFrom dplyr group_by summarise mutate_if bind_cols bind_rows
#'     lag rename left_join
#' @importFrom tidyr pivot_wider pivot_longer separate
#' @author Yves Croissant
indexes <- function(data, an, bien, quant, prix, base, chained = FALSE){
    data <- data %>% select(an = {{ an }}, bien = {{ bien }}, quant = {{ quant }}, prix = {{ prix }})
    # data for the base year
    data_base <- data %>% filter(an == base) %>% select(- an) %>%
        rename(quant_base = quant, prix_base = prix)
    dep_tot_base <- data_base %>% summarise(depense = sum(.data$quant_base * .data$prix_base))
    data_base <- data_base %>% bind_cols(dep_tot_base) %>%
        mutate(cbudg_base = .data$quant_base * .data$prix_base / .data$depense) %>%
        select(- .data$depense)
    # total expense by year
    dep_tot <- data %>% group_by(an) %>% summarise(dep_tot = sum(prix * quant))
    # initial data set with budget coefficients
    data <- data %>% left_join(dep_tot) %>% mutate(cbudg = quant * prix / dep_tot) %>%
        select(- dep_tot)
    data <- data %>% left_join(data_base)
    if (! chained){
        data_synth <- data %>% mutate(prix = prix / .data$prix_base,
                                      quant = quant / .data$quant_base) %>%
            select(- .data$quant_base, - .data$prix_base)
        data_synth <- data_synth %>% group_by(an) %>%
            summarise(laspeyres_prix = sum(prix * .data$cbudg_base) * 100,
                      laspeyres_quant = sum(quant * .data$cbudg_base) * 100,
                      pasche_prix = 1 / sum(1 / prix * .data$cbudg) * 100,
                      pasche_quant = 1 / sum(1 / quant * .data$cbudg) * 100) %>%
            mutate(fish_prix = sqrt(.data$laspeyres_prix * .data$pasche_prix),
                   fish_quant = sqrt(.data$laspeyres_quant * .data$pasche_quant))
        data_synth <- data_synth %>% pivot_longer(-an) %>%
            separate(.data$name, into = c("indice", "grandeur")) %>%
            pivot_wider(names_from = .data$grandeur, values_from = .data$value)
    }
    else{
        data_synth <- data %>% group_by(bien) %>%
            mutate(prix = prix / lag(prix),
                   quant = quant / lag(quant),
                   lcbudg = lag(.data$cbudg)) %>% group_by(an) %>%
            summarise(laspeyres_prix = sum(prix * .data$lcbudg),
                      laspeyres_prix = ifelse(is.na(.data$laspeyres_prix), 1, .data$laspeyres_prix),
                      laspeyres_quant = sum(quant * .data$lcbudg),
                      laspeyres_quant = ifelse(is.na(.data$laspeyres_quant), 1, .data$laspeyres_quant),
                      pasche_prix = sum(prix * .data$cbudg),
                      pasche_prix = ifelse(is.na(.data$pasche_prix), 1, .data$pasche_prix),
                      pasche_quant = sum(quant * .data$cbudg),
                      pasche_quant = ifelse(is.na(.data$pasche_quant), 1, .data$pasche_quant)) %>%
            mutate(laspeyres_prix = cumprod(.data$laspeyres_prix),
                   laspeyres_quant = cumprod(.data$laspeyres_quant),
                   pasche_prix = cumprod(.data$pasche_prix),
                   pasche_quant = cumprod(.data$pasche_quant))
        data_synth <- data_synth %>% pivot_longer(- an) %>%
            separate(.data$name, into = c("indice", "grandeur")) %>%
            pivot_wider(names_from = .data$grandeur, values_from = .data$value)
        data_base <- filter(data_synth, an == base) %>%
            rename(prix_base = prix, quant_base = quant) %>% select(-an)
        data_synth <- data_synth %>% left_join(data_base) %>%
            mutate(prix = prix / .data$prix_base * 100,
                   quant = quant / .data$quant_base * 100) %>%
            select(- .data$prix_base, - .data$quant_base)
    }
    data_synth
}
