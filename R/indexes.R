#' Composite indexes
#'
#' Computation of composite indexes (Laspeyres, Paasche and Fisher),
#' chained or not.
#' 
#' @name indices
#' @aliases indices
#' @param data a tibble
#' @param an the date of observation
#' @param bien the series containing the goods that are part of the
#'     computation of the indexes
#' @param quant the quantity
#' @param prix the price
#' @param base the base date
#' @param chaine if `TRUE`, chained indexes are computed
#' @return a tibble
#' @export
#' @importFrom dplyr group_by summarise mutate_if bind_cols bind_rows
#'     lag rename left_join
#' @importFrom tidyr pivot_wider pivot_longer separate
#' @author Yves Croissant
indices <- function(data, an, bien, quant, prix, base, chaine = FALSE){
    data <- data %>% select(an = {{ an }}, bien = {{ bien }}, quant = {{ quant }}, prix = {{ prix }})
    # data for the base year
    data_base <- data %>% filter(an == base) %>% select(- an) %>%
        rename(quant_base = quant, prix_base = prix)
    dep_tot_base <- data_base %>% summarise(depense = sum(quant_base * prix_base))
    data_base <- data_base %>% bind_cols(dep_tot_base) %>%
        mutate(cbudg_base = quant_base * prix_base / depense) %>%
        select(- depense)
    # total expense by year
    dep_tot <- data %>% group_by(an) %>% summarise(dep_tot = sum(prix * quant))
    # initial data set with budget coefficients
    data <- data %>% left_join(dep_tot) %>% mutate(cbudg = quant * prix / dep_tot) %>%
        select(- dep_tot)
    data <- data %>% left_join(data_base)
    if (! chaine){
        data_synth <- data %>% mutate(prix = prix / prix_base,
                                      quant = quant / quant_base) %>%
            select(- quant_base, - prix_base)
        data_synth <- data_synth %>% group_by(an) %>%
            summarise(laspeyres_prix = sum(prix * cbudg_base) * 100,
                      laspeyres_quant = sum(quant * cbudg_base) * 100,
                      pasche_prix = 1 / sum(1 / prix * cbudg) * 100,
                      pasche_quant = 1 / sum(1 / quant * cbudg) * 100) %>%
            mutate(fish_prix = sqrt(laspeyres_prix * pasche_prix),
                   fish_quant = sqrt(laspeyres_quant * pasche_quant))
        data_synth <- data_synth %>% pivot_longer(-an) %>%
            separate(name, into = c("indice", "grandeur")) %>%
            pivot_wider(names_from = grandeur, values_from = value)
    }
    else{
        data_synth <- data %>% group_by(bien) %>%
            mutate(prix = prix / lag(prix),
                   quant = quant / lag(quant),
                   lcbudg = lag(cbudg)) %>% group_by(an) %>%
            summarise(laspeyres_prix = sum(prix * lcbudg),
                      laspeyres_prix = ifelse(is.na(laspeyres_prix), 1, laspeyres_prix),
                      laspeyres_quant = sum(quant * lcbudg),
                      laspeyres_quant = ifelse(is.na(laspeyres_quant), 1, laspeyres_quant),
                      pasche_prix = sum(prix * cbudg),
                      pasche_prix = ifelse(is.na(pasche_prix), 1, pasche_prix),
                      pasche_quant = sum(quant * cbudg),
                      pasche_quant = ifelse(is.na(pasche_quant), 1, pasche_quant)) %>%
            mutate(laspeyres_prix = cumprod(laspeyres_prix),
                   laspeyres_quant = cumprod(laspeyres_quant),
                   pasche_prix = cumprod(pasche_prix),
                   pasche_quant = cumprod(pasche_quant))
        data_synth <- data_synth %>% pivot_longer(- an) %>%
            separate(name, into = c("indice", "grandeur")) %>%
            pivot_wider(names_from = grandeur, values_from = value)
        data_base <- filter(data_synth, an == base) %>%
            rename(prix_base = prix, quant_base = quant) %>% select(-an)
        data_synth <- data_synth %>% left_join(data_base) %>%
            mutate(prix = prix / prix_base * 100,
                   quant = quant / quant_base * 100) %>%
            select(- prix_base, - quant_base)
    }
    data_synth
}
