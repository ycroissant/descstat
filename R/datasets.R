#' French employment survey
#'
#' The employment survey gives information about characteristics of a
#' sample of individuals (employed/unemployed, part/full time job,
#' education, etc.).
#' 
#' @name employment
#' @docType data
#' @keywords datasets
#' 
#' @format a tibble containing
#' - activity : a factor with levels `occupied`, `unemployed` and
#' `inactive`,
#' - time : job time a factor with levels `part`, `full` and
#' `unknown`,
#' - education : level of education,
#' - age : age in years,
#' - sex : one of `male` or `female`,
#' - household : kind of household, `single`, `monop` (mono-parental
#' family), `couple` (couple without children), `family` (couple with
#' families) and `other`,
#' - weights : weights to mimic the population.
#' 
#' @source Employment survey 2018,
#'     [INSEE](https://www.insee.fr/fr/statistiques/4191029)'s
#'     website.
#'
NULL


#' DADS survey
#'
#' The DADS survey (Declaration Annuelle des Données Sociales)
#' provides characteristics of wage earners (wages in class, number
#' of working hours, etc.).
#' 
#' @name wages
#' @docType data
#' @keywords datasets
#' 
#' @format a tibble containing
#' - sector : activity sector, `industry`, `building`, `business`,
#' `services` and `administration`,
#' - age :  the age in years,
#' - hours : annual number of hours worked,
#' - sex : sex of the wage earner, `male` or `female`,
#' - wage : class of yearly wages, in thousands of euros,
#' - size : class of working force size of the firm.
#'
#' @source DADS survey 2015,
#'     [INSEE](https://www.insee.fr/fr/statistiques/3536754)'s
#'     website.
#'
NULL

#' Housing prices in Padova
#'
#'
#' This data set documents characteristics (including the prices) of a
#' sample of housings in Padova.
#' 
#' @name padova
#' @docType data
#' @keywords datasets
#' 
#' @format a tibble containing
#' - zone : one of the 12 zones of Padova,
#' - condition : `new` for new housings, `ordinary` or `good` for old ones,
#' - house : dummy for houses,
#' - floor : floor,
#' - rooms : number of rooms,
#' - bathrooms : number of bathrooms,
#' - parking : dummy for parkings,
#' - energy : energy cathegory for the house (A for the best, G for the worst),
#' - area : area of the house in square meters,
#' - price : price of the house in thousands of euros.
#'
#' @source
#' [Data in Brief](https://www.sciencedirect.com/science/article/pii/S2352340915003224)'s website.
#' @references
#'  Bonifaci P, Copiello S
#'  (2015). "Real estate market and building energy performance: Data for a mass appraisal approach."
#'  _Data in Brief_, *5*, 1060-1065. ISSN 2352-3409.
#' 
NULL

#' Extract of the French census
#'
#' This extract of the French census gives information about a sample
#' of French households.
#' 
#' @name rgp
#' @docType data
#' @keywords datasets
#' 
#' @format a tibble containing : 
#' - cars : number of cars,
#' - rooms : number of rooms of the housing,
#' - children : number of children,
#' - type : type of household ; `couple` or `monop` (for mono-parental families),
#' 
#' @source [INSEE](https://www.insee.fr/fr/statistiques/4507685?sommaire=4508161)'s
#'     website.
#'
NULL



#' Income of French households
#'
#' Bins of income classes, number of households and mass of income.
#' 
#' @name income
#' @docType data
#' @keywords datasets
#' 
#' @format a tibble containing : 
#' - bin: bin of income,
#' - number: number of households in the bin,
#' - income: mass of income in the bin.
#' 
#' @source Impot sur le revenu par commune (IRCOM)
#'     [DGI](https://www.impots.gouv.fr/portail/statistiques)'s
#'     website.
#'
NULL

