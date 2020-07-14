#' Belgian motor third party liability insurance portfolio
#'
#' Dataset containing the number of motor insurance claims and several
#' characteristics for a portfolio of around 163000 policyholders.
#'
#' @format A data frame with 163210 rows and 13 variables: \describe{
#'   \item{id}{policyholder id}
#'   \item{nclaims}{number of claims}
#'   \item{expo}{exposure period, as a fraction of a year}
#'   \item{coverage}{coverage type: TPL, TPL+ or TPL++}
#'   \item{sex}{gender: female or male}
#'   \item{fuel}{fuel type: diesel or gasoline}
#'   \item{use}{use type: private or work}
#'   \item{fleet}{fleet vehicle: 0 or 1}
#'   \item{ageph}{age of the policyholder, in years}
#'   \item{bm}{bonus-malus level, higher is worse}
#'   \item{power}{horsepower of the vehicle, in kilowatt}
#'   \item{agec}{age of the vehicle, in years}
#'   \item{postcode}{first two digits of the postal code of
#'   the municipality of residence}
#' }
'mtpl_be'

#' French motor third party liability insurance portfolio
#'
#' Dataset containing the number of motor insurance claims and several
#' characteristics for a portfolio of around 669000 policyholders.
#'
#' @format A data frame with 163212 rows and 14 variables: \describe{
#'   \item{id}{policyholder id}
#'   \item{nclaims}{number of claims}
#'   \item{expo}{exposure period, as a fraction of a year}
#'   \item{fuel}{fuel type: diesel or gasoline}
#'   \item{brand}{brand of the vehicle, 11 levels}
#'   \item{region}{region of residence, 22 levels}
#'   \item{power}{horsepower of the vehicle, 12 ordered levels}
#'   \item{ageph}{age of the policyholder, in years}
#'   \item{bm}{bonus-malus level, higher is worse}
#'   \item{agec}{age of the vehicle, in years}
#'   \item{popdens}{population density of the municipality of
#'   residence, in persons per squared kilometer}
#' }
'mtpl_fr'
