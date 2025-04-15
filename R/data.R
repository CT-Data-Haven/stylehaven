#' Life expectancy
#'
#' Dataset of life expectancy in years by census tract within New Haven County for 2010 to 2015.
#'
#' @format A data frame with `r nrow(life_exp)` rows and `r ncol(life_exp)` variables:
#' \describe{
#'   \item{tract}{Tract FIPS code}
#'   \item{value}{Life expectancy in years}
#' }
#' @source [CDC's USALEEP project](https://www.cdc.gov/nchs/nvss/usaleep/usaleep.html), current as of Oct. 2021
#' @examples
#' head(life_exp)
#'
"life_exp"


#' 2015-2024 DCWS sample trends
#'
#' This is a subset of questions from both the 2015 & 2024 DataHaven Community Wellbeing Surveys for Greater New Haven.
#' @format A data frame with `r nrow(cws_trend)` rows and `r ncol(cws_trend)` variables:
#' \describe{
#'   \item{question}{Indicator: local government is responsive, police approval, parks in good condition}
#'   \item{year}{Year, either 2015 or 2024}
#'   \item{category}{Category: gender, age, etc.}
#'   \item{group}{Group: male, female, ages 18-34, etc.}
#'   \item{value}{Percentage value}
#' }
#' @source 2015 & 2024 DataHaven Community Wellbeing Survey
#' @examples
#' head(cws_trend)
#'
"cws_trend"


#' 2024 DCWS sample: self-rated health
#'
#' This is the question from the statewide 2024 DataHaven Community Wellbeing Survey data on self-rated health. This includes all response categories (excellent, very good, good, fair, poor) for Connecticut adults by gender.
#' @format A data frame with `r nrow(self_rated_health)` rows and `r ncol(self_rated_health)` variables:
#' \describe{
#'   \item{category}{Category: gender, age, etc.}
#'   \item{group}{Group: male, female, ages 18-34, etc.}
#'   \item{response}{Response}
#'   \item{value}{Percentage value}
#' }
#' @source 2024 DataHaven Community Wellbeing Survey
#' @examples
#' head(self_rated_health)
#'
"self_rated_health"


#' Median age by sex
#'
#' This is a sample of data from the 2023 ACS, table B01002. Values are median age for Connecticut, the Capitol Region and Greater Bridgeport counties of government (COGs), and all towns in either COG, each for total population, males, and females.
#' @format
#' A data frame with `r nrow(median_age)` rows and `r ncol(median_age)` variables:
#' \describe{
#'  \item{level}{Geographic level: factor with levels for state, county, and town}
#'  \item{county}{County: `NA` for state- and county- level observations, county name for town-level observations}
#'  \item{name}{Name of geography}
#'  \item{sex}{Sex: factor with levels total, male, and female}
#'  \item{value}{Median age}
#' }
#' @source
#' 2023 US Census Bureau American Community Survey 5-year estimates
#' @examples
#' head(median_age)
#'
"median_age"
