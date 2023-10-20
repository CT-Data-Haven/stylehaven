#' Life expectancy
#'
#' Dataset of life expectancy in years by census tract within New Haven County for 2010 to 2015.
#'
#' @format A data frame with 189 rows and 3 variables:
#' \describe{
#'   \item{tract}{Tract FIPS code}
#'   \item{town}{Town name}
#'   \item{value}{Life expectancy, including `NA` values for 9 tracts without data}
#' }
#' @source [CDC's USALEEP project](https://www.cdc.gov/nchs/nvss/usaleep/usaleep.html), current as of Oct. 2021

"life_exp"

#' 2018 DCWS sample indicators
#'
#' This is a subset of the 2018 DataHaven Community Wellbeing Survey data for Greater New Haven covering several commonly used indicators, split into separate data frames for each topic but with the same format.
#'
#' @format Data frames with 4 columns:
#' \describe{
#'   \item{question}{Abbreviated indicator}
#'   \item{category}{Category: gender, age, etc.}
#'   \item{group}{Group: male, female, ages 18–34, etc.}
#'   \item{value}{Percentage value}
#' }
#'
#' @details Detail and meaning of values for each dataset and question:
#'
#' |Dataset            |Question             |Definition / response                                                 |
#' |:------------------|:--------------------|:---------------------------------------------------------------------|
#' |chronic_disease    |hypertension         |Received hypertension diagnosis                                       |
#' |chronic_disease    |asthma               |Received asthma diagnosis                                             |
#' |chronic_disease    |diabetes             |Received diabetes diagnosis                                           |
#' |community_cohesion |positive_role_models |Strongly / somewhat agree local youth have positive role models       |
#' |community_cohesion |satisfied_with_area  |Satisfied with area                                                   |
#' |community_cohesion |trust_neighbors      |Strongly / somewhat agree neighbors can be trusted                    |
#' |fin_insecurity     |food_insecurity      |Unable to afford food in past 12 months                               |
#' |fin_insecurity     |housing_insecurity   |Unable to afford adequate housing in past 12 months                   |
#' |fin_insecurity     |no_bank_account      |Didn't have bank account in past 12 months                            |
#' |walkability        |safe_biking          |Strongly / somewhat agree there are safe places to bike nearby        |
#' |walkability        |safe_sidewalks       |Strongly / somewhat agree there are safe sidewalks & crosswalks nearby|
#'
#' @source 2018 DataHaven Community Wellbeing Survey
#' @name cws2018

NULL

#' @rdname cws2018
"chronic_disease"

#' @rdname cws2018
"community_cohesion"

#' @rdname cws2018
"fin_insecurity"

#' @rdname cws2018
"walkability"

#' 2018 DCWS sample indicators: self-rated health
#'
#' This is a subset of the 2018 DataHaven Community Wellbeing Survey data for Greater New Haven, just including the question on self-rated health. Unlike [other DCWS datasets][cws2018] in this package, this includes all response options.
#' @format A data frame with 100 rows and 4 variables:
#' \describe{
#'   \item{category}{Category: gender, age, etc.}
#'   \item{group}{Group: male, female, ages 18–34, etc.}
#'   \item{response}{Response: excellent, very good, good, fair, poor}
#'   \item{value}{Percentage value}
#' }
#' @source 2018 DataHaven Community Wellbeing Survey

"self_rated_health"

#' 2015–2018 DCWS sample trends
#'
#' This is a subset of 3 questions from both the 2015 & 2021 DataHaven Community Wellbeing Surveys for Connecticut.
#' @format A data frame with 48 rows and 5 variables:
#' \describe{
#'   \item{year}{Year, either 2015 or 2021}
#'   \item{indicator}{Indicator: satisfied with area, local government is responsive, or police approval}
#'   \item{category}{Category: gender, age, etc.}
#'   \item{group}{Group: male, female, ages 18–34, etc.}
#'   \item{value}{Percentage value}
#' }
#' @source 2015 & 2021 DataHaven Community Wellbeing Survey

"cws_trend"

#' Rent and income data
#'
#' This is data from a few tables of the 2019 American Community Survey for New Haven County, useful for comparing household incomes to local rents as a way to measure rent affordability.
#' @format A data frame with 48 rows and 6 variables:
#' \describe{
#'   \item{name}{Town name}
#'   \item{bedrooms}{Whether values correspond to all housing units, or just 2-bedroom units}
#'   \item{renters}{Number of renter households}
#'   \item{income}{Median household income of renter households}
#'   \item{annual_rent}{Annual rent, calculated as 12 * median gross rent}
#'   \item{income_needed}{Income needed for the median rent to make up no more than 30 percent of a household's income, per HUD housing affordability guidelines. Beyond this, a household is considered cost-burdened.}
#' }
#' @source US Census Bureau 2019 American Community Survey tables B25119, B25031, and B25042.

"rent_x_income"
