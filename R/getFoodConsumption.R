##' Get Food Consumption
##'
##' This function pulls the food consumption data from the SWS
##'
##' @param yearRange Character vector with the range of the years.
##'
##' @return The dataset with the food consumption.
##'
##' @export
##'

getFoodConsumption <- function(yearRange) {

  foodKey = DatasetKey(
    domain = "agriculture",
    dataset = "aproduction",
    dimensions = list(
      Dimension(name = "geographicAreaM49",
                keys = getAllCountries()),
      Dimension(name = "measuredElement",
                keys = "5141"),
      Dimension(name = "measuredItemCPC",
                keys = getAllItemCPC()),
      Dimension(name = "timePointYears",
                keys = yearRange))
  )

  foodData = GetData(
    foodKey,
    flags = FALSE)
  return(foodData)
}
