##' Get All Countries
##'
##' This function pulls all the countries in M49 code.
##'
##' @export
##'

getAllCountries = function(){
  GetCodeList(domain = "agriculture",
              dataset = "aproduction",
              dimension = "geographicAreaM49")[type == "country", code]
}
