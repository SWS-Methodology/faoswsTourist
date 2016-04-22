##' Get All Item CPC
##'
##' This function pulls all the cpc codes.
##'
##' @export
##'

getAllItemCPC = function(){
  GetCodeList(domain = "agriculture",
              dataset = "aproduction",
              dimension = "measuredItemCPC")[, code]
}
