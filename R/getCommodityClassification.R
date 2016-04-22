##' Get Commodity Classification
##' 
##' This function classifies the cpc code in "Consumable, main", "Non-Consumable"
##' or "Consumable, residual".
##'
##' @param cpcCodes A character vector containing the CPC codes.
##'
##' @return A character vector whose ith element is the food classification
##'  corresponding to the passed CPC code in position i. If no valid cpc code is 
##'  found, an NA is returned in this place.
##'
##' @examples
##' \dontrun{
##' getCommodityClassification(c("0111", "23110", "F0020"))
##' }
##' 
##' @import data.table
##' 
##' @export
##'


getCommodityClassification <- function(cpcCodes) {
    ## Data Quality Checks
    stopifnot(is(cpcCodes, "character"))
    if(!exists("swsContext.datasets")){
        stop("No swsContext.datasets object defined.  Thus, you probably ",
             "won't be able to read from the SWS and so this function won't ",
             "work.")
    }
    
    ## After putting this table on SWS, we should use ReadDatatable to pull it.
    ## This table should has 4 columns: cpcCode, fclCode, description and type.
    ## Where the variable "type" has 3 classifications.
    
    #foodClassification = faosws::ReadDatatable(table = "foodClassification")
    
    ## Whereas the table is not on SWS
    foodClassification = fread(
        paste0(R_SWS_SHARE_PATH, "/caetano/food/food_classification.csv"))
    
    ## Merge the cpcCodes with the foodClassification
    result = merge(data.table(measuredItemCPC = cpcCodes, index = 1:length(cpcCodes)),
                   foodClassification, by = "measuredItemCPC", all.x = TRUE)
    
    setkeyv(result, "measuredItemCPC")
    result <- result[order(result$index)]
    result[, type]
}