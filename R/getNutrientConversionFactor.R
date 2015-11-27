getNutrientConversionFactors <- function(country, item) {
    
    data=data.table(country=country, item=item)
    ## Data Quality Checks
    #stopifnot(sum(names(data) %in% c("country", "item")) == 2)
    
    ## Pulling calories Data
    geographiCal <- GetCodeList("agriculture", "aupus_ratio", dimension = "geographicAreaM49")
    elementCal <- GetCodeList("agriculture", "aupus_ratio", dimension = "measuredElement")
    itemCal <- GetCodeList("agriculture", "aupus_ratio", dimension = "measuredItemCPC")
    timeCal <- GetCodeList("agriculture", "aupus_ratio", dimension = "timePointYearsSP")
    
    geographiCalDim1Country <- Dimension(name = "geographicAreaM49", keys = geographiCal[, code])
    # 261, 271, 281 (calories, proteins, and fats)
    elementCalDim2 <- Dimension(name = "measuredElement", keys = "261") # calorie code
    itemCalDim3 <- Dimension(name = "measuredItemCPC", keys = itemCal[, code])
    timeCalDim4 <- Dimension(name = "timePointYearsSP", keys = "0")
    dataCalKeyCountry <- DatasetKey(domain = "agriculture", dataset = "aupus_ratio", 
                                    dimensions = list(geographiCalDim1Country, elementCalDim2, 
                                                      itemCalDim3, timeCalDim4))
    # country dependency
    calorieData <- GetData(dataCalKeyCountry, flags = FALSE)
    calorieData[, c("measuredElement", "timePointYearsSP") := NULL]
    setnames(calorieData, old = c("geographicAreaM49", "measuredItemCPC", "Value"),
             new = c("country", "item", "valueCal"))
    calorieCountry <- calorieData[country != "0"]
    
    # standard values
    calorieWildCard <- calorieData[country == "0"]
    calorieWildCard[, c("country") := NULL]
    
    # standard values with generic cpc (without "dot")
    ## split by "." to find a itemCPC
    calorieWildCard$itemCPC <- sapply(strsplit(as.character(calorieWildCard$item), 
                                               ".", fixed = TRUE), "[", 1)
    
    calorieGenericCPC <- calorieWildCard[, list(valueCal = mean(valueCal)),
                                         by = itemCPC]
    
    calorieWildCard[, c("itemCPC") := NULL]
    
    # 3 datasets in the list: calorieCountry, calorieWildCard and calorieGenericCPC
    calorieList <- list(calorieCountry, calorieWildCard, calorieGenericCPC)
    
    ## We need to have a item code without "." in our data
    data$itemCPC <- sapply(strsplit(as.character(data$item), ".", fixed = TRUE), 
                           "[", 1)
    
    ## We'll do 3 merges with the datasets calorieCountry, calorieWildCard and 
    ## calorieGenericCPC in this sequency.
    
    # merge 1
    dataMerge1 <- merge(data, calorieList[[1]],
                        by=c("country", "item"), all.x=T)
    #length(unique(data[is.na(valueCal)]$itemCPC))
    data1 <- dataMerge1[is.na(valueCal) != T] # save this dataset
    
    # merge 2
    data2 <- dataMerge1[is.na(valueCal) == T]
    data2[, valueCal := NULL]
    
    data2 <- merge(data2, calorieList[[2]],
                   by=c("item"), all.x=T)
    #length(unique(data2[is.na(valueCal)]$item))
    data2.1 <- data2[is.na(valueCal) != T] # save this dataset
    
    setcolorder(data2.1, c("country", "item", "itemCPC", "valueCal"))
    
    # merge 3
    data3 <- data2[is.na(valueCal) == T]
    data3[, valueCal := NULL]
    
    data3 <- merge(data3, calorieList[[3]],
                   by=c("itemCPC"), all.x=T)
    #length(unique(data3[is.na(valueCal)]$item))
    
    data3[, valueCal := ifelse(is.na(valueCal), 0, valueCal)] # save this dataset
    
    setcolorder(data3, c("country", "item", "itemCPC", "valueCal"))
    
    # rbind
    data <- rbind(data1, data2.1, data3)
    return(data$valueCal)
}