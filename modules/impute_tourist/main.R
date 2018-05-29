## Load required functions
suppressMessages({
library(faosws)
library(dplyr)
library(reshape2)
library(data.table)
library(faoswsUtil)
library(faoswsTourist)
library(imputeTS)
})


R_SWS_SHARE_PATH = Sys.getenv("R_SWS_SHARE_PATH")

# This return FALSE if on the Statistical Working System
if(CheckDebug()){

  message("Not on server, so setting up environment...")

  library(faoswsModules)
  SETTINGS <- ReadSettings("modules/impute_tourist/sws.yml")

  # If you're not on the system, your settings will overwrite any others
  R_SWS_SHARE_PATH <- SETTINGS[["share"]]

  # Define where your certificates are stored
  SetClientFiles(SETTINGS[["certdir"]])

  # Get session information from SWS. Token must be obtained from web interface
  GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                     token = SETTINGS[["token"]])
  files = dir("~/Github/faoswsTourist/R", full.names = TRUE)
  sapply(files, source)

}

## set the year range to pull data from the SWS

# Parameters: year to process

minYearToProcess <- as.numeric(ifelse(is.null(swsContext.computationParams$minYearToProcess), "1990",
                                      swsContext.computationParams$minYearToProcess))

maxYearToProcess <- as.numeric(ifelse(is.null(swsContext.computationParams$maxYearToProcess), "2016",
                                      swsContext.computationParams$maxYearToProcess))

if(minYearToProcess > maxYearToProcess | maxYearToProcess < minYearToProcess)
  stop("Please check the time range for the years to be processed")

yearRange <- as.character(minYearToProcess:maxYearToProcess)

## Obtain the complete imputation Datakey
completeImputationTouristKey = getCompleteImputationKey("tourist")
completeImputationTouristKey@dimensions$timePointYears@keys <- yearRange

completeImputationFoodKey = getCompleteImputationKey("food")
completeImputationFoodKey@dataset <- "fooddata"
completeImputationFoodKey@dimensions$timePointYears@keys <- yearRange

destinationCountryM49 <- completeImputationTouristKey@dimensions$destinationCountryM49@keys
originCountryM49 <- completeImputationTouristKey@dimensions$originCountryM49@keys
areaCodesM49 <- unique(destinationCountryM49, originCountryM49)

## Step 1: Pull the food consumption
# foodConsumption <- getFoodConsumption(yearRange)
foodConsumption <- GetData(completeImputationFoodKey, flags = TRUE)
foodConsumption[, c("measuredElement", "flagObservationStatus", "flagMethod") := NULL]

## Set the column names to small simple ones representing country, commodity,
## year and total food consumption.

setnames(foodConsumption, old = c("geographicAreaM49", "measuredItemCPC",
                                  "timePointYears", "Value"),
         new = c("country", "item", "year", "totalFood"))

## Pulling the calories data
caloriesData <- faoswsUtil::getNutritiveFactors(geographicAreaM49 = foodConsumption$country,
                                    measuredElement = "261",
                                    measuredItemCPC = foodConsumption$item,
                                    timePointYearsSP = foodConsumption$year)
setnames(caloriesData,
         c("geographicAreaM49", "measuredItemCPC", "timePointYearsSP", "Value"),
         c("country", "item", "year", "valueCal"))

## Merge the food data with calories data
foodConsumption <- merge(foodConsumption,
                         caloriesData[, c(
                           "country", "item", "year", "valueCal"), with=F],
                         by = c("country", "item", "year"), all.x=T)

## Step 2: Pull the population datasets
## The element 511 contains the FBS population numbers
populationCodes <- "511"
dimPop <- Dimension(name = "measuredElement", keys = populationCodes)
dimM49 <- Dimension(name = "geographicAreaM49", keys = areaCodesM49)
dimTime <- Dimension(name = "timePointYears", keys = yearRange)

popKey <- DatasetKey(domain = "population", dataset = "population_unpd",
                     dimensions = list(dimM49, dimPop, dimTime))
popData <- GetData(popKey, flags = FALSE)

popData <- dcast.data.table(popData,
                            geographicAreaM49 + timePointYears ~ measuredElement,
                                value.var = "Value")
setnames(popData,
         old = c("geographicAreaM49", "timePointYears", "511"),
         new = c("country", "year", "pop"))

# China
popData[country == 156, country := "1248"]

## Merge population data with food data
setkey(foodConsumption, "country", "year")
setkey(popData, "country", "year")

foodConsumptionPop <- merge(foodConsumption, popData, by = c("country", "year"),
                                  all.x = TRUE )

## Compute total calories per person per day in orig country
## Population is in 1000s
foodConsumptionPop[, calPerPersonPerDay := (totalFood * valueCal * 10000) / 365 / (pop * 1000)]

## Compute the calorie consumption per country, per person and per day
calorieCountryDay <- foodConsumptionPop[, list(totalCalDay = sum(calPerPersonPerDay, na.rm=T)),
                                                by=list(country, year)]

## Let's create the baseline consumption level (2500 calories per person per day).
calorieCountryDay[, baselineCalDay:= totalCalDay/2500]

## Step 3: Pull the tourist data
## Pull the bidirectional movement data from SWS pertaining to tourist visitors
## to all countries.

touristFlowData <- GetData(completeImputationTouristKey, flags = FALSE)

# China
touristFlowData[destinationCountryM49 == "156", destinationCountryM49 := "1248"]
touristFlowData[originCountryM49 == "156", originCountryM49 := "1248"]

cat("Tourist data loaded with ", nrow(touristFlowData), " rows.")

## Remove the tourismElement column which is of no value to me here
touristFlowData[, "tourismElement" := NULL]

## Change column names to small simple ones representing destination, origin,
## year and overnight visitor number
setnames(touristFlowData, old = c("destinationCountryM49", "originCountryM49", "timePointYears", "Value"),
         new = c("dest", "orig", "year", "overnightVisitNum"))

touristFlowData[, destOrigin := as.character(paste(dest, orig, sep = ","))]

## Filling the full time series
timeSeriesDataFlow <- as.data.table(expand.grid(destOrigin = unique(touristFlowData$destOrigin),
                                            year = yearRange))

timeSeriesDataFlow <- merge(timeSeriesDataFlow,
                        touristFlowData[, c("destOrigin", "dest", "orig", "year", "overnightVisitNum"),
                                    with = F], by = c("destOrigin", "year"), all.x = T)

timeSeriesDataFlow <- timeSeriesDataFlow[order(destOrigin, -as.numeric(year))]
timeSeriesDataFlow[is.na(dest), dest := gsub(",.*$", "", destOrigin)]
timeSeriesDataFlow[is.na(orig), orig := sub('.*,\\s*', '', destOrigin)]

tab <- timeSeriesDataFlow[is.na(overnightVisitNum), .N, destOrigin]
setnames(tab, "N", "numbMissing")
tab <- tab[order(-numbMissing)]

timeSeriesDataFlow <- merge(timeSeriesDataFlow, tab, by = "destOrigin")

timeSeriesDataFlow[numbMissing < max(numbMissing),
               interpolationMA := na.ma(
                 overnightVisitNum, weighting = "linear"), by = list(destOrigin)]

missingCases <- timeSeriesDataFlow[numbMissing == max(numbMissing) & !is.na(overnightVisitNum),
                               c("destOrigin", "overnightVisitNum"), with = F]

timeSeriesDataFlow <- merge(timeSeriesDataFlow,
                        missingCases,
                        by = "destOrigin",
                        all.x = T)

timeSeriesDataFlow[, overnightVisitNum := interpolationMA]
timeSeriesDataFlow[is.na(overnightVisitNum), overnightVisitNum := overnightVisitNum.y]
timeSeriesDataFlow[, c("overnightVisitNum.x", "overnightVisitNum.y", "interpolationMA",
                   "numbMissing", "destOrigin") := NULL]

## Step 4: Pull the tourist consumption data
## Pull number of mean number of days stayed, and number of single-day visitors
## from SWS pertaining to visitors to all countries

# 20 - average nights stayed
# 30 - same-day visitor numbers
# 40 - average days stayed

dimTouElements = Dimension(name = "tourismElement", keys = c("20", "30", "40"))

touConsumptionKey = DatasetKey(domain = "tourism", dataset = "tourist_consumption",
                            dimensions = list(dimM49, dimTouElements, dimTime))

touConsumptionData <- GetData(touConsumptionKey, flags = FALSE)

# China
touConsumptionData[geographicAreaM49 == "156", geographicAreaM49 := "1248"]
## Set the column names to small simple ones representing destination, database
## element, year and value
setnames(touConsumptionData, old = c("geographicAreaM49", "tourismElement", "timePointYears", "Value"),
         new = c("dest", "element", "year", "value"))

# Step 5: Calculate tourists leaving/coming to country A, B, ...

## cast the data table to get it in long format
touConsumptionData <- as.data.table(dcast(touConsumptionData, dest + year ~ element))

## Change the column names to something readable, "averageNights" and "averageDays"
## stand for the mean number of days that overnight visitors stayed in the
## destination country.
## "sameDayVistNum" is the number of people who visited the country but for a
## single day, e.g. from a cruise ship
setnames(touConsumptionData, old = c("20", "30", "40"),
         new = c("averageNights", "sameDayVistNum", "averageDays"))

# onVisDays20 = averageNights
# totDayVisNum = sameDayVistNum
# onVisDays40 = averageDays

touConsumptionData[!is.na(averageNights), averageNightsDays := averageNights]
touConsumptionData[is.na(averageNights) & !is.na(averageDays), averageNightsDays := averageDays]
touConsumptionData[, c("averageNights", "averageDays") := NULL]

timeSeriesDataConsumption <- as.data.table(expand.grid(dest = unique(touConsumptionData$dest),
                                            year = yearRange))

timeSeriesDataConsumption <- merge(timeSeriesDataConsumption, touConsumptionData,
                                   by = c("dest", "year"), all.x = T)

timeSeriesDataConsumption <- timeSeriesDataConsumption[order(dest, -as.numeric(year))]

auxTabSameDay <- timeSeriesDataConsumption[is.na(sameDayVistNum), .N, dest]
setnames(auxTabSameDay, "N", "numbMissing")
auxTabSameDay = auxTabSameDay[order(-numbMissing)]

timeSeriesDataConsumption <- merge(timeSeriesDataConsumption, auxTabSameDay, by = "dest")

timeSeriesDataConsumption[numbMissing < length(yearRange) - 1,
                          interpMASameDayVistNum := na.ma(sameDayVistNum, weighting = "linear"),
                          by = list(dest)]

auxTabAverageNightsDays <- timeSeriesDataConsumption[is.na(averageNightsDays), .N, dest]
setnames(auxTabAverageNightsDays, "N", "numbMissing")
auxTabAverageNightsDays = auxTabAverageNightsDays[order(-numbMissing)]

timeSeriesDataConsumption <- merge(timeSeriesDataConsumption, auxTabAverageNightsDays, by = "dest")

timeSeriesDataConsumption[numbMissing.y < length(yearRange) - 1,
                          interpMAAverageNightsDays := na.ma(
                            averageNightsDays, weighting = "linear"), by = list(dest)]

timeSeriesDataConsumption[, c("sameDayVistNum", "averageNightsDays",
                              "numbMissing.x", "numbMissing.y") := NULL]

setnames(timeSeriesDataConsumption, "interpMASameDayVistNum", "sameDayVistNum")
setnames(timeSeriesDataConsumption, "interpMAAverageNightsDays", "averageNightsDays")

## Replace missing day visitor numbers (NA) with zero, because it won't effect
## end calculations, but NA's cause equations to fail
timeSeriesDataConsumption$sameDayVistNum[is.na(timeSeriesDataConsumption$sameDayVistNum)] <- 0

## Merge the two data sets, one containing overnight visitor numbers and number
## of days they visited, the other data set the number of tourists travelling to
## and from each country
touristData <- merge(timeSeriesDataFlow, timeSeriesDataConsumption,
                                by=c("dest", "year"), all.x = TRUE)

## Rearrange the column order to make it easier to view
touristData <- setcolorder(touristData,
                           neworder = c("year", "orig", "dest", "overnightVisitNum",
                                        "averageNightsDays", "sameDayVistNum"))

touristData$sameDayVistNum[is.na(touristData$sameDayVistNum)] <- 0

## A small number of countries are missing values for "averageNightsDays" and
## this affects the bi-directional calculations for them, but also all of the
## other countries as well, so this imputes the missing number of days,
## by taking the mean of all day numbers present, grouped by year.
touristData[, averageNightsDays := ifelse(
  is.na(averageNightsDays), mean(averageNightsDays, na.rm=TRUE),
  averageNightsDays), by = year]

## Calculate the total number of tourist visitor days, the product of overnight
## visitor number and days per visit and adding the same day visitor numbers
touristData[, totVisitDays := overnightVisitNum * averageNightsDays + sameDayVistNum]

## We can rid of these variables as we need the totVisitDays
touristData[, c("overnightVisitNum", "averageNightsDays", "sameDayVistNum") := NULL]

## We need to merge touristData with calorieCountryDay to calculate
## the consumption in each country. In our approach, the person comes from country A
## to country B eats the same food types as people in country B but eats the same
## calorie amounts as in their home country. In case we don't have totalCalDay for
## the country A, we will assume the same calories amount in country B.

# Merge by orig country
setnames(calorieCountryDay, "country", "orig")
setkeyv(calorieCountryDay, c("orig", "year"))

setkeyv(touristData, c("orig", "dest", "year"))

keys = c("orig", "year")
touristData <- merge(touristData, calorieCountryDay, by = keys, all.x = T)

setnames(touristData, old="baselineCalDay", new="baselineCalDayOrigCountry")
touristData[, totalCalDay := NULL]

# Merge by dest country
setnames(calorieCountryDay, "orig", "dest")

setkeyv(calorieCountryDay, c("dest", "year"))

setkeyv(touristData, c("orig", "dest", "year"))
keys = c("dest", "year")
touristData <- merge(touristData, calorieCountryDay, by = keys, all.x = T)

setnames(touristData, old = "baselineCalDay", new = "baselineCalDayDestCountry")
touristData[, totalCalDay := NULL]

touristData[, baselineCalDayOrigCountry := ifelse(
  is.na(baselineCalDayOrigCountry), baselineCalDayDestCountry, baselineCalDayOrigCountry)]

## Now, we'll use only the baselineCalDayOrigCountry
touristData[, baselineCalDayDestCountry := NULL]

## There are combinations of countries (origin-destination that we don't have the
## amount of calories consumed, so we are going to compute the average for those cases.

averageCalTab <- touristData[, list(averageCal = mean(
  baselineCalDayOrigCountry, na.rm = T)),
  by = list(orig, year)]

keys = c("orig", "year")
touristData <- merge(touristData, averageCalTab, by = keys, all.x = T)
touristData[is.na(baselineCalDayOrigCountry), baselineCalDayOrigCountry := averageCal]
touristData[, "averageCal" := NULL]

## Calculate total visitor days per year who leaves your country using the baseline
daysOut <- touristData[, list(daysOut = sum(totVisitDays * baselineCalDayOrigCountry, na.rm=T)),
                                      by = c('orig', 'year')]

## Calculate total visitor days per year who comes to the country
daysIn <- touristData[, list(daysIn = sum(totVisitDays * baselineCalDayOrigCountry, na.rm=T)),
                                     by = c('dest', 'year')]

## change column name from "orig" to "country"
setnames(daysOut, 'orig', 'country')

## change column name from "dest" to "country"
setnames(daysIn, 'dest', 'country')

##-------------
## tourist data
##-------------

## Merge daysOut and daysIn to allow calculation of days net per country and year
tourismDays <- merge(daysOut, daysIn, by = c("country", "year"), all.x = TRUE)
tourismDays[, daysNet := daysIn - daysOut]

## Merge tourismDays with the food consumption data
setkey(tourismDays, "country", "year")
setkey(foodConsumptionPop, "country", "year")

foodTouristConsumption <- merge(foodConsumptionPop, tourismDays,
                                by = c("country", "year"), all.x = T)

foodTouristConsumption[, calOutCountry := daysOut * calPerPersonPerDay]
foodTouristConsumption[, calInCountry := daysIn * calPerPersonPerDay]
foodTouristConsumption[, calNetCountry := daysNet * calPerPersonPerDay]
foodTouristConsumption[, netFood := calNetCountry/(valueCal * 10000)]

## Get rid of some of the columns that we don't need anymore:
foodTouristConsumption[, c("totalFood", "pop", "valueCal", "calPerPersonPerDay",
                           "daysOut", "daysIn", "daysNet", "calOutCountry",
                           "calInCountry", "calNetCountry") := NULL]

## Net calories
foodTouristConsumption[, tourismElement:= "100"]
foodTouristConsumption[, flagObservationStatus:= "I"]
foodTouristConsumption[, flagMethod:= "e"]

# Save the data

setnames(foodTouristConsumption,
         old = c("country", "year", "item", "netFood"),
         new = c("geographicAreaM49", "timePointYears", "measuredItemCPC", "Value"))
setcolorder(foodTouristConsumption,
            c("timePointYears", "geographicAreaM49", "measuredItemCPC",
              "tourismElement", "Value", "flagObservationStatus", "flagMethod"))

foodTouristConsumption <- foodTouristConsumption[!is.na(Value)]

stats = SaveData(domain = "tourism", dataset = "tourismprod",
                 data = foodTouristConsumption, waitTimeout = 1800)

paste0("Tourist module completed successfully!!!",
       stats$inserted, " observations written, ",
       stats$ignored, " weren't updated, ",
       stats$discarded, " had problems.")
