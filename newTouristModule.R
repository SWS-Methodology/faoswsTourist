## Load required functions
library(faosws)
library(dplyr)
library(reshape2)
library(data.table)
library(faoswsUtil)

foodCode = "5141"

# 1. Pull in tourist data

DEBUG_MODE = Sys.getenv("R_DEBUG_MODE")

if(!exists("DEBUG_MODE") || DEBUG_MODE == ""){
  token = "3e0bbf64-9e57-4731-9341-5b7f7a4d0a42"
  GetTestEnvironment("https://hqlqasws1.hq.un.fao.org:8181/sws",token)
}

## set the keys to get the tourist data from the FAO working system

destinationAreaCodes <- faosws::GetCodeList("tourism", "tourist_flow",
                                            "destinationCountryM49")

originAreaCodes <- faosws::GetCodeList("tourism", "tourist_flow",
                                       "originCountryM49")

tourismElementCodes <- faosws::GetCodeList("tourism", "tourist_consumption",
                                           "tourismElement")

## set the year range to pull data from the SWS

yearRange <- swsContext.datasets[[1]]@dimensions$timePointYears@keys

##Pull the bidirectional movement data from SWS pertaining to tourist visitors
##to all countries

destinationCountryDim1 <- Dimension(name = "destinationCountryM49",
                  keys = destinationAreaCodes[, code])

originCountryDim2 <- Dimension(name = "originCountryM49", keys = originAreaCodes[, code])

tourismElementDim3 <- Dimension(name = "tourismElement", keys = ("60"))

timePointYearsDim4 <- Dimension(name = "timePointYears", keys = yearRange)

touristKey <- DatasetKey(domain = "tourism", dataset = "tourist_flow",
                  dimensions = list(destinationCountryDim1, originCountryDim2, 
                                    tourismElementDim3, timePointYearsDim4))

## download the first tourist data from the SWS

touristData.1 <- GetData(touristKey, flags = FALSE)

## remove the tourismElement column which is of no value to me here
touristData.1 <- touristData.1[, which(!grepl("tourism", colnames(touristData.1))), with=FALSE]

## change column names to small simple ones representing destination, origin,
## year and overnight visitor number
setnames(touristData.1, old = colnames(touristData.1), new = c("dest", "orig", "year", "onVisNum"))

# 2. Pull in consumption data

## set the keys to get the tourist data from the FAO working system
touristAreaCodes = faosws::GetCodeList("tourism", "tourist_consumption",
                                "geographicAreaM49")

## Pull number of mean number of days stayed, and number of single-day visitors
## from SWS pertaining to visitors to all countries
consumptionCountryDim1 = Dimension(name = "geographicAreaM49", keys = touristAreaCodes [, code])
consumptionElementDim2 = Dimension(name = "tourismElement", keys = c("20", "30"))
timePointYearsDim3 = Dimension(name = "timePointYears", keys = yearRange)
consumptionKey = DatasetKey(domain = "tourism", dataset = "tourist_consumption",
                 dimensions = list(consumptionCountryDim1, consumptionElementDim2, timePointYearsDim3))

## download the bi-direction tourism data from the SWS
consumptionData.2 <- GetData(consumptionKey, flags = FALSE)

## set the column names to small simple ones representing destination, database
## element, year and value
setnames(consumptionData.2, old = colnames(consumptionData.2), new = c("dest", "element", "year", "value"))

# 3. Calculate tourists leaving/coming to country A, B, ...

## cast the data table to get it in long format
consumptionData.2.1 <- as.data.table(dcast(consumptionData.2, dest + year ~ element))

## change the column names to something readable, "onVisDays" stands for the
## mean number of days that overnight visitors stayed in the destination country.
## "totDayVisNum" is the number of people who visited the country but for a
## single day, e.g. from a cruise ship
setnames(consumptionData.2.1, old = colnames(consumptionData.2.1), new = c("dest", "year", "onVisDays",
                                               "totDayVisNum"))

## replace missing day visitor numbers (NA) with zero, because it won't effect
## end calculations, but NA's cause equations to fail
consumptionData.2.1$totDayVisNum[is.na(consumptionData.2.1$totDayVisNum)] <- 0

## merge the two data sets, one containing overnight visitor numbers and number
## of days they visited, the other data set the number of tourists travelling to
## and from each country
touristOverNightData.3 <- merge(touristData.1, consumptionData.2.1, by=c("dest", "year"), all.x = TRUE)

## rearrange the column order to make it easier to view
touristOverNightData.3 <- setcolorder(touristOverNightData.3, neworder = c("year", "orig", "dest", "onVisNum",
                                         "onVisDays", "totDayVisNum"))

## a small number of countries are missing values for overnight visitor days,
## "onVisDays" and this affects the bi-directional calculations for them, but
## also all of the other countries as well, so this imputes the missing number
## of days, by taking the mean of all day numbers present, grouped by year.
touristOverNightData.3[, onVisDays := ifelse(is.na(onVisDays), mean(onVisDays, na.rm=TRUE),
                            onVisDays), by = year]

## calculate the total number tourist visitor days, the product of overnight
## visitor number and days per visit
touristOverNightData.3[, onVisTotDays := onVisNum * onVisDays]


## calculate a new total overnight visitor number per country of destination, to
## be used later to proportion the day visitor number, because we do not have
## data for country of origin, and allocate them to a country of origin,
## assuming they arrive in the same relative proportions as the overnight
## visitors
touristOverNightData.3[, totOnVisNum := sum(onVisNum), by=list(year,dest)]

## create a new total visitor days by summing the overnight visitor days, and
## the day visitor days
touristOverNightData.3[, totVisDays := onVisTotDays + totDayVisNum]


## calculate total visitor days per year who leaves your country
daysOut.4 <- touristOverNightData.3[, list(daysOut = sum(totVisDays, na.rm = T)),
                 by = c('orig', 'year')]

## calculate total visitor days per year who comes to the country
daysIn.5 <- touristOverNightData.3[, list(daysIn = sum(totVisDays, na.rm = T)),
                by = c('dest', 'year')]

## change column name from "orig" to "country"
setnames(daysOut.4, 'orig', 'country')

## change column name from "dest" to "country"
setnames(daysIn.5, 'dest', 'country')

##-------------
## tourist data
##-------------

## merge daysOut and daysIn to allow calculation of days net per country and year
tourismDays.6 <- merge(daysOut.4, daysIn.5, by=c("country", "year"), all.x = TRUE)
tourismDays.6[, daysNet := daysIn - daysOut]

# Pull food consumption

## set the keys to get the calorie consumption, by individual FBS commodity for
## each country from the FAO working system
foodAreaCodes <- faosws::GetCodeList("agriculture", "agriculture", "geographicAreaM49")
foodElementCodes <- faosws::GetCodeList("suafbs", "fbs", "measuredElementSuaFbs")

## the Item codes contain a hierarchy.  We need to determine all the child
## nodes of the hierarchy and add them to get total consumption.
foodItemTree <- GetCodeTree("agriculture", "agriculture", "measuredItemCPC")
oldAreaCodes <- GetCodeList("agriculture", "agriculture", "geographicAreaM49")
foodItemTree <- adjacent2edge(foodItemTree)
children <- setdiff(foodItemTree$children, foodItemTree$parent)

## Pull the supply utilization account(SUA) food balance sheet (FBS) data from
## SWS pertaining to calorie consumption from each commodity in each country
countryCodeDim1 <- Dimension(name = "geographicAreaM49",
                  keys = oldAreaCodes[type == "country", code])
## A bit hackish: get population from total calories and total calories/person/day
foodCodeDim2 <- Dimension(name = "measuredElement", keys = c(foodCode))
itemCPCDim3 <- Dimension(name = "measuredItemCPC", keys = children)
timePointYearsDim4 <- Dimension(name = "timePointYears", keys = yearRange)
calorieConsumptionKey <- DatasetKey(domain = "agriculture", dataset = "agriculture",
                  dimensions = list(countryCodeDim1, foodCodeDim2, itemCPCDim3, timePointYearsDim4))

## download the calorie consumption data from the SWS
calorieConsumptionData.7 <- GetData(calorieConsumptionKey, flags = FALSE)

calorieConsumptionData.7.1 <- dcast.data.table(calorieConsumptionData.7,
                          geographicAreaM49 + measuredItemCPC + timePointYears ~ measuredElement,
                          value.var = "Value")

## set the column names to small simple ones representing destination cuntry, database
## element, year and total calories
setnames(calorieConsumptionData.7.1, old = c("geographicAreaM49", "measuredItemCPC", "timePointYears", "5141"),
         new = c("country", "item", "year", "totalCal"))


## Pull the population datasets

countryCodePopDim1 <- Dimension(name = "geographicAreaM49",
                  keys = oldAreaCodes[type == "country", code])
elementPopDim2 <- Dimension(name = "measuredElementPopulation", keys = c("21"))
timePointYearsDim3 <- Dimension(name = "timePointYears", keys = yearRange)
popKey <- DatasetKey(domain = "population", dataset = "population",
                  dimensions = list(countryCodePopDim1, elementPopDim2, timePointYearsDim3))

## download the population data from the SWS
popData.8 <- GetData(popKey, flags = FALSE)

popData.8.1 <- dcast.data.table(popData.8,
                          geographicAreaM49 + timePointYears ~ measuredElementPopulation,
                          value.var = "Value")

## set the column names to small simple ones representing destination cuntry, 
##  year and total population
setnames(popData.8.1, old = c("geographicAreaM49", "timePointYears", "21"),
         new = c("country", "year", "pop"))


## Merge population data with food data

setkey(calorieConsumptionData.7.1, "country", "year")
setkey(popData.8.1, "country", "year")

calorieConsumptionPopData.9 <- merge(calorieConsumptionData.7.1, popData.8.1, by = c("country", "year"), 
               all.x = TRUE )

## compute total calories per person per day in orig country
## Population is in 1000s
calorieConsumptionPopData.9[, calPerPersonPerDay := totalCal / 365 / (pop * 1000)]

## merge data8 and data5 to allow calculation of calories 
## by commodity per year for who goes to the country and who left  

setkey(tourismDays.6, "country", "year")
setkey(calorieConsumptionPopData.9, "country", "year")

caloriesCommoditiesTourist.10 <- merge(calorieConsumptionPopData.9, tourismDays.6, by = c("country", "year"), 
      all.x = T)

caloriesCommoditiesTourist.10[, calOutCountry := daysOut * calPerPersonPerDay]
caloriesCommoditiesTourist.10[, calInCountry := daysIn * calPerPersonPerDay]
caloriesCommoditiesTourist.10[, calNetCountry := daysNet * calPerPersonPerDay]

## Get rid of some of the columns that we don't need anymore:

caloriesCommoditiesTourist.10[, c("totalCal", "pop", "calPerPersonPerDay", "daysOut", 
          "daysIn", "daysNet") := NULL]

## Net calories
touristCaloriesNetCountryByItem.11 <- caloriesCommoditiesTourist.10[ , list(calNetCountry),
                                          by = list(year, country, item) ]

touristCaloriesNetCountryByItem.11[, measuredElement:= NA]
touristCaloriesNetCountryByItem.11[, flagObservationStatus:= "I"]
touristCaloriesNetCountryByItem.11[, flagMethod:= "e"]


# Save the data

SaveData(domain = "agriculture", dataset = "agriculture", data = touristCaloriesNetCountryByItem.11)
