## Load required functions
library(faosws)
library(dplyr)
library(reshape2)
library(data.table)
library(faoswsUtil)

foodCode = "5141"

DEBUG_MODE = Sys.getenv("R_DEBUG_MODE")
R_SWS_SHARE_PATH = Sys.getenv("R_SWS_SHARE_PATH")

if(!exists("DEBUG_MODE") || DEBUG_MODE == ""){
  token = "41558a20-c419-4821-8288-2dc7ccbc5ecf"
  GetTestEnvironment("https://hqlqasws1.hq.un.fao.org:8181/sws",token)

}

files = dir("~/Github/faoswsTourist/R", full.names = TRUE)
sapply(files, source)

## set the year range to pull data from the SWS
swsContext.computationParams$startYear <- as.numeric(swsContext.computationParams$startYear)
swsContext.computationParams$endYear <- as.numeric(swsContext.computationParams$endYear)
if(swsContext.computationParams$startYear > swsContext.computationParams$endYear)
    stop("First Year should be smallest than End Year")
## yearRange <- swsContext.datasets[[1]]@dimensions$timePointYears@keys
yearRange <- swsContext.computationParams$startYear:swsContext.computationParams$endYear
yearRange <- as.character(yearRange)

# Step 1: Pull the food consumption

## set the keys to get the calorie consumption, by individual FBS commodity for
## each country from the FAO working system
foodAreaCodes <- faosws::GetCodeList("agriculture", "aproduction", "geographicAreaM49")
foodElementCodes <- faosws::GetCodeList("suafbs", "fbs", "measuredElementSuaFbs")

## the Item codes contain a hierarchy.  We need to determine all the child
## nodes of the hierarchy and add them to get total consumption.
oldAreaCodes <- GetCodeList("agriculture", "aproduction", "geographicAreaM49")
itemCodes <- GetCodeList("agriculture", "aproduction", "measuredItemCPC")[, code]

## Pull the supply utilization account(SUA) food balance sheet (FBS) data from
## SWS pertaining to calorie consumption from each commodity in each country
countryCodeDim1 <- Dimension(name = "geographicAreaM49",
                             keys = oldAreaCodes[type == "country", code])
## A bit hackish: get population from total calories and total calories/person/day
foodCodeDim2 <- Dimension(name = "measuredElement", keys = c(foodCode))
itemCPCDim3 <- Dimension(name = "measuredItemCPC", keys = itemCodes)
timePointYearsDim4 <- Dimension(name = "timePointYears", keys = yearRange)
foodConsumptionKey <- DatasetKey(domain = "agriculture", dataset = "aproduction",
                                 dimensions = list(countryCodeDim1, foodCodeDim2, itemCPCDim3, timePointYearsDim4))

## download the calorie consumption data from the SWS
foodConsumptionData.1 <- GetData(foodConsumptionKey, flags = FALSE)

foodConsumptionData.1.1 <- dcast.data.table(foodConsumptionData.1,
                                            geographicAreaM49 + measuredItemCPC + timePointYears ~ measuredElement,
                                            value.var = "Value")

## set the column names to small simple ones representing destination cuntry, database
## element, year and total calories
setnames(foodConsumptionData.1.1, old = c("geographicAreaM49", "measuredItemCPC", "timePointYears", "5141"),
         new = c("country", "item", "year", "totalFood"))

# Pull calories data from each item using getNutrientConversionFactors function
foodConsumptionData.1.1[, valueCal := getNutrientConversionFactors(country, item)]

## Step 2: Pull the population datasets
countryCodePopDim1 <- Dimension(name = "geographicAreaM49",
                                keys = oldAreaCodes[type == "country", code])
elementPopDim2 <- Dimension(name = "measuredElementPopulation", keys = c("21"))
timePointYearsDim3 <- Dimension(name = "timePointYears", keys = yearRange)
popKey <- DatasetKey(domain = "population", dataset = "population",
                     dimensions = list(countryCodePopDim1, elementPopDim2, timePointYearsDim3))

## download the population data from the SWS
popData.2 <- GetData(popKey, flags = FALSE)

popData.2.1 <- dcast.data.table(popData.2,
                                geographicAreaM49 + timePointYears ~ measuredElementPopulation,
                                value.var = "Value")

## set the column names to small simple ones representing destination cuntry, 
##  year and total population
setnames(popData.2.1, old = c("geographicAreaM49", "timePointYears", "21"),
         new = c("country", "year", "pop"))

## Merge population data with food data
setkey(foodConsumptionData.1.1, "country", "year")
setkey(popData.2.1, "country", "year")

foodConsumptionPopData.3 <- merge(foodConsumptionData.1.1, popData.2.1, by = c("country", "year"), 
                                  all.x = TRUE )

## compute total calories per person per day in orig country
## Population is in 1000s
foodConsumptionPopData.3[, calPerPersonPerDay := (totalFood * valueCal * 10000) / 365 / (pop * 1000)]

## Compute the calorie consumption per country, per person and per day
calorieCountryDay.4 <- foodConsumptionPopData.3[, list(totalCalDay = sum(calPerPersonPerDay, na.rm=T)),
                                                by=list(country)]

## Let's to create the baseline consumption level (2500 calories per person per day).
calorieCountryDay.4[, baselineCalDay:= totalCalDay/2500]

## Step 3: Pull the tourist data
## set the keys to get the tourist data from the FAO working system
destinationAreaCodes <- faosws::GetCodeList("tourism", "tourist_flow",
                                            "destinationCountryM49")

originAreaCodes <- faosws::GetCodeList("tourism", "tourist_flow",
                                       "originCountryM49")

tourismElementCodes <- faosws::GetCodeList("tourism", "tourist_consumption",
                                           "tourismElement")

##Pull the bidirectional movement data from SWS pertaining to tourist visitors
##to all countries
destinationCountryDim1 <- Dimension(name = "destinationCountryM49",
                                    keys = destinationAreaCodes[, code])

originCountryDim2 <- Dimension(name = "originCountryM49",
                               keys = originAreaCodes[, code])

tourismElementDim3 <- Dimension(name = "tourismElement", keys = ("60"))
timePointYearsDim4 <- Dimension(name = "timePointYears", keys = yearRange)

touristKey <- DatasetKey(domain = "tourism", dataset = "tourist_flow",
                         dimensions = list(destinationCountryDim1, originCountryDim2, 
                                           tourismElementDim3, timePointYearsDim4))

## download the first tourist data from the SWS
touristData.5 <- GetData(touristKey, flags = FALSE)

cat("Tourist data loaded with ", nrow(touristData.5), " rows.")

## remove the tourismElement column which is of no value to me here
touristData.5 <- touristData.5[, which(!grepl("tourism", colnames(touristData.5))), 
                               with=FALSE]

## change column names to small simple ones representing destination, origin,
## year and overnight visitor number
setnames(touristData.5, old = c("destinationCountryM49", "originCountryM49", "timePointYears", "Value"), 
         new = c("dest", "orig", "year", "onVisNum"))

# Step 4: Pull the tourist consumption data
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
consumptionData.6 <- GetData(consumptionKey, flags = FALSE)

## set the column names to small simple ones representing destination, database
## element, year and value
setnames(consumptionData.6, old = c("geographicAreaM49", "tourismElement", "timePointYears", "Value"), 
         new = c("dest", "element", "year", "value"))

# Step 5: Calculate tourists leaving/coming to country A, B, ...

## cast the data table to get it in long format
consumptionData.6.1 <- as.data.table(dcast(consumptionData.6, dest + year ~ element))

## change the column names to something readable, "onVisDays" stands for the
## mean number of days that overnight visitors stayed in the destination country.
## "totDayVisNum" is the number of people who visited the country but for a
## single day, e.g. from a cruise ship
setnames(consumptionData.6.1, old = c("20", "30"), 
         new = c("onVisDays", "totDayVisNum"))

## replace missing day visitor numbers (NA) with zero, because it won't effect
## end calculations, but NA's cause equations to fail
consumptionData.6.1$totDayVisNum[is.na(consumptionData.6.1$totDayVisNum)] <- 0

## merge the two data sets, one containing overnight visitor numbers and number
## of days they visited, the other data set the number of tourists travelling to
## and from each country
touristOverNightData.7 <- merge(touristData.5, consumptionData.6.1, 
                                by=c("dest", "year"), all.x = TRUE)

## rearrange the column order to make it easier to view
touristOverNightData.7 <- setcolorder(touristOverNightData.7, neworder = c("year", "orig", "dest", "onVisNum",
                                                                           "onVisDays", "totDayVisNum"))

## a small number of countries are missing values for overnight visitor days,
## "onVisDays" and this affects the bi-directional calculations for them, but
## also all of the other countries as well, so this imputes the missing number
## of days, by taking the mean of all day numbers present, grouped by year.
touristOverNightData.7[, onVisDays := ifelse(is.na(onVisDays), mean(onVisDays, na.rm=TRUE),
                                             onVisDays), by = year]

## calculate the total number tourist visitor days, the product of overnight
## visitor number and days per visit
touristOverNightData.7[, onVisTotDays := onVisNum * onVisDays]

## calculate a new total overnight visitor number per country of destination, to
## be used later to proportion the day visitor number, because we do not have
## data for country of origin, and allocate them to a country of origin,
## assuming they arrive in the same relative proportions as the overnight
## visitors
touristOverNightData.7[, totOnVisNum := sum(onVisNum), 
                       by=list(year,dest)]

## create a new total visitor days by summing the overnight visitor days, and
## the day visitor days
touristOverNightData.7[, totVisDays := onVisTotDays + totDayVisNum]

## We need to merge touristOverNightData.7 with calorieCountryDay.4 to calculate
## the consumption in each country. In our approach, the person comes from country A
## to country B eats the same food types as people in country B but eats the same
## calorie amounts as in their home country. In case we don't have totalCalDay for
## the country A, we will assume the same calories amount in country B.

# Merge by orig country
touristOverNightData.7.1 <- merge(touristOverNightData.7, calorieCountryDay.4, 
                                  by.x="orig", by.y="country", all.x=T)
setnames(touristOverNightData.7.1, old="baselineCalDay", new="baselineCalDayOrigCountry")
touristOverNightData.7.1[, totalCalDay := NULL]

# Merge by dest country
touristOverNightData.7.2 <- merge(touristOverNightData.7.1, calorieCountryDay.4, 
                                  by.x="dest", by.y="country", all.x=T)
setnames(touristOverNightData.7.2, old="baselineCalDay", new="baselineCalDayDestCountry")
touristOverNightData.7.2[, totalCalDay := NULL]

touristOverNightData.7.2[, baselineCalDayOrigCountry := ifelse(is.na(baselineCalDayOrigCountry), baselineCalDayDestCountry, 
                                                               baselineCalDayOrigCountry)]
# Now, we'll use only the baselineCalDayOrigCountry.
touristOverNightData.7.2[, baselineCalDayDestCountry := NULL]

## calculate total visitor days per year who leaves your country using the baseline
daysOut.8 <- touristOverNightData.7.2[, list(daysOut = sum(totVisDays * baselineCalDayOrigCountry, na.rm=T)),
                                      by = c('orig', 'year')]

## calculate total visitor days per year who comes to the country
daysIn.9 <- touristOverNightData.7.2[, list(daysIn = sum(totVisDays * baselineCalDayOrigCountry, na.rm=T)),
                                     by = c('dest', 'year')]

## change column name from "orig" to "country"
setnames(daysOut.8, 'orig', 'country')

## change column name from "dest" to "country"
setnames(daysIn.9, 'dest', 'country')

##-------------
## tourist data
##-------------

## merge daysOut and daysIn to allow calculation of days net per country and year
tourismDays.10 <- merge(daysOut.8, daysIn.9, by=c("country", "year"), all.x = TRUE)
tourismDays.10[, daysNet := daysIn - daysOut]

## Merge tourismDays.10 with the food consumption data
setkey(tourismDays.10, "country", "year")
setkey(foodConsumptionPopData.3, "country", "year")

foodTouristConsumption.11 <- merge(foodConsumptionPopData.3, tourismDays.10, by = c("country", "year"), 
                                   all.x = T)

foodTouristConsumption.11[, calOutCountry := daysOut * calPerPersonPerDay]
foodTouristConsumption.11[, calInCountry := daysIn * calPerPersonPerDay]
foodTouristConsumption.11[, calNetCountry := daysNet * calPerPersonPerDay]

## Get rid of some of the columns that we don't need anymore:
foodTouristConsumption.11[, c("totalFood", "pop", "valueCal", "calPerPersonPerDay", "daysOut", 
                              "daysIn", "daysNet", "calOutCountry", "calInCountry") := NULL]

## Net calories
foodTouristConsumption.11[, tourismElement:= "100"]
foodTouristConsumption.11[, flagObservationStatus:= "I"]
foodTouristConsumption.11[, flagMethod:= "e"]

# Save the data

setnames(foodTouristConsumption.11,
         old = c("country", "year", "item", "calNetCountry"),
         new = c("geographicAreaM49", "timePointYears", "measuredItemCPC", "Value"))
setcolorder(foodTouristConsumption.11,
            c("timePointYears", "geographicAreaM49", "measuredItemCPC",
              "tourismElement", "Value", "flagObservationStatus", "flagMethod"))

stats = SaveData(domain = "tourism", dataset = "tourismprod", data = foodTouristConsumption.11)

paste0(stats$inserted, " observations written, ",
       stats$ignored, " weren't updated, ",
       stats$discarded, " had problems.")


# save(foodTouristConsumption.11, file = paste0(R_SWS_SHARE_PATH, "/caetano/tourist_",
#                                                        gsub(" |:|-", "_", Sys.time()), ".RData"))

