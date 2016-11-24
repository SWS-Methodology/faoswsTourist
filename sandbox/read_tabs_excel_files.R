library(xlsx)
library(stringr)
library(data.table)

?getSheets

getwd()
setwd("T:/Team_working_folder/A/FBS-Modules/Tourism/tourist_consumption/CDROM 1995-2014/CDROM 1995-2014")

files = dir(full.names = F)

data <- list()

for (i in 1:length(files)) {
  data[[i]] = data.frame(country =substr(files[i], 22, nchar(files[i])),
                         tab = names(getSheets(loadWorkbook(files[i]))))
}

data <- data.table(do.call(rbind.data.frame, data))
data[, country := gsub('.{5}$', '', country)]
data = data[tab %in% c("111", "112", "121", "122")]
countryTab <- data[, .N, country]

countryTab <- countryTab[order(-countryTab$N)]
head(countryTab, 15)

