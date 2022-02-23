library(ggplot2)
# library(plyr)
# library(rattle)
# library(rmr2)
# library(rhdfs)
# library(tm)
# library(snowball)
# library(wordcloud)
# library(randomForest)

setwd("D:/VMware/mac/Fichiers-data")
getwd()

init <- function() {
  # Districts
  districts <<- read.csv2("districts_info.csv", sep = ",")
  names(districts) <- c(
    "district_id",
    "state",
    "locale",
    "pct_black/hispanic",
    "pct_free/reduced",
    "county_connections_ratio",
    "pp_total_raw"
  )
  # Products
  products <<- read.csv("products_info.csv", sep = ",")
  names(products) <- c(
    "LP ID",
    "URL",
    "Product Name",
    "Provider/Company Name",
    "Sector(s)",
    "Primary Essential Function"
  )
  if (exists("products") && is.data.frame(get("products")) &&
      exists("districts") && is.data.frame(get("districts"))) {
    message("Function Ran Successfully")
  } else {
    stop("Initialisation problem")
  }
}

init()
str(districts)
library(skimr)
skim(districts)



