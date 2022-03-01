library(ggplot2)
library(skimr)
library(readr)
library(DataExplorer)

init <- function() {
  # set direcory
  if (substr(getwd(),3,14) != "/4BIGF/data") {
    setwd(paste(getwd(),"data", sep = "/"))
  }
  # Districts
  districts <<- read_csv("districts_info.csv")

  # Products
  products <<- read_csv("products_info.csv")

  if (exists("products") && is.data.frame(get("products")) &&
      exists("districts") && is.data.frame(get("districts"))) {
    message("Function Ran Successfully")
  } else {
    stop("Initialisation problem")
  }
}

main <- function(){
  # initialisation
  init()
  # column analyse
  spec(districts)
  spec(products)
  
  # view data
  View(districts)
  View(products)
  
  # column content analyse
  skim(districts)
  skim(products)
  
  # analyse on missing values
  plot_missing(districts, ggtheme = theme_minimal(base_size = 20))
  plot_missing(products, ggtheme = theme_minimal(base_size = 20))
  
  #Districts
  options(repr.plot.height = 6, repr.plot.width = 10)
  
  plot_bar(districts, ggtheme = theme_minimal(base_size = 10))
  
  #Products
  options(repr.plot.height = 6, repr.plot.width = 10)
  
  plot_bar(products, ggtheme = theme_minimal(base_size = 10))
}

main()




