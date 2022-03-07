init <- function() {
  library(ggplot2)
  library(skimr)
  library(readr)
  library(DataExplorer)
  library(plyr)
  library(dplyr)
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
    message("Function Run Successfully")
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
  
  #engagement
  #list_engagement <- list.files(path = paste(getwd(),"engagement_data", sep = "/"),
  #                             recursive = TRUE,
  #                              pattern = "\\.csv$",
  #                              full.names = TRUE)
  #all_engagement <- read_csv(list_engagement)
  #ggplot(data = all_engagement, aes(x = time, y = pct_access )) + geom_bar(stat = "identity")
  
  new_category <- substr(products$`Primary Essential Function`, 1, 3)
  new_category[new_category == "LC/"] <- "LC/CM/SDO"
  products$category <- new_category
  ggplot(data = products, aes(x = "", y = category, fill = category)) + geom_bar(stat = "identity") + coord_polar("y") +theme_void()
  
}

main()




id_list <- districts$district_id
# for each in id_list
for (i in id_list){
  file <- c(i, ".csv$")
  list_id_file <- list.files(path = paste(getwd(),"engagement_data", sep = "/"),
                             recursive = TRUE,
                             pattern = file,
                             full.names = TRUE)
  new_tabl <- read_csv(list_id_file)
  new_tabl$district_id <- i
  if (exists("tabl") && is.data.frame(get("tabl"))) {
    tabl <- rbind(tabl, new_tabl)
  } else {
    tabl <- new_tabl
  }
}

View(tabl)
print(tabl$district_id)
bl <- tabl[tabl$district_id == 8815,]
bl
tabl$pct_access <- tabl$pct_access*10

ggplot(data = bl, aes(x = time, y = mean(pct_access/10))) + geom_bar(stat = "identity")
