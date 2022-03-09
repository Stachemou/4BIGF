init <- function() {
  library(ggplot2)
  library(skimr)
  library(readr)
  library(plyr)
  library(dplyr)

  # set direcory
  if (substr(getwd(), 3, 14) != "/4BIGF/data") {
    setwd(paste(getwd(), "data", sep = "/"))
  }
  # Districts
  districts <<- read_csv("districts_info.csv")

  # Products
  products <<- read_csv("products_info.csv")

  # tabl
  id_list <- districts$district_id
  for (i in id_list) {
    file <- c(i, ".csv$")
    list_id_file <- list.files(path = paste(getwd(), "engagement_data",
                                            sep = "/"),
                               recursive = TRUE,
                               pattern = file,
                               full.names = TRUE)
    new_tabl <- read_csv(list_id_file)
    new_tabl$district_id <- i
    if (exists("tabl") && is.data.frame(get("tabl"))) {
      tabl <<- rbind(tabl, new_tabl)
    } else {
      tabl <<- new_tabl
    }
  }
  id_list <- products$`LP ID`
  product_name_list <- products$`Product Name`
  for (i in id_list) {
    tabl$productName[tabl$lp_id == i] <<- product_name_list[match(i, id_list)]
  }

  #complete_base
  complete_base <<- merge(x = tabl, y = districts,
                          by = "district_id", all.x = TRUE)
  complete_base <<- merge(x = complete_base, y = products,
                          by.x = "lp_id", by.y = "LP ID", all.x = TRUE)
  # check everything is loaded
  if (exists("products") && is.data.frame(get("products")) &&
      exists("districts") && is.data.frame(get("districts")) &&
      exists("tabl") && is.data.frame(get("tabl")) &&
      exists("complete_base") && is.data.frame(get("complete_base"))) {
    message("Function Run Successfully")
  } else {
    stop("Initialisation problem")
  }
}

main <- function() {
  # initialisation
  init()
  
  # view data
  View(districts)
  View(products)
  
  # column analyse
  spec(districts)
  spec(products)
  
  # column content analyse
  skim(districts)
  skim(products)

  # analyse on missing values
  plot_missing(districts, ggtheme = theme_minimal(base_size = 20))
  plot_missing(products, ggtheme = theme_minimal(base_size = 20))
  
  # chargement de pages par Etats
  graph <- data.frame(complete_base$pct_access, complete_base$state)
  ggplot(data = graph, aes(x = complete_base.pct_access,
                           y = complete_base.state)) +
    geom_bar(stat = "summary", fill = "#1E90FF")
  
  # Nombre de district scolaire par Etat
  ggplot(data = districts, aes(y = state)) +
    geom_bar(stat = "count", fill = "#1E90FF") +
    geom_text(aes(label = ..count..), stat = "count", hjust = -0.5)
  
  # Compte de county connections ratio
  ggplot(data = districts, aes(y = county_connections_ratio,
                               fill = county_connections_ratio)) +
    geom_bar(stat = "count")
  
  # Engagement des étudiants en 2020
  ggplot(data = tabl, aes(x = time, y = mean(engagement_index, na.rm = TRUE))) +
    geom_bar(stat = "identity")
  
  
  # Répartition des fonctionnalités primaires des 372 produits
  new_category <- substr(products$`Primary Essential Function`, 1, 3)
  new_category[new_category == "LC/"] <- "LC/CM/SDO"
  products$category <- new_category
  ggplot(data = products, aes(x = "", y = category, fill = category))
  + geom_bar(stat = "identity") + coord_polar("y") + theme_void()
  
  # Pages chargées par secteurs
  graph <- data.frame(complete_base$Sector(s), complete_base$engagement_index)
  ggplot(data = graph, aes(x = complete_base..Sector.s..,
                             y = complete_base.engagement_index))
  + geom_bar(stat = "identity")
  
  # Produits les plus utilisés
  graph <- data.frame(complete_base$`Product Name`,
                      complete_base$engagement_index)
  df <- aggregate(
    graph$complete_base.engagement_index~graph$complete_base..Product.Name.,
    FUN = sum, na.rm = TRUE
  )
  names(df)[1] <- "product_name"
  names(df)[2] <- "engagement_index"
  df %>%
    arrange(desc(engagement_index)) %>%
    slice(1:10) %>%
    ggplot(., aes(x = engagement_index,
                  y = reorder(product_name, engagement_index))) +
    geom_bar(stat = 'identity', fill = "#1E90FF")
  
  # Engagement des étudiants par Etats
  graph <- data.frame(complete_base$state, complete_base$engagement_index)
  ggplot(data = graph, aes(x = complete_base.engagement_index,
                           y = complete_base.state))
  + geom_bar(stat = "identity")
  
  # Engagement des étudiants selon leurs localités
  graph <- data.frame(complete_base$locale, complete_base$engagement_index)
  ggplot(data = graph, aes(x = complete_base.locale,
                           y = complete_base.engagement_index))
  + geom_bar(stat = "identity")
  
  # Pages chargées par les communautés
  graph <- data.frame(complete_base$`pct_black/hispanic`,
                      complete_base$engagement_index)
  ggplot(data = graph, aes(x = complete_base..pct_black.hispanic.,
                         y = complete_base.engagement_index))
  + geom_bar(stat = "identity")
  
  # Communautés par districts scolaires
  ggplot(data = districts, aes(x = `pct_black/hispanic`))
  + geom_bar(stat = "count")
  
  # Nombre de pages chargées et budget
  graph <- data.frame(complete_base$pp_total_raw,
                      complete_base$engagement_index)
  ggplot(data = graph, aes(x = complete_base.engagement_index,
                           y = complete_base.pp_total_raw)) +
    geom_bar(stat = "identity")
  
  # Budgets alloués par rapport au nombre de district
  ggplot(data = districts, aes(y = pp_total_raw)) + geom_bar(stat = "count")
  
  # Pages chargées par les personnes bénéficiaires d'aides
  graph <- data.frame(complete_base$`pct_free/reduced`,
                      complete_base$engagement_index)
  ggplot(data = graph, aes(x = complete_base..pct_free.reduced.,
                         y = complete_base.engagement_index))
  + geom_bar(stat = "identity")
  
  # Nombre de personnes bénéficiant d'aide dans les districs
  ggplot(data = districts, aes(x = `pct_free/reduced`)) +
    geom_bar(stat = "count")
  
}

main()
