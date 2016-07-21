
#' Function to scrape web for activities in a given city, returns 'activity' dataframe
#'
#' @param city A string representing the city for which you want activities.
#' @return A dataframe with all of the activies for the input city.
scrape_activity <- function(city) {
  suppressPackageStartupMessages(library(rvest))
  suppressPackageStartupMessages(library(RCurl))
  suppressPackageStartupMessages(library(dplyr))
  suppressPackageStartupMessages(library(XML))
  suppressPackageStartupMessages(library(stringr))
  suppressPackageStartupMessages(library(outliers))
  suppressPackageStartupMessages(library(RJSONIO))
  suppressPackageStartupMessages(library(plyr))
  suppressPackageStartupMessages(library(ggmap))

  #open browser
  checkForServer()
  startServer()
  browser <- remoteDriver()
  browser$open()
  url <- "https://www.tripadvisor.co.za/"
  browser$navigate(url)

  #locate search bars
  city_input <- browser$findElement(using = 'css selector', '#GEO_SCOPED_SEARCH_INPUT')
  city_input$sendKeysToElement(list(city))
  button <- browser$findElement(using = 'xpath', '//*[@id="SEARCH_BUTTON"]')
  button$clickElement()
  button$clickElement()


  #generate url template
  page <- read_html(browser$getCurrentUrl()[[1]])
  url_ending <- page %>% html_nodes("li.attractions.twoLines > a") %>% html_attr("href")
  url_thingstodo <- paste("www.tripadvisor.co.za", url_ending, sep = "")

  browser$close()

  #activity id numbers
  nightlife_id <- 20
  entertain_id <- 58
  museum_id <- 49
  outdoor_id <- 61
  sightsee_id <- 47
  shopping_id <- 26
  relax_id <- 40
  tour_id <- 42

  #generate urls for each activity
  split.url <- str_split(url_thingstodo, "Activities")[[1]]
  url.part1 <- paste("https://",split.url[1], "Activities-c", sep="")
  url.part2 <- split.url[2]

  night_url <- paste(url.part1, nightlife_id, url.part2, sep = "")
  ent_url <-paste(url.part1, entertain_id, url.part2, sep = "")
  museum_url <-paste(url.part1, museum_id, url.part2, sep = "")
  outdoor_url <- paste(url.part1, outdoor_id, url.part2, sep = "")
  sightsee_url <-paste(url.part1, sightsee_id, url.part2, sep = "")
  shop_url <- paste(url.part1, shopping_id, url.part2, sep = "")
  relax_url <- paste(url.part1, relax_id, url.part2, sep = "")
  tour_url <- paste(url.part1, tour_id, url.part2, sep = "")

  #get data for each subtype
  night <- get_data(night_url)
  ent <- get_data(ent_url)
  museum <- get_data(museum_url)
  outdoor <- get_data(outdoor_url)
  sightsee <- get_data(sightsee_url)
  shop <-get_data(shop_url)
  relax <-get_data(relax_url)
  tour <- get_data(tour_url)

  #insert subtype column
  night$subtype <- "nightlife"
  ent$subtype <- "entertainment"
  museum$subtype <- "museum"
  outdoor$subtype <- "outdoor"
  sightsee$subtype <- "sightseeing"
  shop$subtype <- "shopping"
  relax$subtype <- "relaxing"
  tour$subtype <- "tour"

  #create full table
  activity <- rbind(night,ent,museum,outdoor, sightsee,shop,relax,tour)

  activity$eventid <- seq(from =2, to = nrow(activity) * 2, by = 2)

  file_name <- paste(city, "_Activities.csv", sep = "")
  write.csv(activity, file = file_name)

  return(activity)

}


get_data <- function(url) {
  #read page
  input <- read_html(url)

  #get activity names
  activity.names <- input %>% html_nodes("div.property_title > a") %>% html_text()

  #get number of reviews
  num.reviews <- input %>% html_nodes("span.more > a") %>% html_text()
  num.reviews <- gsub("\n", "", num.reviews)
  num.reviews <- gsub("Be the first to review this attraction", "0", num.reviews)
  num.reviews <- gsub("[[:space:]]","", num.reviews)
  num.reviews <- gsub("reviews","", num.reviews)
  num.reviews <- gsub("review","", num.reviews)
  num.reviews <- as.integer(str_trim(num.reviews))

  #fill in elements with 0 reviews
  num_no.reviews <- length(activity.names) - length(num.reviews)
  if (num_no.reviews!=0 ) {
    for (i in 1:num_no.reviews) {
      num.reviews[length(num.reviews) + 1] <- 0
    }}

  #translate number of reviews to popularity rating (min = 1, max = 3)
  pop.quant <- quantile(num.reviews, probs = seq(0,1,1/3))
  popularity <- c()
  for (i in 1:length(num.reviews)) {
    if (num.reviews[i]<pop.quant[2])
      popularity[i] <- 1
    else if (num.reviews[i] <pop.quant[3])
      popularity[i] <- 2
    else
      popularity[i] <- 3
  }

  #get addresses
  activity.addresses <- c()
  act.urls <- input %>% html_nodes("div.property_title > a") %>% html_attr("href")
  act.urls <- sapply(act.urls, function(x) { paste("https://www.tripadvisor.co.za",x, sep = "")})
  act.addresses <- sapply(act.urls, function(x) {read_html(x[1]) %>% html_nodes("address > span > span.format_address") %>% html_text()})
  act.addresses <- gsub("Address:", "", act.addresses)
  act.addresses <- str_trim(gsub("\\| ", "", act.addresses))

  #get descriptions
  description <- sapply(activity.names, get_description)

  #get geo location
  geo_location <- sapply(act.addresses, geocode)
  odds <- seq(from =1, to = length(activity.names) *2, by = 2)
  evens <- seq(from =2, to = length(activity.names) *2, by = 2)

  latitude <- c()
  longitude <- c()

  for (i in 1:length(activity.names)) {
    longitude <- c(longitude, geo_location[odds[i]][[1]])
    latitude <- c(latitude, geo_location[evens[i]][[1]])
  }

  #create dataframe
  eventid <- rep(NA, length(activity.names))
  eventname <- activity.names
  subtype <- rep(NA, length(activity.names))
  price <- rep(NA, length(activity.names))
  openWeek <- rep(NA, length(activity.names))
  closeWeek <- rep(NA, length(activity.names))
  openSat <- rep(NA, length(activity.names))
  closeSat <- rep(NA, length(activity.names))
  openSun <- rep(NA, length(activity.names))
  closeSun <- rep(NA, length(activity.names))
  eventlength <- rep(NA, length(activity.names))

  activity.table <- data.frame(eventid, eventname, subtype, description, price, openWeek, closeWeek, openSat, closeSat, openSun, closeSun, eventlength, popularity, latitude, longitude, act.addresses)

}




get_description <- function(event) {

  #transform search query to text+text format (replace spaces with +)
  event <- gsub(" ","+", event )

  #generate url
  #search_url <- sprintf("https://www.google.co.za/search?q=%s&oq=%s&aqs=chrome", event, event)
  #google_search <- read_html(search_url)get_

  apikey <- "secret"

  url <- paste0("https://kgsearch.googleapis.com/v1/entities:search?query=",event,"&key=", apikey, "&limit=1&indent=True")

  #error check URL
  if (verify_URL(url)) {
    json <-  fromJSON(url)
    #description <- json$itemListElement$result$detailedDescription$articleBody
    description <- json$itemListElement[[1]]$result$detailedDescription[1]

    if(is.null(description)) {
      description <- "There is no description available."

    }
  }
  else {
    description <- "There is no description available."
  }

  #trim whitespace
  description <- str_trim(description)

  return(description)
}

#' @param url URL for a search query of Google's Knowledge graph API.
#' @return Returns TRUE if the url was valid, FALSE if there doesn't exist a knowledge graph for that query.
verify_URL <- function(url) {
  tryCatch({
    fromJSON(url)
    description <- json$itemListElement[[1]]$result$detailedDescription[1]
    return(TRUE)
  }, error = function(e) {
    #message(paste("Warning: There is no description available."))
    return(FALSE)
  }, finally = {}
  )
}















