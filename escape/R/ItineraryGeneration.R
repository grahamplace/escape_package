# Escape Project

# Itinerary Generation Algorithm

#* @post /itinerary
#' @param long : numeric of the user's current longitude
#' @param lat : numeric of the user's current latitude
#' @param timespan : character, indicates which part of the day needs planning (morning, afternoon, evening or fullday)
#' @param subtypes : a character vector of subtypes
#' @return a data frame that has the user's itinerary
#' @import RSQLite
#' @import lubridate
#' @import geosphere
#' @import dplyr
#' @import plumber
generator <- function(long, lat, timespan = "fullday",
                      subtypes = c("nightlife", "entertain", "museums", "outdoor", "sightsee", "shopping", "relax", "tour")) {
  # Setup phase

  # Call libraries
  library(RSQLite)
  library(lubridate)
  library(geosphere)
  library(dplyr)

  # Connect to server
  con <- dbConnect(SQLite(), "escapeDB.sqlite")

  # Read in food and activity tables

  food <- dbReadTable(con, "food")
  foodNames <- dbReadTable(con, "food_col_names")
  colnames(food) <- foodNames
  activity <- dbReadTable(con, "activity")
  activityNames <- dbReadTable(con, "activity_col_names")
  colnames(activity) <- activityNames

  # DEBUG
  set.seed(3)
  activity$eventlength <- sample(c(1, 1.5, 2, 2, 2.5, 2.5, 3, 4), nrow(activity), replace = T)
  activity$description[which(activity$description == "There is no description available.")] <- ""
  food <- food[-which(is.na(food$latitude)), ]

  # Initialize variables: itinerary data frame, currenttime, endtime

  itinerary <- data.frame(Event = character(),
                          Tag = character(),
                          StartTime = numeric(),
                          EndTime = numeric(),
                          Price = numeric(),
                          Popularity = numeric(),
                          Latitude = numeric(),
                          Longitude = numeric(),
                          OpeningHours = numeric(),
                          ClosingHours = numeric(),
                          EventLength = numeric(),
                          Description = character())

  # Determine the endtime and currenttime (here, corresponds to the starttime) from the timespan argument
  endtime <- ifelse(timespan == "evening" | timespan == "fullday", 24,
                    ifelse(timespan == "morning", 12, 19))

  currenttime <- ifelse(timespan == "morning" | timespan == "fullday", 9,
                        ifelse(timespan == "afternoon", 12, 19))

  # We need to keep track of which subtypes of cuisines/activities have been used, and also to know which food/activity places
  # have been suggested in order to avoid re-suggesting any of the elements again

  cuisinesdone <- character()
  activitiesdone <- character()
  activitiesleft <- setdiff(subtypes, activitiesdone)

  activitynotseen <- activity

  #----------------------------

  # Generation Flow

  # First step of the itinerary generation: Determine the category of the first meal
  prevmeal.subtype <- ifelse(timespan == "morning" | timespan == "fullday", "breakfast",
                             ifelse(timespan == "afternoon", "lunch", "dinner"))

  # Select first meal
  # Add first meal to itinerary, update variables using the function call's output
  prevmeal <- foodselection(food, cuisinesdone, prevmeal.subtype, currenttime, lat, long)

  itinerary    <- rbind(itinerary, prevmeal[[1]])
  currenttime  <- prevmeal[[2]]
  lat          <- prevmeal[[3]]
  long         <- prevmeal[[4]]
  cuisinesdone <- prevmeal[[5]]

  # Also create an "hours since last meal" variable that will be updated after ever function call
  hsincemeal <- 0

  # We base the generation flow on the time elapsed since the user has had a meal, and the duration threshold after which we
  # decide to add a new meal depends on the subtype of the previous meal

  while (prevmeal.subtype == "breakfast" & currenttime < endtime) {
    # as long as hsincemeal is less than 3, keep generating activities; else, switch to lunch
    if (hsincemeal < 3) {
      newactivity <- list()

      if(length(activitiesleft) > 0){
        newactivity <- sample(activitiesleft, 1)
        activitiesdone <- c(activitiesdone, newactivity)
        newactivity <- activityselection(activitynotseen, newactivity, currenttime, lat, long)

      } else {
        newactivity <- activityselection(activitynotseen, sample(subtypes, 1), currenttime, lat, long)
      }

      itinerary       <- rbind(itinerary, newactivity[[1]])
      currenttime     <- newactivity[[2]]
      lat             <- newactivity[[3]]
      long            <- newactivity[[4]]
      activitynotseen <- newactivity[[5]]
      eventlength     <- newactivity[[6]]
      activitiesleft  <- setdiff(subtypes, activitiesdone)

      # increment time since last meal
      hsincemeal <- hsincemeal + eventlength
    } else {
      prevmeal.subtype <- "lunch"
      prevmeal <- foodselection(food, cuisinesdone, prevmeal.subtype, currenttime, lat, long)

      itinerary    <- rbind(itinerary, prevmeal[[1]])
      currenttime  <- prevmeal[[2]]
      lat          <- prevmeal[[3]]
      long         <- prevmeal[[4]]
      cuisinesdone <- prevmeal[[5]]
    }
  }

  while (prevmeal.subtype == "lunch" & currenttime < endtime) {
    # as long as hsincemeal is less than 6, keep generating activities; else, switch to dinner
    if (hsincemeal < 6) {
      newactivity <- list()

      if(length(activitiesleft) > 0){
        newactivity <- sample(activitiesleft, 1)
        activitiesdone <- c(activitiesdone, newactivity)
        newactivity <- activityselection(activitynotseen, newactivity, currenttime, lat, long)
      } else {
        newactivity <- activityselection(activitynotseen, sample(subtypes, 1), currenttime, lat, long)
      }

      itinerary       <- rbind(itinerary, newactivity[[1]])
      currenttime     <- newactivity[[2]]
      lat             <- newactivity[[3]]
      long            <- newactivity[[4]]
      activitynotseen <- newactivity[[5]]
      eventlength     <- newactivity[[6]]
      activitiesleft  <- setdiff(subtypes, activitiesdone)

      # increment time since last meal
      hsincemeal <- hsincemeal + eventlength
    } else {
      prevmeal.subtype <- "dinner"
      prevmeal <- foodselection(food, cuisinesdone, prevmeal.subtype, currenttime, lat, long)

      itinerary    <- rbind(itinerary, prevmeal[[1]])
      currenttime  <- prevmeal[[2]]
      lat          <- prevmeal[[3]]
      long         <- prevmeal[[4]]
      cuisinesdone <- prevmeal[[5]]
    }
  }

  while (prevmeal.subtype == "dinner" & currenttime < endtime) {
    # Here we only need to generate suggestions for as long as necessary, with endtime being 24
    # Note: we must include nightlife as a subtype, otherwise there is a risk that no event may fit the spot

    newactivity <- list()

    if(length(activitiesleft) > 0){
      newactivity <- sample(activitiesleft, 1)
      activitiesdone <- c(activitiesdone, newactivity)
      newactivity <- activityselection(activitynotseen, c(newactivity, "nightlife"), currenttime, lat, long)
    } else {
      newactivity <- activityselection(activitynotseen, c(sample(subtypes, 1), "nightlife"), currenttime, lat, long)
    }

    itinerary       <- rbind(itinerary, newactivity[[1]])
    currenttime     <- newactivity[[2]]
    lat             <- newactivity[[3]]
    long            <- newactivity[[4]]
    activitynotseen <- newactivity[[5]]
    eventlength     <- newactivity[[6]]
    activitiesleft  <- setdiff(subtypes, activitiesdone)
  }

  # All done

  dbDisconnect(con)
  return(itinerary)
}

#---------------------------------------------------

# Helper functions

#' @param table : table that contains all of the events (food or activity)
#' @param : eventid : id of the event at the center of the circle within which we are looking
#' @param r : radius of the circle within which events are accepted
#' @return eventids of the values within r of the eventid arg
#' @description function that uses the distance matrix to return a vector of eventids which are within a radius r of the event argument's location
withinrange <- function(table, eventid, r = 5000) {
  # Read in the distance matrix
  distmatrix <- dbReadTable(con, "distance_matrix")
  ids <- as.integer(distmatrix[1,])
  distmatrix <- distmatrix[-1,]
  colnames(distmatrix) <- ids
  rownames(distmatrix) <- ids
  # lookup the entire row corresponding to the event's id, return values in corresponding table within range
  values <- distmatrix[eventid, ]
  validindex <- setdiff(which(values < r), eventid)
  validindex <- which(validindex %in% table$eventid)

  return(values[validindex])
}

#' @param lat : latitude of the event we are trying ot detect
#' @param long : longitude of the event we are trying to detect
#' @return the eventid of the event corresponding to the given lat and long
detect.event <- function(table, lat, long) {

  ifelse(length(table$eventid[which(table$latitude == lat & table$longitude == long)]) != 0,
         (table$eventid[which(table$latitude == lat & table$longitude == long)])[1], NA)
}

#' @return longitude range as a function of given coordinates
longitude_range <- function(coord) {
  p1 <- destPoint(p=coord, b=0, d = 16000)[2]
  long_range <- p1 - coord[2]
  return(long_range)
}

# Variable of latitude range
latitude_range <- 0.01449275362


#' @param cuisinesdone : cuisines that have already been included in the itinerary
#' @param subtypename : type of meal
#' @param starttime : the time at which the event starts
#' @param lat : latitude of the user at the beginning; only used for the first event
#' @param long : longitude of the user at the beginning; only used for the first event
#' @returns a list containing the event as it would appear in the row of a dataframe along with the endtime, lat, long and updated cuisinesdone and foodnotseen
#' @description : Function to select a food location
#'
foodselection <- function(food, cuisinesdone, mealtype, currenttime, lat, long) {
  day <- as.POSIXlt(Sys.Date())$wday

  # detect the event corresponding to the given latitude and longitude, could be food or activity
  # the first time (from the user's location), the function returns NA, which is handled accordingly
  currenteventid <- detect.event(food, lat, long)

  if (is.na(currenteventid)) {
    events <- food %>% filter(subtype == mealtype &
                                latitude < (lat + latitude_range) &
                                latitude > (lat - latitude_range) &
                                longitude < (long + longitude_range(c(lat, long))) &
                                longitude > (long - longitude_range(c(lat, long))) &
                                !(cuisine %in% cuisinesdone) &
                                (if(day == 0) {ifelse((is.na(openSun) | is.na(closeSun)), TRUE, (currenttime >= openSun & (currenttime + eventlength)) <= closeSun)}
                                 else if(day == 6) {ifelse((is.na(openSat) | is.na(closeSat)), TRUE, (currenttime>= openSat & (currentttime + eventlength)) <= closeSat)}
                                 else {ifelse((is.na(openWeek) | is.na(closeWeek)), TRUE, currenttime >= openWeek & (currenttime + eventlength) <= closeWeek)}))
  } else {
    events <- food %>% filter(subtype == mealtype &
                                eventid %in% withinrange(food, currenteventid) &
                                !(cuisine %in% cuisinesdone) &
                                (if(day == 0) {ifelse((is.na(openSun) | is.na(closeSun)), TRUE, (currenttime >= openSun & (currenttime + eventlength)) <= closeSun)}
                                 else if(day == 6) {ifelse((is.na(openSat) | is.na(closeSat)), TRUE, (currenttime>= openSat & (currentttime + eventlength)) <= closeSat)}
                                 else {ifelse((is.na(openWeek) | is.na(closeWeek)), TRUE, currenttime >= openWeek & (currenttime + eventlength) <= closeWeek)}))
  }

  # TODO: this is where the user personalisation code is called
  if (nrow(events) > 0) {
    current <- events[sample(nrow(events), 1), ]
  } else {
    samplerange <- which(food$subtype == mealtype)
    current <- food[sample(samplerange, 1), ]
  }

  cuisinesdone <- c(cuisinesdone, current$cuisine)

  endtime <- currenttime + current$eventlength
  open  <- ifelse(day == 0, current$openSun, ifelse(day == 6, current$openSat, current$openWeek))
  close <- ifelse(day == 0, current$closeSun, ifelse(day == 6, current$closeSat, current$closeWeek))

  return(
    list((data.frame(Events = current$eventname,
                     Tag = current$subtype,
                     StartTime = currenttime,
                     EndTime = endtime,
                     Price = current$price,
                     Popularity = current$popularity,
                     Latitude = current$latitude,
                     Longitude = current$longitude,
                     OpeningHours = open,
                     ClosingHours = close,
                     EventLength = current$eventlength,
                     Description = current$cuisine)),
         endtime,
         current$latitude,
         current$longitude,
         cuisinesdone
    )
  )
}


#' @param subtypename : type of activity
#' @param starttime : the time at which the event starts
#' @param lat : latitude of the user at the beginning; only used for the first event
#' @param long : longitude of the user at the beginning; only used for the first event
#' @returns a list containing the event as it would appear in the row of a dataframe along with the endtime, lat, long and an updated activitynotseen
#' @description : Function to select an activity
activityselection <- function(activitynotseen, subtypename, currenttime, lat, long) {
  day <- as.POSIXlt(Sys.Date())$wday

  currenteventid <- detect.event(activitynotseen, lat, long)

  if (is.na(currenteventid)) {
    events <- activitynotseen %>% filter(subtype %in% subtypename &
                                           latitude < (lat + latitude_range) &
                                           latitude > (lat - latitude_range) &
                                           longitude < (long + longitude_range(c(lat, long))) &
                                           longitude > (long - longitude_range(c(lat, long))) &
                                           (if(day == 0) {ifelse((is.na(openSun) | is.na(closeSun)), TRUE, (currenttime >= openSun & (currenttime + eventlength)) <= closeSun)}
                                            else if(day == 6) {ifelse((is.na(openSat) | is.na(closeSat)), TRUE, (currenttime>= openSat & (currentttime + eventlength)) <= closeSat)}
                                            else {ifelse((is.na(openWeek) | is.na(closeWeek)), TRUE, currenttime >= openWeek & (currenttime + eventlength) <= closeWeek)}))
  } else {
    events <- activitynotseen %>% filter(subtype %in% subtypename &
                                           eventid %in% withinrange(activitynotseen, currenteventid) &
                                           (if(day == 0) {ifelse((is.na(openSun) | is.na(closeSun)), TRUE, (currenttime >= openSun & (currenttime + eventlength)) <= closeSun)}
                                            else if(day == 6) {ifelse((is.na(openSat) | is.na(closeSat)), TRUE, (currenttime>= openSat & (currentttime + eventlength)) <= closeSat)}
                                            else {ifelse((is.na(openWeek) | is.na(closeWeek)), TRUE, currenttime >= openWeek & (currenttime + eventlength) <= closeWeek)}))
  }
  # TODO: this is where the user personalisation code is called
  if (nrow(events) > 0) {
    current <- events[sample(nrow(events), 1), ]
  } else {
    current <- activitynotseen[sample(nrow(activitynotseen), 1), ]
  }

  activitynotseen <- activitynotseen[-which(activitynotseen$eventid == current$eventid), ]

  endtime <- currenttime + current$eventlength
  open  <- ifelse(day == 0, current$openSun, ifelse(day == 6, current$openSat, current$openWeek))
  close <- ifelse(day == 0, current$closeSun, ifelse(day == 6, current$closeSat, current$closeWeek))

  return(
    list((data.frame(Events = current$eventname,
                     Tag = current$subtype,
                     StartTime = currenttime,
                     EndTime = endtime,
                     Price = current$price,
                     Popularity = current$popularity,
                     Latitude = current$latitude,
                     Longitude = current$longitude,
                     OpeningHours = open,
                     ClosingHours = close,
                     EventLength = current$eventlength,
                     Description = current$description)),
         endtime,
         current$latitude,
         current$longitude,
         activitynotseen,
         current$eventlength)
  )
}
