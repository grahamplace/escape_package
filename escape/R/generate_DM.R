#' Function to create distance matrix between pairs of every escape location in city
#'
#' @param activity Data frame returned from activity scraping function
#' @param food Data frame returned from food scraping function
#'
generate_DM <- function(activity, food){

  #create unified list of ids in database
  ids <- c(activity$eventid, food$eventid)

  #create empty dataframe with correct number of rows
  dm <- data.frame(id = ids)

  #rownames need to be ids
  row.names(dm) <- ids

  #delete initial column created for size setting
  dm$id <- NULL

  #nested for loop to hit all pairs of ids
  for(a in 1:length(ids)){

    #temp vector of length of # of ids
    temp <- 1:length(ids)

    #set id1 for call to dist
    id1 <- ids[a]

    #inner for loop iterates across all ids for current a id
    for(b in 1:length(ids)){
      #assign id2 for call to dist
      id2 <- ids[b]

      #fill temp vector with distances between current id pair
      temp[b] = dist(id1, id2)
    }

    #cbind into final distance matrix
    dm <- cbind(dm, temp)
  }

  #finally, name columns with ids for lookup
  colnames(dm) <- ids
  dm <- rbind(ids, dm)

  #return final dataframe
  return (dm)
}



#' Function to calculate distance between two locations, given their unique event ids
#'
#' @param id1 event id for first event in pair
#' @param id2 event id for second event in pair
dist <- function(id1, id2) {

  #all activity ids are even
  if(id1 %% 2 == 0){
    lat1 <- activity$latitude[which(activity$eventid == id1)]
    long1 <- activity$longitude[which(activity$eventid == id1)]
  }

  #all food ids are odd
  else {
    lat1 <- food$latitude[which(food$eventid == id1)]
    long1 <- food$longitude[which(food$eventid == id1)]
  }

  if(id2 %% 2 == 0){
    lat2 <- activity$latitude[which(activity$eventid == id2)]
    long2 <- activity$longitude[which(activity$eventid == id2)]
  }

  else {
    lat2 <- food$latitude[which(food$eventid == id2)]
    long2 <- food$longitude[which(food$eventid == id2)]
  }

  return(acos(sin(lat1) * sin(lat2) + cos(lat1) * cos(lat2) * cos(long1 - long2)) * 6371)
}

