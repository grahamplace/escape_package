
library(RSQLite)
activity <- read.csv("activity.csv")

head(activity)
actNames <- colnames(activity)
actNames <- actNames[-1]

names <- data.frame(names = actNames)


db <- dbConnect(SQLite(), "escapeDB.sqlite")
dbIsValid(db)
dbListTables(db)
dbWriteTable(db, "activity_col_names", names, overwrite = TRUE)

food <- read.csv("food_dubai.csv")
food <- food[-1]
foodNames <- colnames(food)
foodNames <- data.frame(names = foodNames)
dbWriteTable(db, "food_col_names", foodNames, overwrite = TRUE)
check <- dbReadTable(db, "food_col_names")
dbDisconnect(db)
