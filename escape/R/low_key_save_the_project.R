activity <- read.csv("activity.csv")

head(activity)
actNames <- colnames(activity)
actNames <- actNames[-1]

names <- data.frame(names = actNames)


db <- dbConnect(SQLite(), "escapeDB.sqlite")
dbIsValid(db)
dbListTables(db)
dbWriteTable(db, "activity_col_names", names)

food <- read.csv("food.csv")
foodNames <- colnames(food)
dbWriteTable(db, "food_col_names", names)






