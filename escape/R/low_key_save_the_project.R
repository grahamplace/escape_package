activity <- read.csv("activity.csv")

head(activity)
actNames <- colnames(activity)
actNames <- actNames[-1]

names <- data.frame(names = actNames)


db <- dbConnect(SQLite(), "escapeDB.sqlite")
dbIsValid(db)
dbListTables(db)
dbWriteTable(db, "activity_col_names", names)


foodNames <- food
