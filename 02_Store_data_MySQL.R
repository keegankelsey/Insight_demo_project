#-------------------------------------------------------------------------#
#-----------> Build MySQL database with clean file as a table <-----------#
#-------------------------------------------------------------------------#

library(RMySQL) # MySQL access for I/O


#-----------> 1. Read in clean file <-----------#

df <- read.table("~/Desktop/Insight/PVLL/PVLL_data_20150118/SMSMessage_Graphed_Anonymous_Processed_cleaned.csv",header=TRUE)


#-----------> 2. Query MySQL databases for databases  <-----------#

drv <- dbDriver("MySQL")
con <- dbConnect(drv, user="root", pass="")
rs <- dbSendQuery(con, statement = "SHOW DATABASES;")
fetch(rs)


#-----------> 3. Make a new database <-----------#

dbSendQuery(con, "CREATE DATABASE pvll;") # new database nameed 'pvll' was created


#-----------> 4. Make a new table within the database <-----------#

# reconnect to the new database #

con <- dbConnect(drv, user="root", pass="",dbname="pvll")

# Return tables in our database #
dbListTables(con)

# Write data frame into MySQL table #
dbWriteTable(con, "pvll_full_cleaned", df)


#-----------> 5. Disconnect <-----------#

dbDisconnect(con)


#-----------------------------#
#-----------> END <-----------#
#-----------------------------#