#-------------------------------------------------------------------------------------#
#-----------> Clean data for use in model building and feature detection  <-----------#
#-------------------------------------------------------------------------------------#

library(RMySQL) # MySQL for I/O #


#-----------> 1. Pull database from MySQL <-----------#

drv <- dbDriver("MySQL")
con <- dbConnect(drv, user="root", pass="",dbname="pvll")
dbListTables(con)
dbListFields(con, "pvll_full_cleaned")
df <- dbReadTable(conn = con, name = "pvll_full_cleaned")
dbDisconnect(con)


#-----------> 2. Isolate unique non-user with only one user:pair  <-----------#

# Find unique non-users #
non_user_set <- unique(df[,"address"])[unique(df[,"address"]) %in% unique(df[,"username"]) == FALSE]

# Make sure all non-users have only one user pair #
nonuser_user_links <- c()
for(i in 1:length(non_user_set)){
  print(i/length(non_user_set))
  nonuser_user_links[i] <- length(unique(df[df[,"address"]==non_user_set[i],"username"]))
}
table(nonuser_user_links)

# Remove non-users with more than 1 user connection #
nonusers_to_keep <- non_user_set[nonuser_user_links == 1]

df.trim <- df[df[,"address"] %in% nonusers_to_keep,]
rm(df)


#-----------> 3. Remove users with high and low message counts  <-----------#

message_counts_by_user <- tapply(df.trim[,"username"],df.trim[,"username"],length)
plot(density(message_counts_by_user))
user_message_quantile <- quantile(message_counts_by_user,probs = seq(0,1,0.05))

# This takes about 40 minutes and should be modified to improve speed #
cutoff <- c()
cdf <- c()
for(i in 2:(length(user_message_quantile)-1)){
  cutoff[i] <- sum(user_message_quantile[1:i])/sum(user_message_quantile[1:(i+1)])
}
plot(1:(length(user_message_quantile)-1),cutoff,type="l",xlab="quantile",ylab="mass",
     main="Mass of messages compared to top individual")

# Insight: remove top 5% and bottom 5% of performers as top users will introduce bias in message type#
keep_users <- message_counts_by_user[message_counts_by_user >= user_message_quantile["5%"] & message_counts_by_user <= user_message_quantile["95%"]]

df.trim.2 <- df.trim[df.trim[,"username"] %in% names(keep_users),]
rm(df.trim)


#-----------> 4. Remove early messages before company launch <-----------#

old <- 1404172800 #2014-07-01
d <- as.POSIXct(old, origin="1970-01-01", tz="GMT")
date <- as.Date(d)

df.trim.2 <- df.trim.2[as.numeric(df.trim.2[,"smsDate"])/1000 >= old,]

# store data into database #
con <- dbConnect(drv, user="root", pass="",dbname="pvll")
dbListTables(con)
dbWriteTable(con, "pvll_trim_clean", df.trim.2)
dbDisconnect(con)


#-----------------------------#
#-----------> END <-----------#
#-----------------------------#